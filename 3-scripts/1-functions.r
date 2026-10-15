### CREATE OR LOAD STUDY AREA ----

get_study_area <- function() {
  file_path <- here::here(
    "2-data",
    "processed",
    "study_area.rds"
  )

  if (file.exists(file_path)) {
    study_area <- readRDS(file_path)
  } else {
    wbid <- st_read(
      here::here(
        "2-data",
        "raw",
        "Waterbody_IDs.shp"
      )
    )

    # List of relevant WBIDs
    study_area_wbid_list <- c(
      "548GA",
      "548GB",
      "548H",
      "548C",
      "548D",
      "548B",
      "548AA",
      "548AB",
      "548AC"
    )

    study_area <- wbid %>%
      # Filter results to only include desired WBIDs
      filter(WBID %in% study_area_wbid_list) %>%
      # Dissolve all polygons into one geometry
      summarise()

    saveRDS(study_area, file_path)
  }

  return(study_area)
}


### CREATE OR LOAD SITES ----

get_wq_sites <- function(study_area) {
  out_path <- here::here(
    "2-data",
    "processed",
    "wq_sites.rds"
  )

  # Load data if it exists; otherwise download
  if (file.exists(out_path)) {
    pbs_sites <- readRDS(out_path)
  } else {
    # Query WQP and cache result
    wqp_sites <- whatWQPsites(
      countycode = c("US:12:033", "US:12:113"),
      siteType = "Estuary",
      legacy = FALSE
    )

    # Filter sites to Pensacola Bay System
    pbs_sites <- wqp_sites %>%
      select(
        Org_Identifier,
        Org_FormalName,
        Location_Identifier,
        Location_Longitude,
        Location_Latitude
      ) %>%
      filter(
        !is.na(Location_Longitude),
        !is.na(Location_Latitude)
      ) %>%
      st_as_sf(
        coords = c(
          "Location_Longitude",
          "Location_Latitude"
        ),
        crs = 4326,
        remove = TRUE
      ) %>%
      st_transform(st_crs(study_area)) %>%
      st_filter(
        study_area,
        .predicate = st_within
      )

    # Save to file
    saveRDS(pbs_sites, out_path)
  }

  return(pbs_sites)
}


### RETRIEVE OR LOAD WQ DATA ----

get_wq_data <- function(
  param,
  from_date,
  to_date,
  sites,
  output_name,
  force = FALSE
) {
  # Create file name based on user-provided input
  file_name <- paste0(output_name, ".rds")

  # Build output path
  out_path <- here::here(
    "2-data",
    "raw",
    file_name
  )

  if (file.exists(out_path) && !force) {
    # Load previously downloaded data
    pbs_df <- readRDS(out_path)
  } else {
    # Site list used to filter query results
    site_list <- unique(sites$Location_Identifier)

    # Download data from WQP API
    wq_df <- readWQPdata(
      countycode = c("US:12:033", "US:12:113"),
      siteType = "Estuary",
      characteristicName = param,
      startDateLo = from_date,
      startDateHi = to_date,
      dataProfile = "narrow",
      service = "ResultWQX3",
      ignore_attributes = FALSE
    )

    # Filter results to sites within study area
    pbs_df <- wq_df %>%
      filter(Location_Identifier %in% site_list)

    # Save dataset
    saveRDS(pbs_df, out_path)
  }

  return(pbs_df)
}

### PROCESS WQ DATA ----

proc_wq_data <- function(input_data) {
  # Get name of input dataset
  input_name <- deparse(substitute(input_data))

  # Calculate Quantiles and IQR to remove statistical ouliers

  Q1 <- quantile(input_data$Result_Measure, .25)
  Q3 <- quantile(input_data$Result_Measure, .75)

  # Clean and filter raw observations
  df_clean <- input_data %>%
    mutate(
      Result_Measure = as.numeric(Result_Measure),
      year = year(Activity_StartDate),
      outlier = 
        ifelse( 
        (values >= (quantile(values, .25) - 1.5 * IQR(values)) &
         values <= (quantile(values, .75) + 1.5 * IQR(values))), 
        1,
        0
      ),
      Result_Measure = ifelse(outlier = 1, NA, Result_Measure) #if outlier remove value to avoid statistics
    ) %>%
    filter(
      # Select warm season (May-October)
      month(Activity_StartDate) %in% 5:10,

      # Remove null results
      !is.na(Result_Measure),

      # Remove records without coordinates
      !is.na(Location_Latitude),
      !is.na(Location_Longitude)
    )

  # Calculate annual warm-season means for each site
  annual_summary <- df_clean %>%
    group_by(
      Location_Identifier,
      year
    ) %>%
    summarise(
      annual_mean = mean(Result_Measure, na.rm = TRUE),
      annual_n = n(),
      outlier_n = n(outlier, na.rm = TRUE) #count outliers

      # Station metadata
      location_name = first(Location_Name),
      latitude = first(Location_Latitude),
      longitude = first(Location_Longitude),

      .groups = "drop"
    )

  # Calculate long-term site-level conditions
  df <- annual_summary %>%
    group_by(Location_Identifier) %>%
    summarise(
      # Station metadata
      location_name = first(location_name),
      latitude = first(latitude),
      longitude = first(longitude),

      # Long-term summary statistics
      mean = mean(annual_mean, na.rm = TRUE),
      sd = sd(annual_mean, na.rm = TRUE),
      years_sampled = n(),
      total_n = sum(annual_n),

      first_year = min(year),
      last_year = max(year),

      .groups = "drop"
    ) %>%

    # Make data spatially aware for interpolation
    st_as_sf(
      coords = c("longitude", "latitude"),
      crs = 4326,
      remove = FALSE
    ) %>%

    # NAD 1983 (2011) StatePlane Florida North FIPS 0903 (Meters)
    st_transform("ESRI:103021")

  # Save processed data
  outname <- paste0(
    input_name,
    "_0903.rds"
  )

  out_path <- here::here(
    "2-data",
    "processed",
    outname
  )

  saveRDS(df, out_path)

  return(df)
}

### PARAMETER MEANS MAP ----
param_means_map <- function(x, param_label) {
  tm_basemap("Esri.WorldGrayCanvas") +
    tm_shape(x) +
    tm_dots(
      fill = "mean",
      col = "black",
      size = 0.5,
      lwd = 0.5,
      fill.scale = tm_scale_continuous(
        values = "scico.imola"
      ),
      fill.legend = tm_legend(
        title = param_label,
        title.fontface = 2,
        #position = c("left", "bottom"),
        position = tm_pos_out("center", "bottom", pos.h = "center"),
        orientation = "landscape",
        show = TRUE
      )
    ) +
    #tm_legend(
    #position = c("left", "top"),
    #title = param_label
    #) +
    tm_layout(
      inner.margins = c(0.2, 0.2, 0.2, 0.2)
    ) +
    tm_title("Mean Observed Monitoring Location Values", fontface = 2) +
    tm_compass(type = "8star")
}

### PARAMETER MEANS HISTOGRAM ----

param_means_hist <- function(x, param_label) {
  ggplot(x, aes(x = mean)) +
    geom_histogram(
      bins = 20,
      color = "white",
      linewidth = 0.3
    ) +
    labs(
      title = paste("Distribution of Warm-Season Observations -", param_label),
      subtitle = "Sample Counts for monitoring locations in the Pensacola Bay System",
      x = "Observed Mean",
      y = "Number of Monitoring Locations"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 10),
      strip.text = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
}
