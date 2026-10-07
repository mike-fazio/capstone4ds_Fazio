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

  df <- input_data %>%
    mutate(
      Result_Measure = as.numeric(Result_Measure)
    ) %>%
    filter(
      # Select warm season
      month(Activity_StartDate, label = TRUE) %in%
        c("May", "Jun", "Jul", "Aug", "Sep", "Oct"),

      # Remove null results
      !is.na(Result_Measure),

      # Remove records without coordinates
      !is.na(Location_Latitude),
      !is.na(Location_Longitude)
    ) %>%
    group_by(Location_Identifier) %>%
    summarise(
      # Station metadata
      location_name = first(Location_Name),
      latitude = first(Location_Latitude),
      longitude = first(Location_Longitude),

      # Summary statistics
      mean = mean(Result_Measure, na.rm = TRUE),
      sd = sd(Result_Measure, na.rm = TRUE),
      n = n(),
      log = log(mean),

      # Date range for QC and reference
      first_date = min(Activity_StartDate, na.rm = TRUE),
      last_date = max(Activity_StartDate, na.rm = TRUE),

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
