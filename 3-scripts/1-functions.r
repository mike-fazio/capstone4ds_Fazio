# -----------------------------------------------------------------------------
#  DEFINE STUDY AREA FUNCTION -
#  Creates singular study area polygon from chosen WBIDs. WBID data from FDEP.
# -----------------------------------------------------------------------------

get_study_area <- function() {
  file_path <- here::here(
    "2-data",
    "processed",
    "study_area_0903.rds"
  )

  if (file.exists(file_path)) {
    study_area <- readRDS(file_path)
  } else {
    #wbid <- sf::st_read(here::here("2-data","raw","Waterbody_IDs.shp"))
    wbid <- sf::st_read(
      "https://ca.dep.state.fl.us/arcgis/rest/services/OpenData/WBIDS/MapServer/0/query?outFields=*&where=1%3D1&f=geojson"
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
      summarise() %>%
      sf::st_transform(
        # re-project to Florida Albers NAD 2011 State Plane North (EPSG: 0903)
        crs = "+proj=lcc +lat_0=29 +lon_0=-84.5 +lat_1=29.5833333333333 +lat_2=30.75 +x_0=600000 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
      )

    saveRDS(study_area, file_path)
  }

  return(study_area)
}


# -----------------------------------------------------------------------------
#  DEFINE SITE DATA RETRIEVAL FUNCTION -
#  Checks if data already exists. If so, loads data, else retrieves from EPA
#  WQP.
# -----------------------------------------------------------------------------

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
    wqp_sites <- dataRetrieval::whatWQPsites(
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
      sf::st_as_sf(
        coords = c(
          "Location_Longitude",
          "Location_Latitude"
        ),
        crs = 4326,
        remove = TRUE
      ) %>%
      sf::st_transform(sf::st_crs(study_area)) %>%
      sf::st_filter(
        study_area,
        .predicate = st_within
      )

    # Save to file
    saveRDS(pbs_sites, out_path)
  }

  return(pbs_sites)
}


# -----------------------------------------------------------------------------
#  DEFINE WQ DATA RETRIEVAL FUNCTION -
#  Checks if data already exists. If so, loads data, else retrieves from EPA
#  WQP.
# -----------------------------------------------------------------------------

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
    wq_df <- dataRetrieval::readWQPdata(
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

# -----------------------------------------------------------------------------
#  DEFINE OUTLIER DETECTION FUNCTION -
#  Detect, flag, and return extreme outliers for further inspection.
# -----------------------------------------------------------------------------

find_outliers <- function(df) {
  # Ensure result values are numeric
  df <- df %>%
    mutate(
      Result_Measure = as.numeric(Result_Measure)
    ) %>%
    filter(
      # Select warm season (May-October)
      lubridate::month(Activity_StartDate) %in% 5:10
    )

  # Calculate quantiles and IQR
  Q1 <- quantile(
    df$Result_Measure,
    0.25,
    na.rm = TRUE
  )

  Q3 <- quantile(
    df$Result_Measure,
    0.75,
    na.rm = TRUE
  )

  iqr_value <- IQR(
    df$Result_Measure,
    na.rm = TRUE
  )

  # Flag outliers and perform initial filtering
  df_flagged <- df %>%
    mutate(
      outlier = (Result_Measure < (Q1 - 1.5 * iqr_value) |
        Result_Measure > (Q3 + 1.5 * iqr_value))
    ) %>%
    filter(
      outlier
    )

  print(paste("Number of outliers detected: ", sum(df_flagged$outlier)))

  return(df_flagged)
}

# -----------------------------------------------------------------------------
#  DEFINE WATER QUALITY DATA PROCESSING FUNCTION -
#  Processes input data to prepare it for use in this projects analysis.
# -----------------------------------------------------------------------------

proc_wq_data <- function(df, param_name) {
  # Get name of input dataset
  input_name <- deparse(substitute(df))

  df <- df %>%
    filter(
      # Select warm season (May-October)
      lubridate::month(Activity_StartDate) %in% 5:10,
      # Remove records without measured values
      !is.na(Result_Measure),

      # Remove records without coordinates
      !is.na(Location_Latitude),
      !is.na(Location_Longitude)
    ) %>%
    mutate(
      year = lubridate::year(Activity_StartDate)
    )

  # Calculate annual warm-season means for each site
  annual_summary <- df %>%
    group_by(
      Location_Identifier,
      year
    ) %>%
    summarise(
      # Mean excluding statistical outliers
      annual_mean = mean(
        Result_Measure,
        na.rm = TRUE
      ),

      # Number of observations retained
      annual_n = n(),

      # Station metadata
      location_name = first(Location_Name),
      latitude = first(Location_Latitude),
      longitude = first(Location_Longitude),

      .groups = "drop"
    )

  # Calculate long-term site-level conditions
  df_out <- annual_summary %>%
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
    sf::st_as_sf(
      coords = c("longitude", "latitude"),
      crs = 4326,
      remove = FALSE
    ) %>%

    # NAD 1983 (2011) StatePlane Florida North FIPS 0903 (Meters)
    sf::st_transform("ESRI:103021")

  # Save processed data
  outname <- paste0(
    param_name,
    "_0903.rds"
  )

  out_path <- here::here(
    "2-data",
    "processed",
    outname
  )

  saveRDS(df_out, out_path)

  return(df_out)
}

# -----------------------------------------------------------------------------
#  DEFINE PARAMETER MEANS MAP FUNCTION -
#  Creates spatial plot (map) of input parameter means for visual inspection of
#  spatial distribution.
# -----------------------------------------------------------------------------

param_means_map <- function(x, param_label) {
  my_bbox <- sf::st_bbox(get_study_area())

  tmap::tm_basemap("Esri.WorldGrayCanvas") +
    tmap::tm_shape(x, bbox = my_bbox) +
    tmap::tm_dots(
      fill = "mean",
      col = "black",
      size = 0.5,
      lwd = 0.5,
      fill.scale = tmap::tm_scale_continuous(
        values = "scico.imola"
      ),
      fill.legend = tmap::tm_legend(
        title = param_label,
        title.fontface = 2,
        #position = c("left", "bottom"),
        position = tmap::tm_pos_out("center", "bottom", pos.h = "center"),
        orientation = "landscape",
        show = TRUE
      )
    ) +
    #tmap::tm_legend(
    #position = c("left", "top"),
    #title = param_label
    #) +
    tmap::tm_layout(
      inner.margins = c(0.2, 0.2, 0.2, 0.2)
    ) +
    tmap::tm_title("Mean Observed Monitoring Location Values", fontface = 2) +
    tmap::tm_compass(type = "8star")
}

# -----------------------------------------------------------------------------
#  DEFINE PARAMETER MEANS HISTOGRAM FUNCTION -
#  Creates histogram of input parameters mean values for visiual inspection of
#  statistical distribution.
# -----------------------------------------------------------------------------

param_means_hist <- function(x, param_label) {
  ggplot2::ggplot(x, ggplot2::aes(x = mean)) +
    ggplot2::geom_histogram(
      binwidth = 0.5,
      color = "white",
      linewidth = 0.3
    ) +
    ggplot2::labs(
      title = paste("Distribution of Warm-Season Observations -", param_label),
      subtitle = "Sample Counts for monitoring locations in the Pensacola Bay System",
      x = "Observed Mean",
      y = "Number of Monitoring Locations"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      plot.subtitle = ggplot2::element_text(size = 10),
      strip.text = ggplot2::element_text(face = "bold"),
      #panel.grid.minor = element_blank()
    )
}

# -----------------------------------------------------------------------------
#  DEFINE OYSTER REEF RETRIEVAL FUNCTION -
#  Checks for oyster reef data and if not existing downloads from FWC server
# -----------------------------------------------------------------------------

get_oyster_reefs <- function() {
  file_path <- here::here(
    "2-data",
    "processed",
    "oyster_pts_0903.rds"
  )

  if (file.exists(file_path)) {
    oyster_reefs <- readRDS(file_path)
  } else {
    oysters <- sf::read_sf(
      "https://gis.myfwc.com/hosting/rest/services/Open_Data/Oyster_Beds_Statewide/MapServer/17/query?outFields=*&where=1%3D1&f=geojson"
    )

    study_area <- get_study_area()

    oyster_reefs <- oysters %>%
      sf::st_transform(sf::st_crs(study_area)) %>%
      sf::st_make_valid() %>%
      sf::st_centroid() %>%
      sf::st_filter(
        study_area,
        .predicate = st_within
      )

    saveRDS(
      oyster_reefs,
      file_path
    )
  }

  return(oyster_reefs)
}


# -----------------------------------------------------------------------------
#  DEFINE MANUAL FUNCTION FOR IDPW -
#  Allows two-step interpolation porcess to mask un-needed pixels. IDPW() func
#  resulted in washed out plots.
# -----------------------------------------------------------------------------

ipdw_manual <- function(
  sf_ob,
  cost_ras,
  range,
  par_list,
  dist_power = 1,
  trim = FALSE,
  path_dists = NULL
) {
  # Only generate if not already supplied
  if (is.null(path_dists)) {
    path_dists <- ipdw::pathdistGen(
      sf_ob = sf_ob,
      costras = cost_ras,
      range = range,
      progressbar = TRUE
    )
  }

  coverage <- raster::calc(
    path_dists,
    function(x) sum(x > 0, na.rm = TRUE)
  )

  interp <- ipdw::ipdwInterp(
    sf_ob = sf_ob,
    rstack = path_dists,
    paramlist = par_list,
    dist_power = dist_power,
    trim_rstack = trim
  )

  interp[
    cost_ras != 1 |
      coverage == 0
  ] <- NA

  raster::crs(interp) <- raster::crs(cost_ras)

  return(interp)
}
# -----------------------------------------------------------------------------
# IPDW CROSS VALIDATION FUNCTION -
# Split data into spatial folds and assess prediction accruacy of interpolation
# model.
# -----------------------------------------------------------------------------

ipdw_run <- function(
  df,
  cost_raster,
  folds,
  dist_pwr = 1.5,
  max_range = 10000,
  value_col = "mean",
  path_dists = NULL
) {
  cv_results <- purrr::map_dfr(
    seq_along(folds$folds_list),
    function(i) {
      train_idx <- folds$folds_list[[i]][[1]]
      test_idx <- folds$folds_list[[i]][[2]]

      train <- df[train_idx, ]
      test <- df[test_idx, ]

      # Generate IPDW surface from training observations
      ipdw_result <- ipdw_manual(
        train,
        cost_raster,
        max_range,
        value_col,
        dist_power = dist_pwr,
        path_dists = path_dists[[i]]
      )

      # Extract predicted values at withheld stations
      predicted <- terra::extract(
        terra::rast(ipdw_result),
        terra::vect(test),
        ID = FALSE
      )[[1]]

      tibble::tibble(
        fold = i,
        station_id = test_idx,
        observed = test[[value_col]],
        predicted = predicted
      )
    }
  )

  # Calculate statistics for the complete set of folds
  cv_results %>%
    dplyr::summarise(
      dist_power = dist_pwr,
      max_range = max_range,
      n = dplyr::n(),
      n_valid = sum(is.finite(predicted) & is.finite(observed)),
      coverage = n_valid / n,
      MAE = yardstick::mae_vec(observed, predicted, na_rm = TRUE),
      RMSE = yardstick::rmse_vec(observed, predicted, na_rm = TRUE),
      Bias = mean(predicted - observed, na.rm = TRUE)
    )
}

# -----------------------------------------------------------------------------
# IPDW SENSITIVITY FUNCTION - Test distance power, max range, or both and
# generate results table with model diagnostics to allow tuning for most
# accurate prediction surfaces.
# -----------------------------------------------------------------------------

ipdw_sensitivity <- function(
  df,
  cost_raster,
  n_seeds = 5,
  value_col = "mean",
  unit = "",
  dist_powers = 1.5,
  max_ranges = 10000,
  block_size = 5000,
  k = 5
) {
  # All parameter combinations
  parameter_grid <- tidyr::expand_grid(
    dist_power = dist_powers,
    max_range = max_ranges
  )

  # Repeated spatial cross-validation
  cv_results <- purrr::map_dfr(
    seq_len(n_seeds),
    function(s) {
      set.seed(s)

      folds <- blockCV::cv_spatial(
        x = df,
        k = k,
        size = block_size,
        selection = "random",
        plot = FALSE
      )

      # Generate path distances once for each range and fold
      path_cache <- purrr::map(
        max_ranges,
        function(max_range) {
          purrr::map(
            seq_along(folds$folds_list),
            function(i) {
              message(
                "Seed ",
                s,
                "/",
                n_seeds,
                " | Range ",
                max_range,
                " | Fold ",
                i,
                "/",
                length(folds$folds_list)
              )
              train_idx <- folds$folds_list[[i]][[1]]
              train <- df[train_idx, ]

              ipdw::pathdistGen(
                sf_ob = train,
                costras = cost_raster,
                range = max_range,
                progressbar = FALSE
              )
            }
          )
        }
      )

      # Name cached results by max range
      names(path_cache) <- as.character(max_ranges)

      # Run all parameter combinations using cached path distances
      purrr::pmap_dfr(
        parameter_grid,
        message("Running Parameter Combinations..."),
        function(dist_power, max_range) {
          message(
            "Distance Power | ",
            dist_power,
            "Max Range | ",
            max_range
          )

          ipdw_run(
            df = df,
            cost_raster = cost_raster,
            folds = folds,
            dist_pwr = dist_power,
            max_range = max_range,
            value_col = value_col,
            path_dists = path_cache[[as.character(max_range)]]
          )
        }
      ) %>%
        dplyr::mutate(seed = s)
    }
  )

  # Summarize performance across random partitions
  cv_summary <- cv_results %>%
    dplyr::group_by(dist_power, max_range) %>%
    dplyr::summarise(
      mean_MAE = mean(MAE),
      sd_MAE = sd(MAE),
      mean_RMSE = mean(RMSE),
      sd_RMSE = sd(RMSE),
      mean_Bias = mean(Bias),
      min_coverage = min(coverage),
      mean_coverage = mean(coverage),
      .groups = "drop"
    ) %>%
    dplyr::arrange(
      dplyr::desc(min_coverage),
      mean_MAE
    )

  # Best parameter combination for each partition
  best_by_seed <- cv_results %>%
    dplyr::group_by(seed) %>%
    dplyr::arrange(
      dplyr::desc(coverage),
      MAE,
      .by_group = TRUE
    ) %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::ungroup()

  # Count number of wins
  parameter_wins <- best_by_seed %>%
    dplyr::count(
      dist_power,
      max_range,
      name = "wins"
    ) %>%
    dplyr::arrange(
      dplyr::desc(wins)
    )

  # Return complete results
  list(
    summary = cv_summary,
    raw_results = cv_results,
    best_by_seed = best_by_seed,
    parameter_wins = parameter_wins
  )
}
