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
# DEFINE CANONICAL AQUATIC COST RASTER FUNCTION -
# Creates one reusable 100 m water-path template for binning and interpolation.
# -----------------------------------------------------------------------------

make_cost_raster <- function(
  study_area,
  sites,
  resolution = 100,
  buffer = 1000,
  force = FALSE
) {
  out_path <- here::here(
    "2-data",
    "processed",
    "cost_raster_100m_0903.rds"
  )

  if (file.exists(out_path) && !force) {
    cached <- readRDS(out_path)
    valid_crs <- isTRUE(
      sf::st_crs(cached)[["wkt"]] == sf::st_crs("ESRI:103021")[["wkt"]]
    )
    valid_resolution <- isTRUE(all.equal(
      raster::res(cached),
      c(resolution, resolution)
    ))
    if (valid_crs && valid_resolution) {
      return(cached)
    }
  }

  study_area <- sf::st_transform(study_area, "ESRI:103021")
  sites <- sf::st_transform(sites, sf::st_crs(study_area))

  study_extent <- study_area %>%
    sf::st_bbox() %>%
    sf::st_as_sfc() %>%
    sf::st_buffer(buffer)

  land_barrier <- sf::st_difference(
    study_extent,
    sf::st_union(study_area)
  ) %>%
    sf::st_as_sf()

  projection <- "ESRI:103021"
  cost_raster <- suppressMessages(ipdw::costrasterGen(
    xymat = sites,
    pols = land_barrier,
    extent = "polys",
    projstr = projection,
    resolution = resolution
  ))
  raster::projection(cost_raster) <- projection

  if (!isTRUE(all.equal(raster::res(cost_raster), c(resolution, resolution)))) {
    stop("The cost raster was not created at the requested resolution.")
  }

  saveRDS(cost_raster, out_path)
  cost_raster
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

proc_wq_data <- function(
  df,
  param_name,
  cost_raster,
  min_n = 2,
  save_audit = TRUE
) {
  collapse_ids <- function(x) {
    paste(sort(unique(stats::na.omit(as.character(x)))), collapse = ";")
  }

  retained <- df %>%
    dplyr::mutate(
      Result_Measure = suppressWarnings(as.numeric(Result_Measure)),
      year = lubridate::year(Activity_StartDate)
    ) %>%
    dplyr::filter(
      lubridate::month(Activity_StartDate) %in% 5:10,
      is.finite(Result_Measure),
      is.finite(Location_Latitude),
      is.finite(Location_Longitude),
      !is.na(year)
    ) %>%
    sf::st_as_sf(
      coords = c("Location_Longitude", "Location_Latitude"),
      crs = 4326,
      remove = FALSE
    ) %>%
    sf::st_transform(sf::st_crs(raster::crs(cost_raster)))

  xy <- sf::st_coordinates(retained)
  retained$cell_id <- raster::cellFromXY(cost_raster, xy)
  retained$cost_value <- raster::extract(cost_raster, xy)

  excluded <- retained %>%
    dplyr::filter(is.na(cell_id) | is.na(cost_value) | cost_value != 1)

  retained <- retained %>%
    dplyr::filter(!is.na(cell_id), cost_value == 1) %>%
    sf::st_drop_geometry()

  annual_summary <- retained %>%
    dplyr::group_by(cell_id, year) %>%
    dplyr::summarise(
      annual_mean = mean(Result_Measure),
      annual_sd = stats::sd(Result_Measure),
      annual_n = dplyr::n(),
      station_count = dplyr::n_distinct(Location_Identifier),
      organization_count = dplyr::n_distinct(Org_Identifier),
      station_ids = collapse_ids(Location_Identifier),
      organization_ids = collapse_ids(Org_Identifier),
      .groups = "drop"
    )

  cell_metadata <- retained %>%
    dplyr::group_by(cell_id) %>%
    dplyr::summarise(
      station_count = dplyr::n_distinct(Location_Identifier),
      organization_count = dplyr::n_distinct(Org_Identifier),
      station_ids = collapse_ids(Location_Identifier),
      organization_ids = collapse_ids(Org_Identifier),
      .groups = "drop"
    )

  cell_summary <- annual_summary %>%
    dplyr::group_by(cell_id) %>%
    dplyr::summarise(
      mean = mean(annual_mean),
      annual_mean_sd = stats::sd(annual_mean),
      sd = annual_mean_sd,
      years_sampled = dplyr::n(),
      total_n = sum(annual_n),
      first_year = min(year),
      last_year = max(year),
      .groups = "drop"
    ) %>%
    dplyr::left_join(cell_metadata, by = "cell_id") %>%
    dplyr::filter(total_n >= min_n)

  centers <- raster::xyFromCell(cost_raster, cell_summary$cell_id)
  cell_summary$x <- centers[, 1]
  cell_summary$y <- centers[, 2]

  df_out <- cell_summary %>%
    sf::st_as_sf(
      coords = c("x", "y"),
      crs = sf::st_crs(raster::crs(cost_raster)),
      remove = FALSE
    )

  out_path <- here::here(
    "2-data",
    "processed",
    paste0(param_name, "_0903.rds")
  )
  saveRDS(df_out, out_path)

  if (save_audit) {
    saveRDS(
      annual_summary,
      here::here(
        "2-data",
        "processed",
        paste0(param_name, "_cell_year_0903.rds")
      )
    )
    saveRDS(
      excluded,
      here::here(
        "2-data",
        "processed",
        paste0(param_name, "_grid_exclusions_0903.rds")
      )
    )
  }

  df_out
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
  path_dists = NULL,
  return_support = FALSE,
  progressbar = TRUE
) {
  # Only generate if not already supplied
  if (is.null(path_dists)) {
    path_dists <- ipdw::pathdistGen(
      sf_ob = sf_ob,
      costras = cost_ras,
      range = range,
      progressbar = progressbar
    )
  }

  contributor_count <- raster::calc(
    path_dists,
    function(x) sum(x > 0, na.rm = TRUE)
  )

  nearest_path_distance <- raster::calc(
    path_dists,
    function(x) {
      max_weight <- suppressWarnings(max(x[x > 0], na.rm = TRUE))
      if (!is.finite(max_weight)) {
        if (is.infinite(max_weight)) 0 else NA_real_
      } else {
        range / (2 * sqrt(max_weight))
      }
    }
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
      contributor_count == 0
  ] <- NA

  unsupported <- cost_ras != 1 | contributor_count == 0
  contributor_count[unsupported] <- NA
  nearest_path_distance[unsupported] <- NA

  raster::crs(interp) <- raster::crs(cost_ras)
  raster::crs(contributor_count) <- raster::crs(cost_ras)
  raster::crs(nearest_path_distance) <- raster::crs(cost_ras)

  if (!return_support) {
    return(interp)
  }

  list(
    surface = interp,
    contributor_count = contributor_count,
    nearest_path_distance = nearest_path_distance
  )
}

# -----------------------------------------------------------------------------
# IPDW METRIC SUMMARY FUNCTION -
# Uses predicted minus observed as the signed prediction error.
# -----------------------------------------------------------------------------

summarise_ipdw_metrics <- function(point_results, by = character()) {
  finite_mean <- function(x) {
    x <- x[is.finite(x)]
    if (length(x) == 0) NA_real_ else mean(x)
  }

  grouped <- if (length(by) == 0) {
    point_results
  } else {
    dplyr::group_by(point_results, dplyr::across(dplyr::all_of(by)))
  }

  grouped %>%
    dplyr::summarise(
      n = dplyr::n(),
      n_valid = sum(is.finite(observed) & is.finite(predicted)),
      coverage = n_valid / n,
      MAE = finite_mean(abs(error)),
      RMSE = sqrt(finite_mean(error^2)),
      Bias = finite_mean(error),
      .groups = "drop"
    )
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
  path_dists = NULL,
  path_range = max_range,
  seed = NA_integer_
) {
  if (path_range < max_range) {
    stop("path_range must be at least as large as max_range.")
  }

  if (is.null(path_dists)) {
    path_dists <- vector("list", length(folds$folds_list))
  }

  cv_results <- purrr::map_dfr(
    seq_along(folds$folds_list),
    function(i) {
      train_idx <- folds$folds_list[[i]][[1]]
      test_idx <- folds$folds_list[[i]][[2]]

      train <- df[train_idx, ]
      test <- df[test_idx, ]

      if (length(intersect(train$cell_id, test$cell_id)) > 0) {
        stop("A raster cell occurs in both the training and test data.")
      }

      fold_paths <- path_dists[[i]]
      if (is.null(fold_paths)) {
        fold_paths <- ipdw::pathdistGen(
          sf_ob = train,
          costras = cost_raster,
          range = max_range,
          progressbar = FALSE
        )
        path_range <- max_range
      }

      path_weights <- raster::extract(
        fold_paths,
        sf::st_coordinates(test)
      ) %>%
        as.matrix()

      if (path_range != max_range) {
        path_weights <- path_weights * (max_range / path_range)^2
        path_weights[
          is.finite(path_weights) & path_weights <= 1
        ] <- 0
      }

      train_values <- train[[value_col]]
      predicted <- apply(
        path_weights,
        1,
        function(weights) {
          exact <- is.infinite(weights) & weights > 0
          if (any(exact)) {
            return(mean(train_values[exact]))
          }
          valid_weights <- is.finite(weights) &
            weights > 0 &
            is.finite(train_values)
          if (!any(valid_weights)) {
            return(NA_real_)
          }
          powered <- weights[valid_weights]^dist_pwr
          sum(powered * train_values[valid_weights]) / sum(powered)
        }
      )

      contributor_count <- rowSums(path_weights > 0, na.rm = TRUE)
      nearest_path_distance <- apply(
        path_weights,
        1,
        function(weights) {
          max_weight <- suppressWarnings(max(
            weights[weights > 0],
            na.rm = TRUE
          ))
          if (identical(max_weight, Inf)) {
            0
          } else if (!is.finite(max_weight)) {
            NA_real_
          } else {
            max_range / (2 * sqrt(max_weight))
          }
        }
      )

      coordinates <- sf::st_coordinates(test)
      observed <- test[[value_col]]
      valid <- is.finite(observed) & is.finite(predicted)

      support_columns <- intersect(
        c(
          "annual_mean_sd",
          "years_sampled",
          "total_n",
          "first_year",
          "last_year",
          "station_count",
          "organization_count"
        ),
        names(test)
      )

      tibble::tibble(
        seed = seed,
        fold = i,
        row_id = test_idx,
        cell_id = test$cell_id,
        x = coordinates[, 1],
        y = coordinates[, 2],
        observed = observed,
        predicted = predicted,
        error = dplyr::if_else(valid, predicted - observed, NA_real_),
        absolute_error = dplyr::if_else(
          valid,
          abs(predicted - observed),
          NA_real_
        ),
        squared_error = dplyr::if_else(
          valid,
          (predicted - observed)^2,
          NA_real_
        ),
        valid = valid,
        contributor_count = contributor_count,
        nearest_path_distance = nearest_path_distance,
        dist_power = dist_pwr,
        max_range = max_range
      ) %>%
        dplyr::bind_cols(
          sf::st_drop_geometry(test)[support_columns]
        )
    }
  )

  list(
    point_results = cv_results,
    fold_results = summarise_ipdw_metrics(
      cv_results,
      c("seed", "fold", "dist_power", "max_range")
    ),
    seed_results = summarise_ipdw_metrics(
      cv_results,
      c("seed", "dist_power", "max_range")
    ),
    summary = summarise_ipdw_metrics(
      cv_results,
      c("dist_power", "max_range")
    )
  )
}

# -----------------------------------------------------------------------------
# IPDW SENSITIVITY FUNCTION - Test distance power, max range, or both and
# generate results table with model diagnostics to allow tuning for most
# accurate prediction surfaces.
# -----------------------------------------------------------------------------

ipdw_sensitivity_signature <- function(
  df,
  value_col,
  dist_powers,
  max_ranges,
  seed_values,
  block_size,
  k
) {
  coordinates <- sf::st_coordinates(df)
  cells <- data.frame(
    cell_id = df$cell_id,
    value = df[[value_col]],
    x = coordinates[, 1],
    y = coordinates[, 2]
  )
  cells <- cells[order(cells$cell_id), , drop = FALSE]

  digest::digest(
    list(
      version = 1L,
      cells = cells,
      value_col = value_col,
      dist_powers = dist_powers,
      max_ranges = max_ranges,
      seed_values = seed_values,
      block_size = block_size,
      k = k
    ),
    algo = "sha256"
  )
}

ipdw_sensitivity <- function(
  df,
  cost_raster,
  n_seeds = 5,
  value_col = "mean",
  unit = "",
  dist_powers = 1.5,
  max_ranges = 10000,
  block_size = 5000,
  k = 5,
  seed_values = NULL
) {
  if (is.null(seed_values)) {
    seed_values <- seq_len(n_seeds)
  }

  if (anyDuplicated(df$cell_id)) {
    stop("Each input row must represent a unique aquatic raster cell.")
  }

  # All parameter combinations
  parameter_grid <- tidyr::expand_grid(
    dist_power = dist_powers,
    max_range = max_ranges
  )

  # Repeated spatial cross-validation
  seed_runs <- purrr::map(
    seed_values,
    function(s) {
      set.seed(s)

      folds <- blockCV::cv_spatial(
        x = df,
        k = k,
        size = block_size,
        selection = "random",
        plot = FALSE
      )

      fold_assignments <- purrr::map_dfr(
        seq_along(folds$folds_list),
        function(i) {
          train_idx <- folds$folds_list[[i]][[1]]
          test_idx <- folds$folds_list[[i]][[2]]
          if (length(intersect(train_idx, test_idx)) > 0) {
            stop("A row occurs in both the training and test sets.")
          }
          tibble::tibble(
            seed = s,
            fold = i,
            row_id = test_idx,
            cell_id = df$cell_id[test_idx]
          )
        }
      )

      if (
        nrow(fold_assignments) != nrow(df) ||
          anyDuplicated(fold_assignments$cell_id)
      ) {
        stop("Each cell must occur in exactly one test fold per seed.")
      }

      # Generate paths once at the longest range. Shorter ranges are exact
      # rescalings of these inverse-squared path weights.
      path_range <- max(max_ranges)
      path_cache <- purrr::map(
        seq_along(folds$folds_list),
        function(i) {
          message(
            "Seed ",
            s,
            "/",
            length(seed_values),
            " | Path range ",
            path_range,
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
            range = path_range,
            progressbar = FALSE
          )
        }
      )

      # Run all parameter combinations using cached path distances
      model_runs <- purrr::map(
        seq_len(nrow(parameter_grid)),
        function(j) {
          dist_power <- parameter_grid$dist_power[j]
          max_range <- parameter_grid$max_range[j]
          message(
            "Seed ",
            s,
            " | Distance power ",
            dist_power,
            " | Maximum range ",
            max_range
          )

          ipdw_run(
            df = df,
            cost_raster = cost_raster,
            folds = folds,
            dist_pwr = dist_power,
            max_range = max_range,
            value_col = value_col,
            path_dists = path_cache,
            path_range = path_range,
            seed = s
          )
        }
      )

      list(
        point_results = purrr::map_dfr(model_runs, "point_results"),
        fold_assignments = fold_assignments
      )
    }
  )

  point_results <- purrr::map_dfr(seed_runs, "point_results")
  fold_assignments <- purrr::map_dfr(seed_runs, "fold_assignments")
  cv_results <- summarise_ipdw_metrics(
    point_results,
    c("seed", "dist_power", "max_range")
  )
  fold_results <- summarise_ipdw_metrics(
    point_results,
    c("seed", "fold", "dist_power", "max_range")
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
      sd_Bias = sd(Bias),
      min_coverage = min(coverage),
      mean_coverage = mean(coverage),
      sd_coverage = sd(coverage),
      .groups = "drop"
    ) %>%
    dplyr::arrange(
      dplyr::desc(min_coverage),
      dplyr::desc(mean_coverage),
      mean_MAE,
      mean_RMSE,
      abs(mean_Bias),
      max_range,
      dist_power
    )

  # Best parameter combination for each partition
  best_by_seed <- cv_results %>%
    dplyr::group_by(seed) %>%
    dplyr::arrange(
      dplyr::desc(coverage),
      MAE,
      RMSE,
      abs(Bias),
      max_range,
      dist_power,
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
    parameter_wins = parameter_wins,
    point_results = point_results,
    fold_results = fold_results,
    fold_assignments = fold_assignments,
    selected = dplyr::slice_head(cv_summary, n = 1)
  )
}

# -----------------------------------------------------------------------------
# IPDW MORAN'S I FUNCTION -
# Tests validation errors separately for each seed using aquatic-path weights.
# -----------------------------------------------------------------------------

ipdw_moran <- function(
  point_results,
  observations,
  path_dists,
  nsim = 999,
  seed = 2026
) {
  if (anyDuplicated(observations$cell_id)) {
    stop("Moran's I observations must have unique cell IDs.")
  }

  repeated_cells <- point_results %>%
    dplyr::count(seed, cell_id) %>%
    dplyr::filter(n != 1)
  if (nrow(repeated_cells) > 0) {
    stop("Moran's I requires one selected-model prediction per cell and seed.")
  }

  path_weights <- raster::extract(
    path_dists,
    sf::st_coordinates(observations)
  ) %>%
    as.matrix()

  if (
    nrow(path_weights) != nrow(observations) ||
      ncol(path_weights) != nrow(observations)
  ) {
    stop("The path-distance stack does not match the observation rows.")
  }

  path_weights[!is.finite(path_weights)] <- 0
  diag(path_weights) <- 0
  path_weights <- (path_weights + t(path_weights)) / 2

  purrr::map_dfr(
    sort(unique(point_results$seed)),
    function(validation_seed) {
      seed_rows <- point_results %>%
        dplyr::filter(seed == validation_seed, is.finite(error))
      index <- match(seed_rows$cell_id, observations$cell_id)
      keep <- !is.na(index)
      seed_rows <- seed_rows[keep, ]
      index <- index[keep]
      weights <- path_weights[index, index, drop = FALSE]
      connected <- rowSums(weights) > 0
      seed_rows <- seed_rows[connected, ]
      weights <- weights[connected, connected, drop = FALSE]

      if (nrow(seed_rows) < 3 || sum(weights) == 0) {
        return(tibble::tibble(
          seed = validation_seed,
          n = nrow(seed_rows),
          moran_i = NA_real_,
          expected_i = NA_real_,
          permutation_p = NA_real_
        ))
      }

      list_weights <- spdep::mat2listw(
        weights,
        style = "W",
        zero.policy = TRUE
      )
      set.seed(seed + validation_seed)
      test <- spdep::moran.mc(
        seed_rows$error,
        listw = list_weights,
        nsim = nsim,
        alternative = "two.sided",
        zero.policy = TRUE
      )

      tibble::tibble(
        seed = validation_seed,
        n = nrow(seed_rows),
        moran_i = unname(test$statistic),
        expected_i = -1 / (nrow(seed_rows) - 1),
        permutation_p = test$p.value
      )
    }
  )
}

# -----------------------------------------------------------------------------
# IPDW DIAGNOSTIC PLOT FUNCTION -
# Creates cell-level validation tables and required model diagnostic figures.
# -----------------------------------------------------------------------------

ipdw_diagnostics <- function(
  point_results,
  observations,
  sensitivity_summary = NULL,
  unit = ""
) {
  unit_label <- if (nzchar(unit)) paste0(" (", unit, ")") else ""

  cell_errors <- point_results %>%
    dplyr::group_by(cell_id) %>%
    dplyr::summarise(
      observed = mean(observed, na.rm = TRUE),
      predicted = mean(predicted, na.rm = TRUE),
      error = mean(error, na.rm = TRUE),
      absolute_error = mean(abs(error), na.rm = TRUE),
      validation_coverage = mean(valid),
      contributor_count = mean(contributor_count, na.rm = TRUE),
      nearest_path_distance = mean(nearest_path_distance, na.rm = TRUE),
      .groups = "drop"
    )

  spatial_errors <- observations %>%
    dplyr::select(cell_id) %>%
    dplyr::left_join(cell_errors, by = "cell_id")

  agreement_limits <- range(
    c(point_results$observed, point_results$predicted),
    na.rm = TRUE
  )

  observed_vs_predicted <- ggplot2::ggplot(
    point_results,
    ggplot2::aes(observed, predicted)
  ) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = 2) +
    ggplot2::geom_point(alpha = 0.55) +
    ggplot2::scale_x_continuous(limits = agreement_limits) +
    ggplot2::scale_y_continuous(limits = agreement_limits) +
    ggplot2::coord_equal() +
    ggplot2::labs(
      x = paste0("Observed", unit_label),
      y = paste0("Predicted", unit_label)
    ) +
    ggplot2::theme_minimal()

  prediction_error <- ggplot2::ggplot(
    point_results,
    ggplot2::aes(predicted, error)
  ) +
    ggplot2::geom_hline(yintercept = 0, linetype = 2) +
    ggplot2::geom_point(alpha = 0.55) +
    ggplot2::labs(
      x = paste0("Predicted", unit_label),
      y = paste0("Prediction error", unit_label)
    ) +
    ggplot2::theme_minimal()

  error_distribution <- ggplot2::ggplot(
    point_results,
    ggplot2::aes(error)
  ) +
    ggplot2::geom_histogram(bins = 25, color = "white") +
    ggplot2::geom_vline(xintercept = 0, linetype = 2) +
    ggplot2::labs(
      x = paste0("Prediction error", unit_label),
      y = "Validation predictions"
    ) +
    ggplot2::theme_minimal()

  study_outline <- get_study_area() %>%
    sf::st_transform(sf::st_crs(spatial_errors))

  spatial_error <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = study_outline,
      fill = "grey95",
      color = "grey65",
      linewidth = 0.3
    ) +
    ggplot2::geom_sf(
      data = spatial_errors,
      ggplot2::aes(color = error),
      size = 2
    ) +
    ggplot2::scale_color_gradient2(
      low = "#b2182b",
      mid = "white",
      high = "#2166ac",
      midpoint = 0
    ) +
    ggplot2::labs(
      x = NULL,
      y = NULL,
      color = paste0("Error", unit_label)
    ) +
    ggplot2::theme_minimal()

  support_long <- point_results %>%
    dplyr::select(error, contributor_count, nearest_path_distance) %>%
    tidyr::pivot_longer(
      c(contributor_count, nearest_path_distance),
      names_to = "support_measure",
      values_to = "support"
    )

  support_error <- ggplot2::ggplot(
    support_long,
    ggplot2::aes(support, abs(error))
  ) +
    ggplot2::geom_point(alpha = 0.45) +
    ggplot2::geom_smooth(method = "loess", se = FALSE) +
    ggplot2::facet_wrap(
      ggplot2::vars(support_measure),
      scales = "free_x",
      labeller = ggplot2::as_labeller(c(
        contributor_count = "Contributor count",
        nearest_path_distance = "Nearest aquatic-path distance (m)"
      ))
    ) +
    ggplot2::labs(
      x = "Descriptive support measure",
      y = paste0("Absolute prediction error", unit_label)
    ) +
    ggplot2::theme_minimal()

  parameter_sensitivity <- NULL
  if (!is.null(sensitivity_summary)) {
    parameter_sensitivity <- ggplot2::ggplot(
      sensitivity_summary,
      ggplot2::aes(
        factor(dist_power),
        factor(max_range),
        fill = mean_MAE
      )
    ) +
      ggplot2::geom_tile() +
      ggplot2::geom_point(
        data = dplyr::filter(sensitivity_summary, min_coverage < 1),
        ggplot2::aes(shape = "Incomplete coverage"),
        color = "black",
        size = 2.5,
        stroke = 0.8
      ) +
      ggplot2::geom_point(
        data = dplyr::slice_head(sensitivity_summary, n = 1),
        shape = 21,
        fill = NA,
        color = "white",
        size = 5,
        stroke = 1.2
      ) +
      ggplot2::scale_fill_viridis_c() +
      ggplot2::scale_shape_manual(
        values = c("Incomplete coverage" = 4),
        name = NULL
      ) +
      ggplot2::labs(
        x = "Distance power",
        y = "Maximum range (m)",
        fill = paste0("Mean MAE", unit_label)
      ) +
      ggplot2::theme_minimal()
  }

  list(
    cell_errors = spatial_errors,
    observed_vs_predicted = observed_vs_predicted,
    prediction_error = prediction_error,
    error_distribution = error_distribution,
    spatial_error = spatial_error,
    support_error = support_error,
    parameter_sensitivity = parameter_sensitivity
  )
}

# -----------------------------------------------------------------------------
# IPDW RASTER QA FUNCTION -
# Checks geometry, mask, support, values, coverage, and prediction range.
# -----------------------------------------------------------------------------

ipdw_qa <- function(
  bundle,
  cost_raster,
  observations,
  max_range,
  value_col = "mean",
  stop_on_fail = TRUE
) {
  surface <- bundle$surface
  contributors <- bundle$contributor_count
  nearest <- bundle$nearest_path_distance

  aligned <- isTRUE(raster::compareRaster(
    surface,
    contributors,
    nearest,
    cost_raster,
    extent = TRUE,
    rowcol = TRUE,
    crs = TRUE,
    res = TRUE,
    orig = TRUE,
    rotation = TRUE,
    values = FALSE,
    stopiffalse = FALSE
  ))

  cost_values <- raster::getValues(cost_raster)
  surface_values <- raster::getValues(surface)
  contributor_values <- raster::getValues(contributors)
  nearest_values <- raster::getValues(nearest)
  water <- is.finite(cost_values) & cost_values == 1
  supported <- is.finite(contributor_values) & contributor_values > 0
  observation_cells <- raster::cellFromXY(
    cost_raster,
    sf::st_coordinates(observations)
  )
  observation_cost <- raster::extract(
    cost_raster,
    sf::st_coordinates(observations)
  )
  tolerance <- sqrt(.Machine$double.eps)
  canonical_crs <- isTRUE(
    sf::st_crs(cost_raster)[["wkt"]] == sf::st_crs("ESRI:103021")[["wkt"]]
  )
  canonical_resolution <- isTRUE(all.equal(
    raster::res(cost_raster),
    c(100, 100)
  ))

  qa <- tibble::tribble(
    ~check                   , ~pass                                                                                                                                                  , ~detail                                                                                     ,
    "canonical_crs"          , canonical_crs                                                                                                                                          , "Template CRS is ESRI:103021"                                                               ,
    "canonical_resolution"   , canonical_resolution                                                                                                                                   , "Template resolution is 100 m"                                                              ,
    "raster_alignment"       , aligned                                                                                                                                                , "Surface and support rasters match the canonical template"                                  ,
    "unique_input_cells"     , !anyDuplicated(observations$cell_id)                                                                                                                   , paste(nrow(observations), "input cells")                                                    ,
    "input_cell_assignment"  , all(observation_cells == observations$cell_id)                                                                                                         , "Input geometry matches stored cell IDs"                                                    ,
    "aquatic_inputs"         , all(observation_cost == 1)                                                                                                                             , "All input cells are aquatic"                                                               ,
    "land_mask"              , all(is.na(surface_values[!water]))                                                                                                                     , "Land cells are NA"                                                                         ,
    "unsupported_water_mask" , all(is.na(surface_values[water & !supported]))                                                                                                         , "Unsupported water cells are NA"                                                            ,
    "finite_surface"         , all(is.finite(surface_values[!is.na(surface_values)]))                                                                                                 , "All retained surface values are finite"                                                    ,
    "finite_support"         , all(is.finite(contributor_values[!is.na(contributor_values)])) && all(is.finite(nearest_values[!is.na(nearest_values)]))                               , "All retained support values are finite"                                                    ,
    "support_consistency"    , all(is.na(surface_values) == is.na(contributor_values)) && all(is.na(surface_values) == is.na(nearest_values))                                         , "Surface and support NA masks match"                                                        ,
    "contributor_counts"     , all(contributor_values[supported] >= 1 & contributor_values[supported] %% 1 == 0)                                                                      , "Contributor counts are positive integers"                                                  ,
    "path_distance_range"    , all(nearest_values[supported] >= 0 & nearest_values[supported] <= max_range + tolerance)                                                               , paste("Nearest path distance is between 0 and", max_range, "m")                             ,
    "prediction_range"       , all(surface_values[supported] >= min(observations[[value_col]]) - tolerance & surface_values[supported] <= max(observations[[value_col]]) + tolerance) , "Predictions remain within the observed range"                                              ,
    "aquatic_coverage"       , sum(supported & water) > 0                                                                                                                             , paste0(round(100 * sum(supported & water) / sum(water), 1), "% of aquatic cells supported")
  )

  if (stop_on_fail && any(!qa$pass)) {
    stop(
      "IPDW QA failed: ",
      paste(qa$check[!qa$pass], collapse = ", ")
    )
  }

  qa
}
