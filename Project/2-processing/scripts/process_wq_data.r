library(dataRetrieval)
library(sf)
library(tidyverse)

### CREATE OR LOAD STUDY AREA ----

get_study_area <- function() {
  file_path <- "2-processing/data/processed/study_area.rds"

  if (file.exists(file_path)) {
    study_area <- readRDS(file_path)
  } else {
    wbid <- st_read("2-processing/data/raw/Waterbody_IDs.shp")

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
      filter(WBID %in% study_area_wbid_list) %>% # Filter results to only include desired WBIDs
      summarise() # dissolve all polygons into one singular geometry

    saveRDS(study_area, file_path)
  }

  return(study_area)
}

### CREATE OR LOAD SITES ----

get_wq_sites <- function(study_area) {
  # file path to check
  out_path <- "2-processing/data/processed/wq_sites.rds"

  # Load data if exists, else proceed to download
  if (file.exists(out_path)) {
    pbs_sites <- readRDS(out_path)
  } else {
    # Query WQP and cache result
    wqp_sites <- whatWQPsites(
      countycode = c("US:12:033", "US:12:113"),
      siteType = "Estuary",
      legacy = FALSE
    )

    # filter sites to Pensacola Bay System
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
        coords = c("Location_Longitude", "Location_Latitude"),
        crs = 4326,
        remove = TRUE
      ) %>%
      st_transform(st_crs(study_area)) %>%
      st_filter(study_area, .predicate = st_within)

    # save to file
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
  # create file name based on user provided input
  file_name <- paste0(output_name, ".rds")

  # Build output path
  out_path <- file.path("2-processing/data/raw", file_name)

  if (file.exists(out_path) && !force) {
    # load processed data
    pbs_df <- readRDS(out_path)
  } else {
    # site list used to filter query results
    site_list <- unique(sites$Location_Identifier)

    # download data from WQP api
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
    # filter out sites
    pbs_df <- wq_df %>%
      filter(Location_Identifier %in% site_list)

    # Save dataset
    saveRDS(pbs_df, out_path)
  }

  # return data
  return(pbs_df)
}
