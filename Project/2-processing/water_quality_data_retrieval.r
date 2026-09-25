library(dataRetrieval)
library(sf)
library(arcgis)
library(tidyverse)

# Create study area using FDEP WBID feature service

json_url <- "https://ca.dep.state.fl.us/arcgis/rest/services/OpenData/WBIDS/MapServer/0/query?outFields=*&where=1%3D1&f=geojson"
wbid <- read_sf(json_url)

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


# Get all estuary monitoring sites in Escambia County
pbs_sites <- whatWQPsites(
  # changed from whatWQPdata to whatWQPsites to test wqx3
  countycode = c("US:12:033", "US:12:113"),
  siteType = "Estuary",
  legac = FALSE
)

# Convert the monitoring sites into spatial points
pbs_sites <- pbs_sites %>%
  select(
    OrganizationIdentifier,
    OrganizationFormalName,
    MonitoringLocationIdentifier,
    LongitudeMeasure,
    LatitudeMeasure
  ) %>%
  st_as_sf(
    coords = c("LongitudeMeasure", "LatitudeMeasure"),
    crs = 4326,
    remove = FALSE
  ) %>%
  st_transform(st_crs(study_area)) %>%
  st_filter(study_area, .predicate = st_within)

# Plot the study area and monitoring sites
ggplot() +
  geom_sf(
    data = study_area,
    fill = "lightblue",
    color = "navy"
  ) +
  geom_sf(
    data = pbs_sites,
    color = "red",
    size = 2
  ) +
  theme_minimal()


# get wq data using sites
pbs_data <- readWQPdata(
  siteid = pbs_sites$MonitoringLocationIdentifier,
  dataProfile = "narrow",
  service = "ResultWQX3"
)

get_wqp_data <- function(wq_parameter) {
  # create empty data frame to store data
  df <- data.frame()

  # loop through site list
  for (i in seq_len(nrow(pbs_sites))) {
    site_id <- pbs_sites$MonitoringLocationIdentifier[i]
    message("Retrieving: ", site_id)

    wq_df <-
      tryCatch(
        {
          readWQPdata(
            siteid = site_id,
            characteristicName = wq_parameter,
            startDateLo = "2005-01-01",
            startDateHi = "2025-01-01",
            dataProfile = "narrow",
            service = "ResultWQX3",
            ignore_attribute = FALSE
          )
        },
        error = function(cond) {
          message(paste("No data in:", site_id))
        }
      )

    df <- bind_rows(df, wq_df)
  }
}

pH <- get_wqp_data("pH")
