library(dataRetrieval)
library(sf)
library(dplyr)
library(tmap)
library(knitr)
library(tibble)
library(ipdw)

tmap_mode("plot")

# Functions ----

source(
  here::here(
    "3-scripts",
    "1-functions.r"
  )
)

study_area <- get_study_area()

pbs_sites <- get_wq_sites(study_area)

cost_raster <- make_cost_raster(
  study_area,
  pbs_sites,
  resolution = 100
)

read_processed <- function(filename) {
  path <- here::here("2-data", "processed", filename)
  if (file.exists(path)) readRDS(path) else NULL
}

do <- read_processed("do_0903.rds")
sal <- read_processed("salinity_0903.rds")
chla <- read_processed("chla_0903.rds")
turb <- read_processed("turbidity_0903.rds")
temp <- read_processed("water_temp_0903.rds")
reefs <- read_processed("oyster_pts_0903.rds")
