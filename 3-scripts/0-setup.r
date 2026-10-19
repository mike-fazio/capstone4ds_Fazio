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

do <- readRDS(
  here::here(
    "2-data",
    "processed",
    "do_0903.rds"
  )
)

sal <- readRDS(
  here::here(
    "2-data",
    "processed",
    "salinity_0903.rds"
  )
)

turb <- readRDS(
  here::here(
    "2-data",
    "processed",
    "turbidity_0903.rds"
  )
)

temp <- readRDS(
  here::here(
    "2-data",
    "processed",
    "water_temp_0903.rds"
  )
)

reefs <- readRDS(
  here::here(
    "2-data",
    "processed",
    "oyster_pts_0903.rds"
  )
)
