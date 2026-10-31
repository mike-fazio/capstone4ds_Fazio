library(dataRetrieval)
library(sf)
library(dplyr)
library(tmap)
library(knitr)
library(tibble)
library(ipdw)
library(ggplot2)

tmap_mode("plot")

# Functions ----

source(
  here::here(
    "3-scripts",
    "1-functions.r"
  )
)

study_area <- get_study_area()
