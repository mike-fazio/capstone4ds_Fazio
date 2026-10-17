# Load packages and functions from setup script
source(
  here::here(
    "3-scripts",
    "0-setup.r"
  )
)

proj4 <- st_crs(study_area)$proj4string

# Create polygon surrounding the study area
study_extent <- study_area %>%
  st_bbox() %>%
  st_as_sfc() %>%
  st_buffer(1000)

# Create land/barrier polygon:
land_barrier <- st_difference(
  # everything within extent EXCEPT the water study area
  study_extent,
  st_union(study_area)
)

land_barrier <- st_as_sf(land_barrier) # convert to sf

# Generate cost raster
cost_raster <- costrasterGen(
  pols = land_barrier,
  projstr = proj4,
  resolution = 100
)


# SALINITY

sal <- readRDS(
  here::here(
    "2-data",
    "processed",
    "salinity_0903.rds"
  )
)

sal <- sal %>% # TODO FIX Salinity outlier detection
  filter(
    mean < 50
  )

sal_ipdw <- ipdw_manual(sal, cost_raster, 10000, "mean", dist_power = 0.25)

plot(
  sal_ipdw,
  main = "Salinity IPDW"
)

# DO

do <- readRDS(
  here::here(
    "2-data",
    "processed",
    "do_0903.rds"
  )
)

do_ipdw <- ipdw_manual(do, cost_raster, 10000, "mean", dist_power = 0.25)

plot(
  do_ipdw,
  main = "Dissolved Oxygen IPDW"
)

# Chlorophyll-a

chla <- readRDS(
  here::here(
    "2-data",
    "processed",
    "chla_0903.rds"
  )
)

chla_ipdw <- ipdw_manual(chla, cost_raster, 10000, "mean", dist_power = 1)

plot(
  chla_ipdw,
  main = "Chlorophyll-a IPDW"
)

# Turbidiity
turb <- readRDS(
  here::here(
    "2-data",
    "processed",
    "turbidity_0903.rds"
  )
)

turb_ipdw <- ipdw_manual(turb, cost_raster, 10000, "mean", dist_power = 0.25)

plot(
  turb_ipdw,
  main = "Turbidity IPDW"
)



# Water Temp

temp <- readRDS(
  here::here(
    "2-data",
    "processed",
    "water_temp_0903.rds"
  )
)

water_temp_ipdw <- ipdw_manual(temp, cost_raster, 10000, "mean", dist_power = 0.50)

plot(
  water_temp_ipdw,
  main = "Water Temperature IPDW"
)