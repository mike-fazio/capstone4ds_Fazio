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

sal_ras <- ipdw(
  sal,
  cost_raster,
  range = 10000,
  paramlist = 'mean',
  trim_rstack = TRUE
)

ipdw_manual <- function(sf_ob, cost_ras, range, par_list, trim = FALSE) {

  # Generate path-distance rasters
  path_dists <- pathdistGen(
    sf_ob = sf_ob,
    costras = cost_ras,
    range = range,
    progressbar = TRUE
  )

  # Calculate number of contributing stations per cell
  coverage <- raster::calc(
    path_dists,
    fun = function(x) {
      sum(x > 0, na.rm = TRUE)
    }
  )

  # Interpolate
  interp <- ipdwInterp(
    sf_ob = sf_ob,
    rstack = path_dists,
    paramlist = par_list,
    trim_rstack = trim
  )

  # Remove barriers and cells with no interpolation support
  interp[
    cost_ras != 1 |
      coverage == 0
  ] <- NA

  # Restore CRS
  raster::crs(interp) <- raster::crs(cost_ras)

  return(interp)
}


sal_test <- ipdw_manual(sal, cost_raster, 10000, "mean")


path_dists <- pathdistGen(
  sf_ob = sal,
  costras = cost_raster,
  range = 10000,
  progressbar = TRUE
)

sal_ras_test <- ipdwInterp(
  sf_ob = sal,
  rstack = path_dists,
  paramlist = "mean",
  trim_rstack = FALSE
)

sal_ras_clean <- sal_ras_test

sal_ras_clean[
  cost_raster != 1 |
    coverage == 0
] <- NA


plot(
  sal_ras_clean,
  main = "Salinity IPDW"
)

plot(
  st_geometry(sal),
  add = TRUE,
  pch = 20,
  cex = 0.4
)
