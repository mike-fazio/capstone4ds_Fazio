sites <- get_wq_sites()

cost_raster <- ipdw::costrasterGen(
  xymat = sites,
  pols = study_area,
  extent = "polys",
  projstr = sf::st_crs(study_area)$proj4string,
  resolution = 100
)

dp <- c(1.0, 1.5, 2.0, 2.5, 3.0)


# SAL

sal_ipdw <- ipdw_manual(sal, cost_raster, 10000, "mean", dist_power = 1.5)


# DO

do_test <- ipdw_sensitivity(
  do,
  cost_raster,
  n_seeds = 5,
  value_col = "mean",
  unit = "mg/L",
  dist_powers = dp
)
do_ipdw <- ipdw_manual(do, cost_raster, 10000, "mean", dist_power = 1.5)

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
