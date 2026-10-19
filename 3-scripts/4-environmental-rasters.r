

# DO

do <- readRDS(
  here::here(
    "2-data",
    "processed",
    "do_0903.rds"
  )
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


