#load functions from source script
source("Project/3-scripts/0-functions.r")

pbs_sites <- get_wq_sites(study_area)

## Add sites to map for visualization
tm_shape(pbs_sites) +
  tm_dots(
    fill = "red",
    size = 0.3
  ) +
  tm_shape(study_area) +
  tm_polygons(
    fill_alpha = 0,
    col = "black",
    lwd = 2
  ) +
  tm_basemap("Esri.WorldStreetMap") +
  tm_layout(inner.margins = c(0.2, 0.2, 0.2, 0.2))


# get wq data using retrieved sites and selected parameters

salinity <- get_wq_data(
  param = "Salinity",
  from_date = "2005-01-01",
  to_date = "2025-01-01",
  sites = pbs_sites,
  output_name = "salinity",
  force = FALSE
)

water_temp <- get_wq_data(
  param = "Temperature, water",
  from_date = "2005-01-01",
  to_date = "2025-01-01",
  sites = pbs_sites,
  output_name = "water_temp",
  force = FALSE
)

do_mgL <- get_wq_data(
  param = "Dissolved oxygen (DO)",
  from_date = "2005-01-01",
  to_date = "2025-01-01",
  sites = pbs_sites,
  output_name = "do_mgL",
  force = FALSE
)

chl_a_mgL <- get_wq_data(
  param = "Chlorophyll a, corrected for pheophytin",
  from_date = "2005-01-01",
  to_date = "2025-01-01",
  sites = pbs_sites,
  output_name = "chl_a_mgL",
  force = FALSE
)

turbidity <- get_wq_data(
  param = "Turbidity",
  from_date = "2005-01-01",
  to_date = "2025-01-01",
  sites = pbs_sites,
  output_name = "turbidity",
  force = FALSE
)

sal <- proc_wq_data(salinity)
do <- proc_wq_data(do_mgL)
chla <- proc_wq_data(chl_a_mgL)
turb <- proc_wq_data(turbidity)
temp <- proc_wq_data(water_temp)
