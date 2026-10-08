# Load functions and packages from setup script
source(
  here::here(
    "3-scripts",
    "0-setup.r"
  )
)

study_area <- get_study_area()

study_map <-
  tm_basemap("Esri.WorldStreetMap") +
  tm_shape(study_area) +
  tm_polygons(
    fill_alpha = 0,
    col = "red",
    lwd = 2
  ) +
  tm_layout(
    inner.margins = c(0.2, 0.2, 0.2, 0.2)
  ) +
  tm_title("Pensacola Bay System")

tmap_save(
  tm = study_map,
  filename = here::here(
    "4-tables-and-figures",
    "fig-1-study_area.png"
  ),
  width = 8,
  height = 6,
  units = "in",
  dpi = 300
)

study_map
