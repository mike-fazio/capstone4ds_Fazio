d <- sf::st_distance(oys_proc)

diag(d) <- Inf

nearest <- apply(
  units::drop_units(d),
  1,
  min
)

summary(nearest)


cell_id <- terra::cellFromXY(
  predictors[[1]],
  sf::st_coordinates(oys_proc)
)

sum(duplicated(cell_id))
