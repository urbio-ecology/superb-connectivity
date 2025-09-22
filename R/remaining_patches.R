# original habitat patches underneath barriers need to be removed
remaining_patches <- function(habitat, barrier) {
  # remove all habitat under barriers
  hab_barrier_remove <- sf::st_difference(habitat, barrier)
  # split multipolygon into the original number of separate polygons
  hab_barrier_remove <- sf::st_cast(hab_barrier_remove, "POLYGON")
  hab_barrier_remove
}
