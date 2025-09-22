# clean any spatial data layer (shape file)
# many shape files contain errors, places where the edges of a polygon cross
# over or polygons which overlap. This helps remove some of those errors by
# smoothing the edges of polygons, removing corners, and dissolving
# edges where polygons overlap. This reduces the complexity of the shape file,
# making future steps quicker
clean <- function(spatial_data, ...) {
  # buffer by a small amount
  buffered <- sf::st_buffer(spatial_data, dist = 0, nQuadSegs = 5)
  # union to create one large polygon rather than multiple small ones
  unioned <- sf::st_union(buffered, by_feature = FALSE)
  # simplify to remove some vertices
  simplified <- sf::st_simplify(unioned, dTolerance = 1)
  simplified
}

# clean membership to account for when some fragments appear in no buffered
# areas, or some appear in more than one.
clean_inter <- function(membership_vector) {
  # check for empty vectors
  if (length(membership_vector) == 0) {
    membership_vector <- NA
  }

  if (length(membership_vector) > 1) {
    membership_vector <- membership_vector[1]
    warning(
      "One of the fragments occurs in more than one buffered area, ",
      "so has been assigned to the first area in the list"
    )
  }
  membership_vector
}
