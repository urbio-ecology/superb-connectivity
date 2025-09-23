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
