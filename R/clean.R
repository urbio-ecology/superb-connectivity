#' @title Clean any spatial data layer (shape file)
#' @description many shape files contain errors, places where the edges of
#'  a polygon cross over or polygons which overlap. This helps remove some of
#' those errors by smoothing the edges of polygons, removing corners, and
#' dissolving edges where polygons overlap. This reduces the complexity of the
#' shape file, making future steps quicker.
#' @export
clean <- function(spatial_data, ...) {
  validated <- sf::st_make_valid(spatial_data)
  # union to create one large polygon rather than multiple small ones
  unioned <- sf::st_union(validated, by_feature = FALSE)
  # simplify to remove some vertices
  simplified <- sf::st_simplify(unioned, dTolerance = 1)
  simplified
}
