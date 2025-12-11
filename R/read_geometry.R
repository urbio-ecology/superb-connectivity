#' Read shapefile geometry
#'
#' Reads a shapefile and extracts only the spatial geometry, discarding
#' attribute data.
#'
#' @param shapefile Character. File path to a shapefile or an SF object.
#'
#' @returns An `sfc` object containing only the spatial geometry.
#'
#' @examples
#' \dontrun{
#' # Read geometry from shapefile
#' habitat_geom <- read_geometry("data/habitat.shp")
#'
#' # Can also pass an existing SF object
#' habitat_sf <- sf::st_read("data/habitat.shp")
#' habitat_geom <- read_geometry(habitat_sf)
#' }
#'
#' @export
read_geometry <- function(shapefile) {
  shapefile |>
    # read in shape file from file name or file path
    sf::st_read() |>
    # extract just the spatial portion of the file
    sf::st_geometry()
}
