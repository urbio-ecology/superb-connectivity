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
#' # Read geometry from a shapefile path
#' barrier_path <- system.file("ex/lizard_barrier.shp", package = "urbioconnect")
#' barrier_geom <- read_geometry(barrier_path)
#'
#' @export
read_geometry <- function(shapefile) {
  shapefile |>
    # read in shape file from file name or file path
    sf::st_read() |>
    # extract just the spatial portion of the file
    sf::st_geometry()
}
