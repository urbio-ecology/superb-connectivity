#' Lizard Habitat Data
#'
#' Read an example raster (tif) of lizard habitat data in Australia using
#' [terra::rast].
#'
#' @keywords datasets
#' @examples
#' lizard_habitat <- example_habitat()
#' image(lizard_habitat)
#' @export
example_habitat <- function() {
  habitat_file <- system.file(
    "ex/lizard_habitat_raster.tif",
    package = "urbioconnect"
  )
  lizard_habitat <- terra::rast(habitat_file)
  lizard_habitat
}

#' Lizard Barrier Data (shapefile)
#'
#' Read a shapefile of lizard habitat data in Australia as an SF object
#'
#' @keywords datasets
#' @examples
#' lizard_barrier_shp <- example_barrier_shp()
#' plot(lizard_barrier_shp)
#' @export
example_barrier_shp <- function() {
  barrier_file <- system.file("ex/lizard_barrier.shp", package = "urbioconnect")
  lizard_barrier_shp <- read_geometry(
    barrier_file
  ) |>
    sf::st_as_sf()

  lizard_barrier_shp
}

#' Lizard Barrier Data (raster)
#'
#' Read a raster of lizard barrier data in Australia
#'
#' @keywords datasets
#' @examples
#' library(terra)
#' lizard_barrier <- example_barrier()
#' plot(lizard_barrier)
#' @export
example_barrier <- function() {
  barrier_file <- system.file(
    "ex/lizard_barrier_raster.tif",
    package = "urbioconnect"
  )
  lizard_barrier <- terra::rast(barrier_file)
  lizard_barrier
}
