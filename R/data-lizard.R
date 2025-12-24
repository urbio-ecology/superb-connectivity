#' Lizard Habitat and Barrier Data from Melbourne.
#' 
#' We provide Habitat and Barrier data on various lizard species (nominally, 
#'   Blue-tongued Lizard). The data was collected from Darebin Creek in 
#'   Melbourne, which runs between Preson and West Heidelberg. For analysis 
#'   purposes, a buffer  travel distance of 200 metres is recommended for 
#'   lizard connectivity  assessments. 
#' 
#' We provide helper functions to load the raster and shapefile data. These
#'   are required due to how the raster and vector data are stored.  These 
#'   functions provide easy access to example raster and shapefile data 
#'   included with the package:
#'   * `example_habitat()` Returns a raster of lizard habitat data.
#'   * `example_barrier_shp()` Returns a shapefile of lizard barrier data as 
#'   an SF object.
#'   * `example_barrier()` Returns a raster of lizard barrier data.
#'
#' @return A terra raster object or sf object depending on the function called
#' @name example-lizard-data
#' @keywords datasets
#' @examples
#' library(terra)
#'
#' # Load habitat raster
#' lizard_habitat <- example_habitat()
#' plot(lizard_habitat)
#'
#' # Load barrier shapefile
#' lizard_barrier_shp <- example_barrier_shp()
#' plot(lizard_barrier_shp)
#'
#' # Load barrier raster
#' lizard_barrier <- example_barrier()
#' plot(lizard_barrier)
NULL

#' @rdname example-lizard-data
#' @export
example_habitat <- function() {
  habitat_file <- system.file(
    "ex/lizard_habitat_raster.tif",
    package = "urbioconnect"
  )
  lizard_habitat <- terra::rast(habitat_file)
  lizard_habitat
}

#' @rdname example-lizard-data
#' @export
example_barrier_shp <- function() {
  barrier_file <- system.file("ex/lizard_barrier.shp", package = "urbioconnect")
  lizard_barrier_shp <- read_geometry(
    barrier_file
  ) |>
    sf::st_as_sf()

  lizard_barrier_shp
}

#' @rdname example-lizard-data
#' @export
example_barrier <- function() {
  barrier_file <- system.file(
    "ex/lizard_barrier_raster.tif",
    package = "urbioconnect"
  )
  lizard_barrier <- terra::rast(barrier_file)
  lizard_barrier
}
