#' Buffer habitat by distance
#'
#' Creates a buffer around habitat polygons and unions overlapping areas into
#' a single polygon.
#'
#' @param habitat SF object. Habitat spatial data.
#' @param distance Numeric. Buffer distance in meters.
#'
#' @returns SF object with buffered and unioned habitat geometry.
#' @export
sf_habitat_buffer <- function(habitat, distance) {
  # buffer by the required distance
  habitat_buffer <- sf::st_buffer(x = habitat, dist = distance, nQuadSegs = 5)
  # union creates one large polygon rather than multiple small ones
  habitat_union <- sf::st_union(habitat_buffer, by_feature = FALSE)
  habitat_union
}

#' Fragment habitat along barriers
#'
#' Removes barrier areas from buffered habitat and splits the result into
#' individual polygon fragments.
#'
#' @param habitat_buffered SF object. Buffered habitat geometry.
#' @param barrier SF object. Barrier geometry (e.g., roads).
#'
#' @returns SF object with individual habitat fragments, each with a unique ID.
#' @export
sf_fragment_habitat <- function(habitat_buffered, barrier) {
  # Remove road polygon areas from buffered habitat polygon, creating gaps
  habitat_buffered_no_roads <- sf::st_difference(habitat_buffered, barrier)
  # creates individual polygons, rather than one mega polygon
  fragmented_geometry <- habitat_buffered_no_roads |>
    sf::st_cast("POLYGON") |>
    sf::st_sf(fg = _) |>
    # sequentially number the ID
    tibble::rowid_to_column(var = "id")

  fragmented_geometry
}

#' Remove habitat underneath barriers
#'
#' Removes all habitat areas that intersect with barriers and splits
#' multipolygons into individual patches.
#'
#' @param habitat SF object. Original habitat geometry.
#' @param barrier SF object. Barrier geometry.
#'
#' @returns SF object with habitat patches that don't intersect barriers.
#' @export
sf_remove_habitat_under_barrier <- function(habitat, barrier) {
  # remove all habitat under barriers
  habitat_no_barriers <- sf::st_difference(habitat, barrier)
  # split multipolygon into the original number of separate polygons
  remaining_patches <- sf::st_cast(habitat_no_barriers, "POLYGON")
  remaining_patches
}

#' Assign habitat patches to fragment IDs
#'
#' Determines which connected fragment each remaining habitat patch belongs to
#' based on spatial intersection.
#'
#' @param remaining SF object. Remaining habitat patches after barrier removal.
#' @param fragment_id SF object. Fragment geometries with IDs.
#'
#' @returns SF object with habitat patches labeled by their fragment ID.
#' @export
sf_assign_patches_to_fragments <- function(remaining, fragment_id) {
  intersects <- sf::st_intersects(remaining, fragment_id)
  membership <- sapply(intersects, dplyr::first)
  habitat_id <- sf::st_sf(geometry = remaining) |>
    dplyr::mutate(patch_id = membership)
  habitat_id
}

#' Add patch area column
#'
#' @param patches SF object. Habitat patches.
#'
#' @returns SF object with added `area` column in square meters.
#' @export
sf_add_patch_area <- function(patches) {
  patches |>
    dplyr::mutate(area = sf::st_area(geometry))
}
#' Aggregate connected patch areas
#'
#' Groups habitat patches by their connected fragment ID and calculates total
#' and squared areas for connectivity metrics.
#'
#' @param patch_areas SF object. Habitat patches with area column.
#'
#' @returns Data frame with `patch_id`, `area_total`, and `area_squared`
#'   columns.
#' @export
#' @export
sf_aggregate_connected_patches <- function(patch_areas) {
  summed <- patch_areas |>
    sf::st_drop_geometry() |>
    dplyr::group_by(patch_id) |>
    dplyr::summarise(area_total = sum(area)) |>
    dplyr::mutate(area_squared = area_total^2)
  summed
}

#' Calculate habitat connectivity
#'
#' Performs complete habitat connectivity analysis using vector-based spatial
#' operations. Buffers habitat, fragments it along barriers, and calculates
#' areas of connected patches.
#'
#' @param habitat SF object. Original habitat spatial data.
#' @param barrier SF object. Barrier spatial data (e.g., roads, waterways).
#' @param distance Numeric. Threshold distance in meters for connectivity.
#'   Habitat patches within this distance are considered connected.
#'
#' @returns Data frame with connectivity metrics for each connected patch,
#'   including `patch_id`, `area_total`, and `area_squared`.
#'
#' @examples
#' \dontrun{
#' # Load habitat and barrier data
#' habitat <- sf::st_read("habitat.shp")
#' roads <- sf::st_read("roads.shp")
#'
#' # Calculate connectivity at 100m threshold
#' connectivity <- habitat_connectivity(habitat, roads, distance = 100)
#' }
#' @export
sf_habitat_connectivity <- function(habitat, barrier, distance) {
  # buffer the habitat layer by the distance
  buffer <- sf_habitat_buffer(habitat, distance)
  # create fragmentation geometry
  fragment <- sf_fragment_habitat(buffer, barrier)
  # remove all habitat under barriers
  habitat_remaining <- sf_remove_habitat_under_barrier(habitat, barrier)
  # identify remaining habitat patches according to their connected area
  habitat_remaining_id <- sf_assign_patches_to_fragments(
    habitat_remaining,
    fragment
  ) |>
    # calculate area of each habitat patch
    sf_add_patch_area()
  # group the patches by connected area ID
  areas_connected <- sf_aggregate_connected_patches(habitat_remaining_id)
  areas_connected
}
