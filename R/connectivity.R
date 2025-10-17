# buffer the habitat by half the threshold distance (the distance past
# which habitat patches are no longer considered connected)
habitat_buffer <- function(habitat, distance) {
  # buffer by the required distance
  habitat_buffer <- sf::st_buffer(x = habitat, dist = distance, nQuadSegs = 5)
  # union creates one large polygon rather than multiple small ones
  habitat_union <- sf::st_union(habitat_buffer, by_feature = FALSE)
  habitat_union
}

# create the fragmentation geometry
fragment_habitat <- function(habitat_buffered, barrier) {
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

# original habitat patches underneath barriers need to be removed
remove_habitat_under_barrier <- function(habitat, barrier) {
  # remove all habitat under barriers
  habitat_no_barriers <- sf::st_difference(habitat, barrier)
  # split multipolygon into the original number of separate polygons
  remaining_patches <- sf::st_cast(habitat_no_barriers, "POLYGON")
  remaining_patches
}

# identify which of the remaining original habitat patches belong in which
# connected area
assign_patches_to_fragments <- function(remaining, fragment_id) {
  intersects <- sf::st_intersects(remaining, fragment_id)
  membership <- sapply(intersects, first)
  habitat_id <- sf::st_sf(geometry = remaining) |>
    mutate(patch_id = membership)
  habitat_id
}

# calculate area of each habitat patch
add_patch_area <- function(patches) {
  patches |>
    mutate(
      area = sf::st_area(patches)
    )
}

# function to group the remaining habitat patches by area
aggregate_connected_patches <- function(patch_areas) {
  summed <- patch_areas |>
    sf::st_drop_geometry() |>
    dplyr::group_by(patch_id) |>
    dplyr::summarise(area_total = sum(area)) |>
    dplyr::mutate(area_squared = area_total^2)
  summed
}

# function to run the calculate connectivity functions
# x = habitat layer
# y = barrier layer
# d = distance used as threshold for whether habitat patches are joined or not.
habitat_connectivity <- function(habitat, barrier, distance) {
  # buffer the habitat layer by the distance
  buffer <- habitat_buffer(habitat, distance)
  # create fragmentation geometry
  fragment <- fragment_habitat(buffer, barrier)
  # remove all habitat under barriers
  habitat_remaining <- remove_habitat_under_barrier(habitat, barrier)
  # identify remaining habitat patches according to their connected area
  habitat_remaining_id <- assign_patches_to_fragments(
    habitat_remaining,
    fragment
  ) |>
    # calculate area of each habitat patch
    add_patch_area()
  # group the patches by connected area ID
  areas_connected <- aggregate_connected_patches(habitat_remaining_id)
  areas_connected
}
