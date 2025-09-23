# buffer the habitat by half the threshold distance (the distance past
# which habitat patches are no longer considered connected)
habitat_buffer <- function(habitat, distance) {
  # buffer by the required distance
  habitat_buffer <- sf::st_buffer(
    x = habitat,
    dist = distance,
    nQuadSegs = 5
  )
  # union creates one large polygon rather than multiple small ones
  habitat_union <- sf::st_union(habitat_buffer, by_feature = FALSE)
  habitat_union
}

# create the fragmentation geometry
fragment_geometry <- function(habitat_buffered, barrier) {
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
remaining_patches <- function(habitat, barrier) {
  # remove all habitat under barriers
  habitat_no_barriers <- sf::st_difference(habitat, barrier)
  # split multipolygon into the original number of separate polygons
  remaining_patchs <- sf::st_cast(habitat_no_barriers, "POLYGON")
  remaining_patchs
}

# identify which of the remaining original habitat patches belong in which
# connected area
identify_patches <- function(remaining, fragment_id) {
  intersects <- sf::st_intersects(remaining, fragment_id)
  membership <- sapply(intersects, first)
  habitat_id <- sf::st_sf(geometry = remaining, cluster = membership)
  # TODO check with Holly about removing this area calculation here as we do this in the next step
  # calculate the area of each patch
  # habitat_id$area <- sf::st_area(habitat_id)
  habitat_id
}

# calculate area of each habitat patch
patch_area <- function(patches) {
  patches$area <- sf::st_area(patches)
  areas <- data.frame(
    patch_id = patches$cluster,
    area = patches$area
  )
  areas
}

# function to group the remaining habitat patches by area
group_connect_areas <- function(patch_areas) {
  summed <- patch_areas |>
    dplyr::group_by(patch_id) |>
    dplyr::summarise(area_total = sum(area)) |>
    dplyr::mutate(area_squared = area_total^2)
  summed
}

# function to run the calculate connectivity functions
# x = habitat layer
# y = barrier layer
# d = distance used as threshold for whether habitat patches are joined or not.
connectivity <- function(habitat, barrier, distance) {
  # buffer the habitat layer by the distance
  buffer <- habitat_buffer(habitat, distance)
  # create fragmentation geometry
  fragment <- fragment_geometry(buffer, barrier)
  # remove all habitat under barriers
  habitat_remaining <- remaining_patches(habitat, barrier)
  # identify remaining habitat patches according to their connected area
  habitat_remaining_id <- identify_patches(habitat_remaining, fragment)
  # calculate area of each habitat patch
  habitat_area <- patch_area(habitat_remaining_id)
  # group the patches by connected area ID
  areas_connected <- group_connect_areas(habitat_area)
  areas_connected
}
