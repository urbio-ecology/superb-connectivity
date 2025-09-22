# buffer the habitat by half the threshold distance (the distance past
# which habitat patches are no longer considered connected)
habitat_buffer <- function(habitat, distance) {
  # buffer by the required distance
  habitat_buffer <- sf::st_buffer(habitat, dist = distance, nQuadSegs = 5)
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
    sf::st_sf(fg = _)

  # sequentially number the ID
  fragmented_geometry$ID <- seq.int(nrow(fragmented_geometry))
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

# identify the remaining original habitat patches belong in which connected area
identify_patches <- function(remaining, fragment_id) {
  inter <- sf::st_intersects(remaining, fragment_id)
  # code to sanitise "inter"  (a list of vectors) to make sure any empty vectors are non-empty
  # and make sure there are no vectors longer that 1
  cleaned_inter <- lapply(inter, clean_inter)
  membership <- unlist(cleaned_inter)
  habitat_id <- sf::st_sf(geometry = remaining, cluster = membership)
  habitat_id
}

# calculate area of each habitat patch
patch_area <- function(patches) {
  patches$area <- sf::st_area(patches)
  areas <- data.frame(patches$cluster, patches$area)
  # rename columns
  names(areas)[1] <- "patch_id"
  names(areas)[2] <- "area"
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
# d = distance used as a threshold for whether habitat patches are joined or not.
connectivity <- function(habitat, barrier, distance) {
  # buffer the habitat layer by the distance
  buffer <- habitat_buffer(habitat, distance)
  # create fragmentation geometry
  fragment <- fragment_geometry(buffer, barrier)
  # clean the original habitat layer up
  habitat_simplified <- clean(habitat)
  # remove all habitat under barriers
  habitat_remaining <- remaining_patches(habitat_simplified, barrier)
  # identify the remaining habitat patches according to which connected area they belong to
  habitat_remaining_id <- identify_patches(habitat_remaining, fragment)
  # calculate area of each habitat patch
  habitat_area <- patch_area(habitat_remaining_id)
  # group the patches by connected area ID
  areas_connected <- group_connect_areas(habitat_area)
  areas_connected
}
