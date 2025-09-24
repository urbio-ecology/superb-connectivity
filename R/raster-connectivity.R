# RASTERISE LAYERS
# make the habitat file into a sf object, not sfc
# create an empty raster grid of the correct dimensions, resolution
# set the CRS to the same as the habitat layer
empty_grid <- function(habitat, resolution = 10) {
  grid <- raster::raster(
    x = habitat,
    res = resolution,
    crs = crs(habitat)
  )
  grid
}

# buffer the habitat by half the threshold distance (the distance past
# which habitat patches are no longer considered connected)
rast_habitat_buffer <- function(habitat, distance) {
  buffer_window <- focalWeight(
    x = habitat,
    d = distance,
    type = "circle"
  )
  buffer_window <- buffer_window / max(buffer_window)
  buffered_habitat <- focal(
    x = habitat,
    w = buffer_window,
    fun = max,
    na.rm = TRUE
  ) # the long bit
  buffered_habitat[buffered_habitat != 1] <- NA
  buffered_habitat
}

create_barrier_mask <- function(barrier) {
  # convert barrier layer (1s and NAs) to a multiplier (NA where barrier is)
  barrier_multiplier <- barrier
  barrier_multiplier[is.na(barrier_multiplier)] <- 0
  barrier_multiplier[barrier_multiplier == 1] <- NA
  barrier_multiplier <- barrier_multiplier + 1
  barrier_multiplier
}

rast_remove_habitat_under_barrier <- function(habitat, barrier_mask) {
  # mask out the barrier bits from habitat_raster
  habitat_no_barriers <- mask(habitat, barrier_mask)
  habitat_no_barriers
}

rast_fragment_habitat <- function(buffered_habitat, barrier) {
  buffered_habitat * barrier_mask
}

rast_assign_patches_to_fragments <- function(remaining_habitat, fragment) {
  # get IDs of connected areas
  patch_id_raster <- clump(fragment)
  # intersect with habitat to get area IDs of habitat patches
  patch_id_raster <- remaining_habitat * patch_id_raster
  patch_id_raster
}

rast_add_patch_area <- function(raster) {
  raster_with_area <- raster::addLayer(raster, area(raster))
  names(raster_with_area) <- c("patch_id", "area") # Name both layers
  raster_with_area
}


rast_aggregate_connected_patches <- function(raster) {
  ## This code is to do with finding the actual connectivity calculation
  # FIND PATCH AREAS
  summed <- tibble(
    patch_id = getValues(raster$patch_id),
    area = getValues(raster$area)
  ) %>%
    filter(
      !is.na(patch_id)
    ) %>%
    group_by(
      patch_id
    ) %>%
    summarise(
      area = sum(area)
    ) |>
    mutate(area_squared = area^2)
  summed
}

rast_habitat_connectivity <- function(habitat, barrier, distance) {
  barrier_mask <- create_barrier_mask(barrier = barrier)

  remaining_habitat <- rast_remove_habitat_under_barrier(
    habitat = habitat,
    barrier_mask = barrier_mask
  )

  # buffer by radius (metres)
  buffered_habitat <- rast_habitat_buffer(
    habitat = remaining_habitat,
    distance = distance
  )

  # apply barriers to get the fragmentation
  fragmentation_raster <- rast_fragment_habitat(
    buffered_habitat,
    barrier_mask
  )

  # get IDs of connected areas
  # intersect with habitat to get area IDs of habitat patches
  patch_id_raster <- rast_assign_patches_to_fragments(
    remaining_habitat = remaining_habitat,
    fragment = fragmentation_raster
  ) |>
    rast_add_patch_area()

  rast_areas_connected <- rast_aggregate_connected_patches(patch_id_raster)

  rast_areas_connected
}
