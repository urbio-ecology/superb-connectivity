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

# TODO work out if there is a way to separate this out into two steps?
prepare_rasters <- function(
  habitat,
  barrier,
  base_resolution = 10,
  overlay_resolution = 500
) {
  aggregation_factor <- overlay_resolution / base_resolution

  grid <- empty_grid(habitat, resolution = base_resolution)

  habitat_raster <- fasterize(habitat, raster = grid, background = NA)
  barrier_raster <- fasterize(barrier, raster = grid, background = 0)

  # aggregate rasters to make them the size of the overlay raster
  ## could generate an empty raster with this resolution instead of using
  ## barrier data directly - we could instead do raster of this grid spec
  coarse_raster <- aggregate(barrier_raster * 0, aggregation_factor)
  ## making the grid finer
  coarse_template <- disaggregate(coarse_raster, aggregation_factor)
  ## terra resample or project could help us get to these different resolutions
  ## terra resample by util

  ## again, we could get around this by specifying a grid spec
  ## make sure the extent snaps to the right shape
  ## so give it the extent and resolution
  ## with the extent and the resolution, make sure that they snap together
  ## terra::rast(nrow = ..., ncol = ..., extent)
  habitat_raster_final <- extend(habitat_raster, coarse_template)
  barrier_raster_final <- extend(barrier_raster, coarse_template)

  list(
    habitat_raster = habitat_raster_final,
    barrier_raster = barrier_raster_final
  )
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

rast_fragment_habitat <- function(buffered_habitat, barrier_mask) {
  buffered_habitat * barrier_mask
}

rast_assign_patches_to_fragments <- function(remaining_habitat, fragment) {
  # get IDs of connected areas
  ## terra::patches
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

rast_habitat_connectivity <- function(
  habitat,
  barrier,
  distance,
  verbose = TRUE
) {
  if (verbose) {
    res <- .rast_habitat_connectivity(habitat, barrier, distance)
  } else {
    quiet_rast_habitat_connectivity <- purrr::quietly(
      .rast_habitat_connectivity
    )
    res <- quiet_rast_habitat_connectivity(habitat, barrier, distance)
  }
}

.rast_habitat_connectivity <- function(habitat, barrier, distance) {
  cli::cli_progress_step("Creating barrier mask")
  barrier_mask <- create_barrier_mask(barrier = barrier)

  cli::cli_progress_step("Removing habitat underneath barrier")
  remaining_habitat <- rast_remove_habitat_under_barrier(
    habitat = habitat,
    barrier_mask = barrier_mask
  )

  cli::cli_progress_step("Adding buffer of {distance}m to habitat layer")
  # buffer by radius (metres)
  buffered_habitat <- rast_habitat_buffer(
    habitat = remaining_habitat,
    distance = distance
  )

  cli::cli_progress_step("Fragmenting habitat layer along barrier intersection")
  # apply barriers to get the fragmentation
  fragmentation_raster <- rast_fragment_habitat(
    buffered_habitat,
    barrier_mask
  )

  # get IDs of connected areas
  # intersect with habitat to get area IDs of habitat patches
  cli::cli_progress_step("Assigning patches ID to fragments")
  patch_id_raster <- rast_assign_patches_to_fragments(
    remaining_habitat = remaining_habitat,
    fragment = fragmentation_raster
  ) |>
    rast_add_patch_area()

  cli::cli_progress_step("Summarising area in each patch")
  rast_areas_connected <- rast_aggregate_connected_patches(patch_id_raster)
  rast_areas_connected
}


# loo helpers

rast_remove_habitat_cell <- function(
  habitat_raster,
  i,
  coarse_raster,
  aggregation_factor
) {
  deleted_raster <- coarse_raster + 1
  deleted_raster[i] <- NA
  deleted_raster_hi_res <- disaggregate(deleted_raster, aggregation_factor)
  # create the new habitat with a bit deleted
  loo_habitat <- habitat_raster * deleted_raster_hi_res
  loo_habitat
}
