# RASTERISE LAYERS
# make the habitat file into a sf object, not sfc
# create an empty raster grid of the correct dimensions, resolution
# set the CRS to the same as the habitat layer

#' Create empty terra raster grid
#'
#' @param habitat SF object or terra SpatRaster.
#' @param resolution Numeric. Cell size in meters (default: 10).
#' @returns Terra SpatRaster. Empty raster grid.
#' @export
empty_grid <- function(habitat, resolution = 10) {
  grid <- terra::rast(
    x = habitat,
    res = resolution,
    crs = terra::crs(habitat)
  )
  grid
}

#' Prepare habitat and barrier rasters
#'
#' @param habitat SF object. Habitat spatial data.
#' @param barrier SF object. Barrier spatial data.
#' @param data_resolution Numeric. Fine resolution in meters. Default, 10.
#' @param target_resolution Numeric. Coarse resolution in meters. Default, 500.
#' @returns List with `habitat_raster` and `barrier_raster` elements.
#' @export
prepare_rasters <- function(
  habitat,
  barrier,
  data_resolution = 10,
  target_resolution = 500
) {
  aggregation_factor <- target_resolution / data_resolution

  grid <- empty_grid(habitat, resolution = data_resolution)

  # convert the vector format into a raster
  habitat_raster <- terra::rasterize(habitat, grid, background = NA)
  barrier_raster <- terra::rasterize(barrier, grid, background = 0)

  # aggregate rasters to make them the size of the overlay raster
  coarse_raster <- terra::aggregate(barrier_raster * 0, aggregation_factor)
  coarse_template <- terra::disagg(coarse_raster, aggregation_factor)

  habitat_raster_final <- terra::extend(habitat_raster, coarse_template)
  barrier_raster_final <- terra::extend(barrier_raster, coarse_template)

  list(
    habitat_raster = habitat_raster_final,
    barrier_raster = barrier_raster_final
  )
}


# buffer the habitat by half the threshold distance (the distance past
# which habitat patches are no longer considered connected)

#' Buffer habitat raster
#'
#' @param habitat Terra SpatRaster. Habitat raster.
#' @param distance Numeric. Buffer distance in meters.
#' @returns Terra SpatRaster with buffered habitat.
#' @export
habitat_buffer <- function(habitat, distance) {
  buffer_window <- terra::focalMat(
    x = habitat,
    d = distance,
    type = "circle"
  )
  buffer_window <- buffer_window / max(buffer_window)
  buffered_habitat <- terra::focal(
    x = habitat,
    w = buffer_window,
    fun = max,
    na.rm = TRUE
  )
  buffered_habitat[buffered_habitat != 1] <- NA
  buffered_habitat
}

#' Create barrier mask
#'
#' @param barrier Terra SpatRaster. Barrier layer.
#' @returns Terra SpatRaster. Mask with NA where barriers exist.
#' @export
create_barrier_mask <- function(barrier) {
  # convert barrier layer (1s and NAs) to a multiplier (NA where barrier is)
  barrier_multiplier <- barrier
  barrier_multiplier[is.na(barrier_multiplier)] <- 0
  barrier_multiplier[barrier_multiplier == 1] <- NA
  barrier_multiplier <- barrier_multiplier + 1
  barrier_multiplier
}

#' Remove habitat under barriers
#'
#' @param habitat Terra SpatRaster. Habitat layer.
#' @param barrier_mask Terra SpatRaster. Barrier mask.
#' @returns Terra SpatRaster with habitat remaining after barrier removal.
#' @export
drop_habitat_under_barrier <- function(habitat, barrier_mask) {
  habitat_no_barriers <- terra::mask(habitat, barrier_mask)
  habitat_no_barriers
}

#' Fragment habitat
#'
#' @param buffered_habitat Terra SpatRaster. Buffered habitat.
#' @param barrier_mask Terra SpatRaster. Barrier mask.
#' @returns Terra SpatRaster with fragmented habitat.
#' @export
fragment_habitat <- function(buffered_habitat, barrier_mask) {
  buffered_habitat * barrier_mask
}

#' Assign patches to fragments
#'
#' @param remaining_habitat Terra SpatRaster. Remaining habitat.
#' @param fragment Terra SpatRaster. Fragment geometry.
#' @returns Terra SpatRaster with patch IDs.
#' @export
assign_patches_to_fragments <- function(remaining_habitat, fragment) {
  patch_id_raster <- terra::patches(fragment)
  patch_id_raster <- remaining_habitat * patch_id_raster
  patch_id_raster
}

#' Add patch area layer
#'
#' @param raster Terra SpatRaster. Patch ID raster.
#' @returns Terra SpatRaster with two layers: patch_id and area.
#' @export
add_patch_area <- function(raster) {
  raster_with_area <- c(raster, terra::cellSize(raster))
  names(raster_with_area) <- c("patch_id", "area")
  raster_with_area
}


#' Aggregate connected patch areas
#'
#' @param raster Terra SpatRaster. Raster with patch_id and area layers.
#' @returns Data frame with patch areas and areas squared.
#' @export
aggregate_connected_patches <- function(raster) {
  summed <- tibble::tibble(
    patch_id = as.numeric(terra::values(raster$patch_id)),
    area = as.numeric(terra::values(raster$area))
  ) |>
    dplyr::filter(
      !is.na(patch_id)
    ) |>
    dplyr::group_by(
      patch_id
    ) |>
    dplyr::summarise(
      area = sum(area)
    ) |>
    dplyr::mutate(area_squared = area^2) |>
    dplyr::mutate(dplyr::across(dplyr::starts_with("area"), \(x) round(x, 3)))
  summed
}

#' Calculate habitat connectivity using terra
#'
#' @param habitat Terra SpatRaster. Habitat raster.
#' @param barrier Terra SpatRaster. Barrier raster.
#' @param distance Numeric. Threshold distance in meters.
#' @param verbose Logical. Display progress messages (default: TRUE).
#' @returns Data frame with connectivity metrics per patch.
#' @export
habitat_connectivity <- function(
  habitat,
  barrier,
  distance,
  verbose = TRUE
) {
  if (verbose) {
    habitat_connectivity <- .habitat_connectivity(
      habitat,
      barrier,
      distance
    )
  } else {
    quiet_habitat_connectivity <- purrr::quietly(
      .habitat_connectivity
    )
    habitat_connectivity <- quiet_habitat_connectivity(
      habitat,
      barrier,
      distance
    )
  }
  habitat_connectivity
}

#' @noRd
.habitat_connectivity <- function(habitat, barrier, distance) {
  cli::cli_progress_step("Creating barrier mask")
  barrier_mask <- create_barrier_mask(barrier = barrier)

  cli::cli_progress_step("Removing habitat underneath barrier")
  remaining_habitat <- drop_habitat_under_barrier(
    habitat = habitat,
    barrier_mask = barrier_mask
  )

  cli::cli_progress_step("Adding buffer of {distance}m to habitat layer")
  buffered_habitat <- habitat_buffer(
    habitat = remaining_habitat,
    distance = distance
  )

  cli::cli_progress_step("Fragmenting habitat layer along barrier intersection")
  fragmentation_raster <- fragment_habitat(
    buffered_habitat,
    barrier_mask
  )

  cli::cli_progress_step("Assigning patches ID to fragments")
  patch_id_raster <- assign_patches_to_fragments(
    remaining_habitat = remaining_habitat,
    fragment = fragmentation_raster
  ) |>
    add_patch_area()

  cli::cli_progress_step("Summarising area in each patch")
  areas_connected <- aggregate_connected_patches(patch_id_raster)
  areas_connected
}

#' Calculate habitat connectivity with visualization data
#'
#' @inheritParams habitat_connectivity
#' @returns List with intermediate rasters and connectivity metrics.
#' @export
habitat_connectivity_full <- function(
  habitat,
  barrier,
  distance,
  verbose = TRUE
) {
  if (!verbose) {
    quiet_fun <- purrr::quietly(.habitat_connectivity_full)
    res <- quiet_fun(habitat, barrier, distance)
    return(res$result)
  }

  .habitat_connectivity_full(habitat, barrier, distance)
}

#' @noRd
.habitat_connectivity_full <- function(habitat, barrier, distance) {
  cli::cli_progress_step("Creating barrier mask")
  barrier_mask <- create_barrier_mask(barrier = barrier)

  cli::cli_progress_step("Removing habitat underneath barrier")
  remaining_habitat <- drop_habitat_under_barrier(
    habitat = habitat,
    barrier_mask = barrier_mask
  )

  cli::cli_progress_step("Adding buffer of {distance}m to habitat layer")
  buffered_habitat <- habitat_buffer(
    habitat = remaining_habitat,
    distance = distance
  )

  cli::cli_progress_step("Fragmenting habitat layer along barrier intersection")
  fragmentation_raster <- fragment_habitat(
    buffered_habitat,
    barrier_mask
  )

  cli::cli_progress_step("Assigning patches ID to fragments")
  patch_id_raster <- assign_patches_to_fragments(
    remaining_habitat = remaining_habitat,
    fragment = fragmentation_raster
  ) |>
    add_patch_area()

  cli::cli_progress_step("Summarising area in each patch")
  areas_connected <- aggregate_connected_patches(patch_id_raster)

  # Return all intermediate results
  list(
    buffered_habitat = buffered_habitat,
    patch_id_raster = patch_id_raster,
    areas_connected = areas_connected,
    barrier_mask = barrier_mask,
    remaining_habitat = remaining_habitat
  )
}
