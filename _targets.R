## Load your packages, e.g. library(targets).
source("packages.R")

## Load your R files
tar_source()

## Assign like regular R, just make sure to pipe into a tar_ operation
tar_assign({
  # baseline total habitat areas needed for comparison (in m2 not Ha)
  # e.g. effMesh <- sum(df$areaSquared) / existingHabitatArea
  # echidna = 115421600 500m buffer

  barrier_file <- tar_file(here("data/allSFWRoads.shp"))
  habitat_file <- tar_file(here("data/superbHab.shp"))
  barrier <- read_geometry(barrier_file) |> st_as_sf() |> tar_target()
  habitat <- read_geometry(habitat_file) |>
    clean() |>
    st_as_sf() |>
    tar_target()

  ### prepare rasters
  overlay_resolution <- tar_target(500)
  base_resolution <- tar_target(10)
  aggregation_factor <- tar_target(overlay_resolution / base_resolution)
  #
  empty_grid <- terra_empty_grid(habitat, resolution = base_resolution) |>
    tar_target()
  # ran into error
  # Error storing output: [writeRaster] there are no cell values
  # TODO lodge bug report for geotargets

  # convert the vector format into a raster
  habitat_rasterised <- terra::rasterize(
    habitat,
    terra::rast(
      x = habitat,
      res = base_resolution,
      crs = crs(habitat)
    ),
    background = NA
  ) |>
    tar_terra_rast()
  barrier_rasterised <- terra::rasterize(
    barrier,
    terra::rast(
      x = habitat,
      res = base_resolution,
      crs = crs(habitat)
    ),
    background = 0
  ) |>
    tar_terra_rast()

  # aggregate rasters to make them the size of the overlay raster
  ## could generate an empty raster with this resolution instead of using
  ## barrier data directly - we could instead do raster of this grid spec
  coarse_raster <- terra::aggregate(
    barrier_rasterised * 0,
    aggregation_factor
  ) |>
    tar_terra_rast()
  ## making the grid finer
  coarse_template <- terra::disagg(coarse_raster, aggregation_factor) |>
    tar_terra_rast()
  ## terra resample or project could help us get to these different resolutions
  ## terra resample by util

  ## again, we could get around this by specifying a grid spec
  ## make sure the extent snaps to the right shape
  ## so give it the extent and resolution
  ## with the extent and the resolution, make sure that they snap together
  ## terra::rast(nrow = ..., ncol = ..., extent)
  habitat_raster <- terra::extend(habitat_rasterised, coarse_template) |>
    tar_terra_rast()
  barrier_raster <- terra::extend(barrier_rasterised, coarse_template) |>
    tar_terra_rast()

  barrier_mask <- create_barrier_mask(barrier = barrier_raster) |>
    tar_terra_rast()

  remaining_habitat <- terra_remove_habitat_under_barrier(
    habitat = habitat_raster,
    barrier_mask = barrier_mask
  ) |>
    tar_terra_rast()

  # buffer by radius (metres)
  buffered_habitat <- terra_habitat_buffer(
    habitat = remaining_habitat,
    distance = 250
  ) |>
    tar_terra_rast()

  # apply barriers to get the fragmentation
  fragmentation_raster <- terra_fragment_habitat(
    buffered_habitat,
    barrier_mask
  ) |>
    tar_terra_rast()
  # get IDs of connected areas
  # intersect with habitat to get area IDs of habitat patches
  patch_id_raster <- terra_assign_patches_to_fragments(
    remaining_habitat = remaining_habitat,
    fragment = fragmentation_raster
  ) |>
    terra_add_patch_area() |>
    tar_terra_rast()

  rast_areas_connected <- terra_aggregate_connected_patches(patch_id_raster) |>
    tar_target()
  ## This code is to do with finding the actual connectivity calculation

  connectivity_wren <- summarise_connectivity(
    area_squared = rast_areas_connected$area_squared,
    area_total = rast_areas_connected$area
  ) |>
    tar_target()

  # and as one step, which is now quite straightforward
  terra_areas_connected2 <- terra_habitat_connectivity(
    habitat = habitat_raster,
    barrier = barrier_raster,
    distance = 250
  ) |>
    tar_target()

  explore_doc <- tar_quarto(path = "doc/explore.qmd")
})
