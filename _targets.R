## Load your packages, e.g. library(targets).
source("packages.R")

## Load your R files
tar_source()

# facilitate this working in parallel
controller <- crew_controller_local(
  name = "my_controller",
  workers = 4,
  seconds_idle = 3
)

tar_option_set(
  controller = controller
)

## Assign like regular R, just make sure to pipe into a tar_ operation
tar_assign({
  barrier_file <- tar_file(here(
    "data/wood-bird/AllWoodbirdBarriers_CorrCRS.shp"
  ))
  habitat_file <- tar_file(here("data/wood-bird/WoodbirdAllHabitat.shp"))
  barrier <- read_geometry(barrier_file) |> st_as_sf() |> tar_target()
  habitat <- read_geometry(habitat_file) |>
    clean() |>
    st_as_sf() |>
    tar_target()

  species_name <- tar_target("Woodbird")
  overlay_resolution <- tar_target(500)
  base_resolution <- tar_target(10)
  aggregation_factor <- tar_target(overlay_resolution / base_resolution)
  buffer_distance <- tar_target(c(125, 250))
  # ran into error
  # Error storing output: [writeRaster] there are no cell values
  # TODO lodge bug report for geotargets
  # empty_grid <- terra_empty_grid(habitat, resolution = base_resolution) |>
  #   tar_target()
  # this is now going into `terra::rasterize` - but is there another way?

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

  buffered_habitat <- terra_habitat_buffer(
    habitat = remaining_habitat,
    distance = buffer_distance
  ) |>
    tar_terra_rast(
      pattern = map(buffer_distance)
    )

  # apply barriers to get the fragmentation
  fragmentation_raster <- terra_fragment_habitat(
    buffered_habitat = buffered_habitat,
    barrier_mask = barrier_mask
  ) |>
    tar_terra_rast(
      pattern = map(buffered_habitat)
    )

  # get IDs of connected areas
  # intersect with habitat to get area IDs of habitat patches
  patch_id_raster <- terra_assign_patches_to_fragments(
    remaining_habitat = remaining_habitat,
    fragment = fragmentation_raster
  ) |>
    terra_add_patch_area() |>
    tar_terra_rast(
      pattern = map(fragmentation_raster)
    )

  # or as one step
  terra_areas_connected <- terra_habitat_connectivity(
    habitat = habitat_raster,
    barrier = barrier_raster,
    distance = buffer_distance
  ) |>
    tar_target(
      pattern = map(buffer_distance),
      iteration = "list"
    )

  results_connect_habitat <- summarise_connectivity(
    area_squared = terra_areas_connected$area_squared,
    area_total = terra_areas_connected$area,
    buffer_distance = buffer_distance,
    overlay_resolution = overlay_resolution,
    base_resolution = base_resolution,
    aggregation_factor = aggregation_factor,
    species_name = species_name
  ) |>
    tar_target(
      pattern = map(terra_areas_connected, buffer_distance)
    )

  ### using the vector based approach ------------------------------------------
  ## Not running this for larger file sizes
  ## TODO query this from @mdsumner
  ## how connected is your landscape?
  ## provides a map output
  ## inputs: barriers (roads, houses, etc), habitat (grass, trees, water, etc)
  ## outputs are: the connectedness map, and measurements of connectedness
  ## possible to speed up this code by moving it from vectors to rasters
  ## it is possible we might focus on using the raster approach/vector approach
  # run existing connectivity calculation for SFW

  # habitat - here all understorey from the LiDAR data
  # also clean up the edges, this helps remove some of the resolution
  # that is at a very high level of details which we do not need
  # buffer the habitat by distance
  # buffer <- habitat_buffer(habitat, distance = 250) |> tar_target()
  #
  # vect_areas_connected <- habitat_connectivity(
  #   habitat = habitat,
  #   barrier = barrier,
  #   distance = 250
  # ) |>
  #   tar_target()
  #
  # results_connect_habitat_vect <- summarise_connectivity(
  #   area_squared = vect_areas_connected$area_squared,
  #   area_total = vect_areas_connected$area_total
  # ) |>
  #   tar_target()

  explore_doc <- tar_quarto(path = "doc/explore.qmd")
})
