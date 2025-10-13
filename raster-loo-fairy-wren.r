source("packages.R")
dir_map(path = "R/", fun = source)
barrier <- read_geometry(here("data/allSFWRoads.shp")) |> st_as_sf()

habitat <- read_geometry(here("data/superbHab.shp")) |> clean() |> st_as_sf()

# Calculate connectivity to then use later in the LOO method
rast_areas_connected <- rast_habitat_connectivity(
  habitat = habitat_raster,
  barrier = barrier_raster,
  distance = 250
)

connectivity_summary <- summarise_connectivity(
  area_squared = rast_areas_connected$area_squared,
  area_total = rast_areas_connected$area
)

# this is used later
total_patch_area_hectares <- connectivity_summary$patch_area_total_ha

base_resolution <- 10
overlay_resolution <- 500
prepared_rasters <- prepare_rasters(
  habitat = habitat,
  barrier = barrier,
  base_resolution = base_resolution,
  overlay_resolution = overlay_resolution
)

habitat_raster <- prepared_rasters$habitat_raster
barrier_raster <- prepared_rasters$barrier_raster

# Leave One Out ----

aggregation_factor <- overlay_resolution / base_resolution

# aggregate rasters to make them the size of the overlay raster
coarse_raster <- aggregate(barrier_raster * 0, aggregation_factor)

## How to prioritise which parts are the most important
# This forms a key ideal output of the application/product
# MAIN PRIORITISATION ANALYSIS
# This code loops through the habitat raster, removing one pixel at at time
# and then recalculates effective mesh size. The difference in the effective
# mesh size value is calculated and stored in a new raster, which records the
# change in connectivity when the habitat in that pixel is removed.
# make a copy of the coarse overlay raster to store effective mesh size
connectivity <- coarse_raster
# make a copy to store the CHANGE in connectivity
changeConnect <- coarse_raster

barrier_mask <- create_barrier_mask(barrier = barrier_raster)

# loop through coarse raster cells, running the connectivity thingo every time
# store the connect value in one raster and then
# for (i in seq_len(ncell(coarse_raster))) {
for (i in seq_len(5)) {
  i <- 1
  loo_habitat <- rast_remove_habitat_cell(
    habitat_raster,
    i,
    coarse_raster,
    aggregation_factor
  )

  # mask out the barrier bits from habitat_raster
  loo_remaining_habitat <- rast_remove_habitat_under_barrier(
    habitat = loo_habitat,
    barrier_mask = barrier_mask
  )

  loo_buffered_habitat <- rast_habitat_buffer(
    habitat = loo_remaining_habitat,
    distance = 250
  )

  # apply barriers to get the fragmentation
  loo_fragmentation_raster <- rast_fragment_habitat(
    loo_buffered_habitat,
    barrier_mask
  )

  # get IDs of connected areas
  # intersect with habitat to get area IDs of habitat patches
  loo_patch_id_raster <- rast_assign_patches_to_fragments(
    remaining_habitat = loo_remaining_habitat,
    fragment = loo_fragmentation_raster
  ) |>
    rast_add_patch_area()

  loo_rast_areas_connected <- rast_aggregate_connected_patches(
    loo_patch_id_raster
  )

  # Calculate connectivity ----
  # A lot of these calculations are already created in summaries
  ## total_habitat_area() is what was used to calculate `tot` for the
  ## original data (as dsicussed in #13)
  loo_effective_mesh_ha <- effective_mesh_size(
    area_squared = loo_rast_areas_connected$area_squared,
    # original area measurement, which will get summed
    area_total = rast_areas_connected$area
  )

  ## TODO this number seems wrong, slash, off by an order of magnitude
  # store in rasters
  connectivity[i] <- loo_effective_mesh_ha
  # difference between effective mesh calculated above
  changeConnect[i] <- connectivity_summary$effective_mesh_ha -
    loo_effective_mesh_ha
}

# TODO
# output the connectivity rasters
# Save as .tif using GTiff ? into outfolder
# Explore
# effMesh, effMeshHa, mean_size, numAreas, tot, totHa, probConnect
