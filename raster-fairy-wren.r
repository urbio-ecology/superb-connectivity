# baseline total habitat areas needed for comparison (in m2 not Ha)
# e.g. effMesh <- sum(df$areaSquared) / existingHabitatArea
# echidna = 115421600 500m buffer

source("packages.R")
dir_map(path = "R/", fun = source)
barrier <- read_geometry(here("data/allSFWRoads.shp")) |> st_as_sf()

habitat <- read_geometry(here("data/superbHab.shp")) |> clean() |> st_as_sf()

prepared_rasters <- prepare_rasters(
  habitat = habitat,
  barrier = barrier,
  base_resolution = 10,
  overlay_resolution = 500
)

habitat_raster <- prepared_rasters$habitat_raster
barrier_raster <- prepared_rasters$barrier_raster

plot(habitat_raster)
plot(barrier_raster)

barrier_mask <- create_barrier_mask(barrier = barrier_raster)

plot(barrier_mask)

remaining_habitat <- rast_remove_habitat_under_barrier(
  habitat = habitat_raster,
  barrier_mask = barrier_mask
)

# buffer by radius (metres)
buffered_habitat <- rast_habitat_buffer(
  habitat = remaining_habitat,
  distance = 250
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
## This code is to do with finding the actual connectivity calculation

summarise_connectivity(
  area_squared = rast_areas_connected$area_squared,
  area_total = rast_areas_connected$area
)

# and as one step
rast_areas_connected2 <- rast_habitat_connectivity(
  habitat = habitat_raster2,
  barrier = barrier_raster2,
  distance = 250
)

summarise_connectivity(
  area_squared = rast_areas_connected$area_squared,
  area_total = rast_areas_connected$area
)
