# baseline total habitat areas needed for comparison (in m2 not Ha)
# e.g. effMesh <- sum(df$areaSquared) / existingHabitatArea
# echidna = 115421600 500m buffer

source("packages.R")
dir_map(path = "R/", fun = source)
# set the outfolder
# load prepped barrier layer
barrier <- read_geometry(here("data/allSFWRoads.shp")) |> st_as_sf()

# load habitat layer
habitat <- read_geometry(here("data/superbHab.shp")) |> clean() |> st_as_sf()

# RASTERISE LAYERS
prepared_rasters <- prepare_rasters(
  habitat = habitat,
  barrier = barrier,
  base_resolution = 10,
  overlay_resolution = 500
)

habitat_raster <- prepared_rasters$habitat_raster
barrier_raster <- prepared_rasters$barrier_raster

barrier_mask <- create_barrier_mask(barrier = barrier_raster)

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

# FIND PATCH AREAS
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

# Leave One Out ----
#
# ## How to prioritise which parts are the most important
## This forms a key ideal output of the application/product
# # MAIN PRIORITISATION ANALYSIS
# # This code loops through the habitat raster, removing one pixel at at time
# # and then recalculates effective mesh size. The difference in the effective
# # mesh size value is calculated and stored in a new raster, which records the
# # change in connectivity when the habitat in that pixel is removed.
# # make a copy of the coarse overlay raster to store effective mesh size
# connectivity <- coarse_raster
# # make a copy to store the CHANGE in connectivity
# changeConnect <- coarse_raster
#
#
# # loop through coarse raster cells, running the connectivity thingo every time
# # store the connect value in one raster and then
# for (i in seq_len(ncell(coarse_raster))) {
#   del <- coarse_raster + 1
#   del[i] <- NA
#   del_hires <- disaggregate(del, aggregation_factor)
#   # create the new habitat with a bit deleted
#   habitat_del <- habitat_raster2 * del_hires
#
#   # mask out the barrier bits from habitat_raster
#   habitat_del <- mask(habitat_del, barrier_multiplier)
#
#   # do all the stuff to the habitatDel file
#   # buffer by radius (metres)
#   buffwindow <- focalWeight(habitat_del, radius, "circle")
#   buffwindow <- buffwindow / max(buffwindow)
#   buffhabitat <- focal(habitat_del, buffwindow, fun = max, na.rm = TRUE)
#   buffhabitat[buffhabitat != 1] <- NA
#
#   # apply barriers to get the fragmentation
#   fragRast <- buffhabitat * barrier_multiplier
#   # get IDs of connected areas
#   areaID <- clump(fragRast)
#   # intersect with habitat to get area IDs of habitat patches
#   patchID <- habitat_del * areaID
#
#   # FIND PATCH AREAS
#   df2 <- tibble(
#     area_id = getValues(patchID),
#     area = getValues(area(patchID))
#     # area = prod(res(patch_id_raster))
#   ) %>%
#     filter(
#       !is.na(area_id)
#     ) %>%
#     group_by(
#       area_id
#     ) %>%
#     summarise(
#       area = sum(area)
#     )
#
# Calculate connectivity ----
#   df2$areaSquared <- df2$area^2
#   effMesh2 <- sum(df2$areaSquared) / tot
#   # store in rasters
#   connectivity[i] <- effMesh2
#   changeConnect[i] <- effMesh - effMesh2
# }
#
#
# TODO
# output the connectivity rasters
# Save as .tif using GTiff ? into outfolder
# Explore
# effMesh, effMeshHa, mean_size, numAreas, tot, totHa, probConnect
