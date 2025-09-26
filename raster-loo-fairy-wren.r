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
for (i in seq_len(ncell(coarse_raster))) {
  del <- coarse_raster + 1
  del[i] <- NA
  del_hires <- disaggregate(del, aggregation_factor)
  # create the new habitat with a bit deleted
  habitat_del <- habitat_raster * del_hires

  # mask out the barrier bits from habitat_raster
  habitat_del <- rast_remove_habitat_under_barrier(
    habitat = habitat_del,
    barrier_mask = barrier_mask
  )

  # buffer by radius (metres)
  buffhabitat <- rast_habitat_buffer(
    habitat = habitat_del,
    distance = 250
  )

  # apply barriers to get the fragmentation
  fragRast <- rast_fragment_habitat(
    buffhabitat,
    barrier_mask
  )

  # get IDs of connected areas
  # intersect with habitat to get area IDs of habitat patches
  patchID <- rast_assign_patches_to_fragments(
    remaining_habitat = habitat_del,
    fragment = fragRast
  ) |>
    rast_add_patch_area()

  df2 <- rast_aggregate_connected_patches(patchID)

  # Calculate connectivity ----
  # TODO check `tot` calculation - is this done on the whole data set or?
  df2$areaSquared <- df2$area^2
  effMesh2 <- sum(df2$areaSquared) / tot
  # store in rasters
  connectivity[i] <- effMesh2
  changeConnect[i] <- effMesh - effMesh2
}

# TODO
# output the connectivity rasters
# Save as .tif using GTiff ? into outfolder
# Explore
# effMesh, effMeshHa, mean_size, numAreas, tot, totHa, probConnect
