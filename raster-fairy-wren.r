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

resolution <- 10
# working out resolution of overlay grid
# base resolution
small <- 10
# resolution of overlay grid
big <- 500
aggregation_factor <- big / small

# RASTERISE LAYERS
# make the habitat file into a sf object, not sfc
# create an empty raster grid of the correct dimensions, resolution
# set the CRS to the same as the habitat layer
grid <- raster(habitat, res = resolution, crs = crs(habitat))
# rasterise the habitat layer
habitat_raster <- fasterize(habitat, raster = grid, background = NA)
# rasterise barrier
barrier_raster <- fasterize(barrier, raster = grid, background = 0)

# aggregate rasters to make them the size of the overlay raster
coarse_raster <- aggregate(barrier_raster * 0, aggregation_factor)
coarse_template <- disaggregate(coarse_raster, aggregation_factor)
# create the new habitat
habitat_raster2 <- extend(habitat_raster, coarse_template)
barrier_raster2 <- extend(barrier_raster, coarse_template)

# convert barrier layer (1s and NAs) to a multiplier (NA where the barrier is)
barrier_multiplier <- barrier_raster2
barrier_multiplier[is.na(barrier_multiplier)] <- 0
barrier_multiplier[barrier_multiplier == 1] <- NA
barrier_multiplier <- barrier_multiplier + 1

# mask out the barrier bits from habitat_raster
habitat_raster2 <- mask(habitat_raster2, barrier_multiplier)

# CONNECTIVITY WORKFLOW
# buffer by radius (metres)
radius <- 250
buffer_window <- focalWeight(x = habitat_raster2, d = radius, type = "circle")
buffer_window <- buffer_window / max(buffer_window)
buffered_habitat <- focal(
  x = habitat_raster2,
  w = buffer_window,
  fun = max,
  na.rm = TRUE
) # the long bit
buffered_habitat[buffered_habitat != 1] <- NA

# apply barriers to get the fragmentation
fragmentation_raster <- buffered_habitat * barrier_multiplier
# get IDs of connected areas
area_id_raster <- clump(fragmentation_raster)
# intersect with habitat to get area IDs of habitat patches
patch_id_raster <- habitat_raster2 * area_id_raster
# write this raster to disk

## This code is to do with finding the actual connectivity calculation
# FIND PATCH AREAS
df <- tibble(
  area_id = getValues(patch_id_raster),
  area = getValues(area(patch_id_raster))
  # area = prod(res(patch_id_raster))
) %>%
  filter(
    !is.na(area_id)
  ) %>%
  group_by(
    area_id
  ) %>%
  summarise(
    area = sum(area)
  ) |>
  mutate(areaSquared = area^2)

# CONNECTIVITY CALCULATION
effMesh <- sum(df$areaSquared) / sum(df$area) #also needed for next bit
# convert to hectares
effMeshHa <- effMesh * 0.0001
#total area
tot <- sum(df$area) # this is needed in the next stage!!
totHa <- tot * 0.0001
# calculate mean size of connected areas
mean_size <- mean(df$area)
# find number of connected areas
numAreas <- length(df$area)
# find probability of connectedness
probConnect <- effMesh / tot

results <- tibble(
  probConnect,
  effMesh,
  effMeshHa,
  numAreas,
  mean_size,
  tot,
  totHa
)

# Leave One Out ----
#
# ## How to prioritise which parts are the most important
# ## This forms a key ideal output of the application/product
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
