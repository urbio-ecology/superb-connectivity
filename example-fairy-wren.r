# SFW data prep and analysis
# load data
# prep habitat layer (lidar and shrubs)
# connectivity analysis!
## vector based
## how connected is your landscape?
## provides a map output

source("packages.R")
dir_map(path = "R/", fun = source)
##########
## inputs are: barriers (roads, houses, etc), habitat (grass, trees, water, etc)
## we run that code, get information about movement of species
## outputs are: the connectedness map, and measurements of connectedness
## possible to speed up this code by moving it from vectors to rasters
## (this is done in "rasterAnalysisSFW.R")
## it is possible we might focus on using the raster approach/vector approach
# run existing connectivity calculation for SFW
# load barriers - here all main roads
barrier <- read_geometry(here("data/allSFWRoads.shp"))

plot(barrier, border = "orange2")

# load habitat - here all understorey from the LiDAR data
# also clean up the edges, this helps remove some of the resolution
# that is at a very high level of details which we do not need
habitat <- read_geometry(here("data/superbHab.shp")) |> clean()

plot(habitat, border = "midnightblue", add = TRUE)

# buffer the habitat by distance
buffer <- habitat_buffer(habitat, distance = 250)

plot(habitat, border = "forestgreen")
plot(
  buffer,
  border = "darkgreen",
  add = TRUE,
  col = alpha(colour = "forestgreen", alpha = 0.25)
)

# create fragmentation geometry
fragment <- fragment_geometry(habitat_buffered = buffer, barrier = barrier)

plot(habitat, border = "darkgreen", col = "forestgreen")
plot(
  fragment$fg,
  border = "orange2",
  col = alpha(colour = "midnightblue", alpha = 0.5),
  add = TRUE
)

# remove all habitat under barriers
remaining_habitat <- remove_habitat_under_barrier(habitat, barrier = barrier)

# identify remaining habitat patches according to which connected area they
# belong to
id_remaining_habitat <- identify_connected_patches(remaining_habitat, fragment)
# id_remaining_habitat is a key output that shows you the connected areas in a
# landscape

## Remaining code calculates several metrics of connectivity
# calculate area of each habitat patch
area_hectares <- add_patch_area(id_remaining_habitat)
# group the patches by connected area ID

connect_habitat <- aggregate_connected_patches(area_hectares)

# or, as one step
connect_habitat2 <- connectivity(
  habitat = habitat,
  barrier = barrier,
  distance = 250
)

all.equal(connect_habitat, connect_habitat2)

# calculation
results <- summarise_connectivity(
  area_squared = connect_habitat$area_squared,
  area_total = connect_habitat$area_total
)
results
