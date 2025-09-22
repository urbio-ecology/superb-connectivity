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

plot(barrier)

# load habitat - here all understorey from the LiDAR data
# also clean up the edges, this helps remove some of the resolution
# that is at a very high level of details which we do not need
habitat <- read_geometry(here("data/superbHab.shp")) |> clean()

plot(habitat)

plot(barrier, border = "orange2")
plot(habitat, border = "midnightblue", add = TRUE)

# buffer the habitat by distance
buffer <- habitat_buffer(habitat, distance = 250)

plot(buffer)

# create fragmentation geometry
fragment <- fragment_geometry(buffer, barrier = barrier)

plot(fragment$fg)

# remove all habitat under barriers
remaining_habitat <- remaining_patches(habitat, barrier = barrier)

plot(habitat)
plot(remaining_habitat)
# identify remaining habitat patches according to which connected area they
# belong to
id_remaining_habitat <- identify_patches(remaining_habitat, fragment)
# id_remaining_habitat is a key output that shows you the connected areas in a
# landscape

plot(id_remaining_habitat)

## Remaining code calculates several metrics of connectivity
# calculate area of each habitat patch
area_hectares <- patch_area(id_remaining_habitat)
# group the patches by connected area ID
connect_habitat <- group_connect_areas(area_hectares)

# calculation
connect_value <- calc_connectivity(connect_habitat)
mean <- calc_mean_size(connect_habitat)
num <- calc_num_areas(connect_habitat)
tot <- calc_total(connect_habitat)
prob_connectedness <- calc_prob_connect(connect_habitat)
results <- tibble(prob_connectedness, connect_value, num, mean, tot)

results
