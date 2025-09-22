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
barrier <- read_geometry(here(
  "R/example-workflow-superb-fairy-wren/allSFWRoads.shp"
))

# load habitat - here all understorey from the LiDAR data
habitat <- read_geometry(here(
  "R/example-workflow-superb-fairy-wren/superbHab.shp"
))
habitat <- clean(habitat)

# buffer the habitat by distance
buff <- habitat_buffer(habitat = habitat, distance = 250)

# create fragmentation geometry
frag <- fragment_geometry(buff = buff, barrier = barrier)
# remove all habitat under barriers
remaining_habitat <- remaining_patches(habitat = habitat, barrier = barrier)
# identify remaining habitat patches according to which connected area they
# belong to
id_remaining_habitat <- identify_patches(remaining_habitat, frag)
# id_remaining_habitat is a key output that shows you the connected areas in a
# landscape

## Remaining code calculates several metrics of connectivity
# calculate area of each habitat patch
area_hectares <- patch_area(id_remaining_habitat)
# group the patches by connected area ID
connect_habitat <- group_connect_areas(area_hectares)

# connectivity calculation
area_hectares <- patch_area(id_remaining_habitat)
# group the patches by connected area ID
connect_habitat <- group_connect_areas(area_hectares)
# write csv output

# calculation
connect_value <- calc_connectivity(connect_habitat)
mean <- calc_mean_size(connect_habitat)
num <- calc_num_areas(connect_habitat)
tot <- calc_total(connect_habitat)
prob_connectedness <- calc_prob_connect(connect_habitat)
results <- tibble(prob_connectedness, connect_value, num, mean, tot)

results
