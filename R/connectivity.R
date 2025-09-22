# buffer the habitat by half the threshold distance (the distance past
# which habitat patches are no longer considered connected)
habitat_buffer <- function(habitat, distance) {
  # buffer by the required distance
  hb <- sf::st_buffer(habitat, dist = distance, nQuadSegs = 5)
  # union creates one large polygon rather than multiple small ones
  hb <- sf::st_union(hb, by_feature = FALSE)
  hb
}

# create the fragmentation geometry
fragment_geometry <- function(bufferedHabitat, barrier) {
  fg <- sf::st_difference(bufferedHabitat, barrier) # difference removes the road polygon areas from the buffered habitat polygon, creating gaps
  fg <- sf::st_cast(fg, "POLYGON") # creates individual polygons, rather than one mega polygon
  fg <- sf::st_sf(fg)
  fg$ID <- seq.int(nrow(fg)) # squentially number the
  fg
}

# original habitat patches underneath barriers need to be removed
remaining_patches <- function(habitat, barrier) {
  # remove all habitat under barriers
  hab_barrier_remove <- sf::st_difference(habitat, barrier)
  # split multipolygon into the original number of separate polygons
  hab_barrier_remove <- sf::st_cast(hab_barrier_remove, "POLYGON")
  hab_barrier_remove
}

# identify which of the remaining original habitat patches belong in which connected area
identify_patches <- function(remaining, fragID) {
  inter <- sf::st_intersects(remaining, fragID)
  # code to sanitise "inter"  (a list of vectors) to make sure any empty vectors are non-empty
  # and make sure there are no vectors longer that 1
  cleaned_inter <- lapply(inter, clean_inter)
  membership <- unlist(cleaned_inter)
  habID <- sf::st_sf(geometry = remaining, cluster = membership)
  habID
}

# calculate area of each habitat patch
patch_area <- function(patches) {
  patches$area <- sf::st_area(patches)
  areas <- data.frame(cbind(patches$cluster, patches$area))
  # rename columns
  names(areas)[1] <- "patchID"
  names(areas)[2] <- "area"
  areas
}

# function to group the remaining habitat patches by area
group_connect_areas <- function(patchAreas) {
  grouped <- group_by(patchAreas, patchID)
  summed <- summarise(grouped, total_area = sum(area))
  # create a column with  area squared in it
  summed$areaSquared <- summed$total_area^2
  summed
}

# function to run the calculate connectivity functions
# x = habitat layer
# y = barrier layer
# d = distance used as a threshold for whether habitat patches are joined or not.
connectivity <- function(habitat, barrier, distance) {
  # buffer the habitat layer by the distance
  buff <- habitat_buffer(habitat, distance)
  # create fragmentation geometry
  frag <- fragment_geometry(buff, barrier)
  # clean the original habitat layer up
  simpleHab <- clean(habitat)
  # remove all habitat under barriers
  remainingHab <- remaining_patches(simpleHab, barrier)
  # identify the remaining habitat patches according to which connected area they belong to
  IDRemainingHab <- identify_patches(remainingHab, frag)
  # calculate area of each habitat patch
  areaHab <- patch_area(IDRemainingHab)
  # group the patches by connected area ID
  connectAreas <- group_connect_areas(areaHab)
  connectAreas
}
