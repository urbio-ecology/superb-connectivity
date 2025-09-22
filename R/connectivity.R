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
