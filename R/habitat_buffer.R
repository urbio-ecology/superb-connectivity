# buffer the habitat by half the threshold distance (the distance past
# which habitat patches are no longer considered connected)
habitat_buffer <- function(habitat, distance) {
  # buffer by the required distance
  hb <- sf::st_buffer(habitat, dist = distance, nQuadSegs = 5)
  # union creates one large polygon rather than multiple small ones
  hb <- sf::st_union(hb, by_feature = FALSE)
  hb
}
