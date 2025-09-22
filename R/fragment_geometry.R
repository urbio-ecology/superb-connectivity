# create the fragmentation geometry
fragment_geometry <- function(bufferedHabitat, barrier) {
  fg <- sf::st_difference(bufferedHabitat, barrier) # difference removes the road polygon areas from the buffered habitat polygon, creating gaps
  fg <- sf::st_cast(fg, "POLYGON") # creates individual polygons, rather than one mega polygon
  fg <- sf::st_sf(fg)
  fg$ID <- seq.int(nrow(fg)) # squentially number the
  fg
}
