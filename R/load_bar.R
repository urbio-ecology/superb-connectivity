# function to load the barrier shape file
# This function uses the SF package to extract just the spatial information from a shape file
load_bar <- function(barrierFile, ...) {
  barrier <- sf::st_read(barrierFile) # read in shape file from file name or file path
  barrier <- sf::st_geometry(barrier) # extract just the spatial portion of the file
  barrier
}
