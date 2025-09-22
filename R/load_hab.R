# functions used in the workflow for calculating
# effective mesh size (Connectivity Index)
# function to load the habitat shape file
# This function uses the SF package to extract just the spatial information from a shape file
load_hab <- function(habitatFile, ...) {
  habitat <- sf::st_read(habitatFile) # read in shape file from file name or file path
  habitat <- sf::st_geometry(habitat) # extract just the spatial portion of the file.
  habitat
}
