# This replaces load_bar and load_hab as these are the same function
read_geometry <- function(shapefile) {
  shapefile |>
    sf::st_read() |>
    sf::st_geometry()
}

# keeping them for ease of future use
# function to load the barrier shape file
# This function uses the SF package to extract just the spatial information from a shape file
load_bar <- function(barrierFile, ...) {
  barrier <- sf::st_read(barrierFile) # read in shape file from file name or file path
  barrier <- sf::st_geometry(barrier) # extract just the spatial portion of the file
  barrier
}

# function to load the habitat shape file
# This function uses the SF package to extract just the spatial information from a shape file
load_hab <- function(habitatFile, ...) {
  habitat <- sf::st_read(habitatFile) # read in shape file from file name or file path
  habitat <- sf::st_geometry(habitat) # extract just the spatial portion of the file.
  habitat
}
