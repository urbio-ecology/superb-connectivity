# This replaces load_bar and load_hab as these are the same function
read_geometry <- function(shapefile) {
  shapefile |>
    sf::st_read() |>
    sf::st_geometry()
}

# keeping them for ease of future use
# function to load the barrier shape file
# This function uses the SF package to extract just the spatial information from a shape file
load_bar <- function(barrier_file, ...) {
  # read in shape file from file name or file path
  barrier <- sf::st_read(barrier_file)
  # extract just the spatial portion of the file
  barrier <- sf::st_geometry(barrier)
  barrier
}

# function to load the habitat shape file
# This function uses the SF package to extract just the spatial information from a shape file
load_hab <- function(habitat_file, ...) {
  # read in shape file from file name or file path
  habitat <- sf::st_read(habitat_file)
  # extract just the spatial portion of the file.
  habitat <- sf::st_geometry(habitat)
  habitat
}
