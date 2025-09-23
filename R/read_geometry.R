# This replaces load_bar and load_hab as these are the same function
# extract just the spatial information from a shape file
read_geometry <- function(shapefile) {
  shapefile |>
    # read in shape file from file name or file path
    sf::st_read() |>
    # extract just the spatial portion of the file
    sf::st_geometry()
}
