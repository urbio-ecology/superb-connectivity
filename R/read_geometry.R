read_geometry <- function(shapefile) {
  shapefile |>
    sf::st_read() |>
    sf::st_geometry()
}
