library(tictoc)
habitat_file <- here("data/wood-bird/WoodbirdAllHabitat.shp")

tic()
spatial_data <- habitat_file |>
  sf::st_read() |>
  # extract just the spatial portion of the file
  sf::st_geometry()
toc()

tic()
buffered <- sf::st_buffer(spatial_data, dist = 0, nQuadSegs = 5)
toc()

sgeos <- geos::as_geos_geometry(spatial_data)

wk <- wk::as_wkb(spatial_data)
tic()
sgeos_valid <- geos::geos_make_valid(sgeos)
toc()
# union to create one large polygon rather than multiple small ones

# 33 seconds
tic()
unioned <- sf::st_union(buffered, by_feature = FALSE)
toc()
# simplify to remove some vertices

tic()
sgeos_un <- geos::geos_make_collection(sgeos_valid) |> geos::geos_unary_union()
toc()

tic()
simplified <- sf::st_simplify(unioned, dTolerance = 1)
toc()
# 23 seconds

simplified

barrier <- read_geometry(barrier_file) |> st_as_sf() |> tar_target()
habitat <- read_geometry(habitat_file) |>
  clean() |>
  st_as_sf() |>
  tar_target()
