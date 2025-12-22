library(terra)
library(tidyverse)
library(tidyterra)
library(sf)

ex_hab <- example_habitat()
ex_bar <- example_barrier()

# looks like these aren't quite accurate?
terra::crs(ex_hab, describe = TRUE)
terra::crs(ex_bar, describe = TRUE)
all.equal(terra::crs(ex_hab), terra::crs(ex_bar))

terra::crs(ex_hab) <- sf::st_crs(28355)$wkt
terra::crs(ex_bar) <- sf::st_crs(28355)$wkt

terra::crs(ex_hab, describe = TRUE)
terra::crs(ex_bar, describe = TRUE)

terra::res(ex_bar)
terra::ext(ex_bar)

ex_bar |> create_barrier_mask()

debugonce(habitat_connectivity)
habitat_connectivity(
  habitat = ex_hab,
  barrier = ex_bar,
  distance = 50
)
