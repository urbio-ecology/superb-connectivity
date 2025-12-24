library(terra)
library(tidyverse)
library(tidyterra)
library(sf)

ex_hab <- example_habitat()
ex_bar <- example_barrier()

ex_bar |> create_barrier_mask()

debugonce(habitat_connectivity)
habitat_connectivity(
  habitat = ex_hab,
  barrier = ex_bar,
  distance = 50
)
