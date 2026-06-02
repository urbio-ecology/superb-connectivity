library(urbioconnect)
lizard_areas_connected <- habitat_connectivity(
  habitat = example_habitat(),
  barrier = example_barrier(),
  interpatch_distance = 50,
  verbose = FALSE
)

usethis::use_data(lizard_areas_connected)
