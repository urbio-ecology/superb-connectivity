library(urbioconnect)
lizard_connectivity <- habitat_connectivity(
  habitat = example_habitat(),
  barrier = example_barrier(),
  species = "Blue-tongued Lizard",
  interpatch_distance = 50,
  verbose = FALSE
)

# `habitat_connectivity()` now returns a summary; the dataset is the per-patch
# table, extracted with the accessor.
lizard_areas_connected <- patch_sizes(lizard_connectivity)[[1]]

usethis::use_data(lizard_areas_connected, overwrite = TRUE)
