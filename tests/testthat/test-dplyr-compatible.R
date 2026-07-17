test_that("patch_size class is compatible with dplyr", {
  # summarise_connectivity.patch_area
  areas <- habitat_connectivity(
    habitat = example_habitat(),
    interpatch_distance = 8,
    barrier = example_barrier(),
    species = "Blue-tongued Lizard",
  )
})
