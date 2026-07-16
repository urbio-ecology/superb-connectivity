test_that("patch_size class is compatible with dplyr", {
  # summarise_connectivity.patch_area
  areas <- habitat_connectivity(
    habitat = example_habitat(),
    interpatch_distance = 8,
    barrier = example_barrier(),
    species = "Blue-tongued Lizard",
  )

  library(dplyr)
  expect_snapshot(areas |> filter(area > 4000))
  expect_snapshot(areas |> filter(area > 1000))
  expect_snapshot(areas |> slice(1:10))
  expect_snapshot(areas |> select(-area) |> head())
  expect_snapshot(areas |> select(-patch_id) |> head())
})
