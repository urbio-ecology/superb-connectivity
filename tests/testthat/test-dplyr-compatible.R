test_that("patch_size_tbl class is compatible with dplyr", {
  # summarise_connectivity.patch_area
  ps <- patch_sizes(habitat_connectivity(
    habitat = example_habitat(),
    barrier = example_barrier(),
    species = "Blue-tongued Lizard",
    interpatch_distance = 8,
    verbose = FALSE
  ))[[1]]

  library(dplyr)
  expect_snapshot(ps |> filter(area > 4000))
  expect_snapshot(ps |> filter(area > 1000))
  expect_snapshot(ps |> slice(1:10))
  expect_snapshot(ps |> select(-area) |> head())
  expect_snapshot(ps |> select(-patch_id) |> head())
})
