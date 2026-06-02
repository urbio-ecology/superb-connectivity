test_that("multiplication works", {
  baseline_areas <- round(lizard_areas_connected$area)
  new_areas <- baseline_areas[-1] * 0.8
  expect_snapshot(compare_connectivity(
    new_areas,
    baseline_areas,
    interpatch_distance = 10,
    species = "Blue-tongued Lizard"
  ))
})
