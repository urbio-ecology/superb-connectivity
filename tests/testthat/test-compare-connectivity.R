test_that("compare_connectivity works for patch_connectivity", {
  expect_snapshot(
    compare_connectivity(
      connectivity = lizard_areas_connected,
      connectivity_baseline = lizard_areas_connected
    )
  )
})

test_that("compare-connectivity works", {
  baseline_areas <- round(lizard_areas_connected$area)
  new_areas <- baseline_areas[-1] * 0.8
  expect_snapshot(compare_connectivity(
    connectivity = new_areas,
    connectivity_baseline = baseline_areas,
    interpatch_distance = 10,
    res = pc_res(lizard_areas_connected),
    species = "Blue-tongued Lizard"
  ))
})
