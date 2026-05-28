test_that("multiplication works", {
  baseline_areas <- round(lizard_areas_connected$area)
  new_areas <- baseline_areas[-1] * 0.8
  compare_connectivity(
    new_area = new_areas,
    baseline_area = baseline_areas
  )
})
