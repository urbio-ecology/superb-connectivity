test_that("multiplication works", {
  baseline_areas <- round(lizard_areas_connected$area)
  new_areas <- baseline_areas[-1] * 0.8
  compare_connectivity(
    area_new = new_areas,
    area_baseline = baseline_areas,
    buffer_distance = 10,
    target_resolution = 10,
    data_resolution = 10,
    aggregation_factor = 10,
    species_name = "blue-tongued lizard"
  )
})
