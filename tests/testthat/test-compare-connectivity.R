test_that("compare_connectivity works for patch_size", {
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

wren_habitat <- example_wren_habitat()
wren_barrier <- example_wren_barrier()
wren_barrier_scenario <- example_wren_barrier_scenario()

wren_connectivity_baseline <- habitat_connectivity(
  habitat = wren_habitat,
  barrier = wren_barrier,
  species = "Superb Fairy Wren",
  interpatch_distance = 200
)

wren_connectivity_scenario <- habitat_connectivity(
  habitat = wren_habitat,
  barrier = wren_barrier_scenario,
  species = "Superb Fairy Wren",
  interpatch_distance = 200
)

test_that("compare_connectivity() identifies changes in baseline/scenario", {
  baseline_summary <- summarise_connectivity(
    connectivity = wren_connectivity_baseline
  )

  scenario_summary <- summarise_connectivity(
    connectivity = wren_connectivity_scenario,
    connectivity_baseline = wren_connectivity_baseline
  )

  results_compare <- compare_connectivity(
    connectivity = wren_connectivity_scenario,
    connectivity_baseline = wren_connectivity_baseline
  )

  expect_equal(results_compare$n_patches[1], baseline_summary$n_patches)
  expect_equal(results_compare$n_patches[2], scenario_summary$n_patches)
  expect_gt(
    results_compare$effective_mesh_ha[1],
    results_compare$effective_mesh_ha[2]
  )
  expect_lt(
    results_compare$n_patches[1],
    results_compare$n_patches[2]
  )
  expect_snapshot(results_compare)
})
