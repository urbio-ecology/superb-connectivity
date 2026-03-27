test_that("effective_mesh_size with many equal patches scales correctly", {
  # 10 patches of 1000 sq m each vs 1 patch of 10000 sq m
  # With 10 equal patches: sum(1000^2 * 10) / 10000 * 0.0001 = 1e7/1e4 * 1e-4 = 0.1
  # With 1 patch of 10000: 1e8/1e4 * 1e-4 = 1.0
  many <- effective_mesh_size(rep(1000^2, 10), rep(1000, 10))
  one <- effective_mesh_size(10000^2, 10000)

  expect_lt(many, one)
  expect_equal(many, 0.1)
})

test_that("total_habitat_area is additive", {
  combined <- total_habitat_area(c(10000, 20000, 30000))
  separate <- total_habitat_area(10000) +
    total_habitat_area(20000) +
    total_habitat_area(30000)

  expect_equal(combined, separate)
})

test_that("n_patches handles single-element input", {
  expect_equal(n_patches(42), 1)
})

test_that("mean_patch_size with identical patches equals that patch size", {
  expect_equal(mean_patch_size(rep(500, 10)), 500)
})

test_that("summarise_connectivity with multiple buffer distances stays one row each", {
  # Called once per buffer distance — should always return exactly 1 row
  purrr::walk(c(50, 100, 200), function(dist) {
    result <- summarise_connectivity(
      area_squared = c(5000^2, 5000^2),
      area_total = c(5000, 5000),
      buffer_distance = dist,
      target_resolution = 500,
      data_resolution = 10,
      aggregation_factor = 50,
      species_name = "Wren"
    )
    expect_equal(nrow(result), 1)
    expect_equal(result$buffer_distance, dist)
  })
})

test_that("connectivity_probability with single large patch equals single small patch", {
  # For one patch: prob = (A^2/A * 0.0001) / A = 0.0001 regardless of area
  small <- connectivity_probability(1000^2, 1000)
  large <- connectivity_probability(100000^2, 100000)

  expect_equal(small, large)
  expect_equal(small, 0.0001)
})
