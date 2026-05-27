test_that("effective_mesh_size computes correctly in hectares", {
  # Single patch: sum(a^2)/sum(a) * 0.0001 = a * 0.0001
  expect_equal(effective_mesh_size(10000^2, 10000), 1)

  # Two equal patches: (5000^2 + 5000^2) / 10000 * 0.0001 = 0.5
  expect_equal(effective_mesh_size(c(5000^2, 5000^2), c(5000, 5000)), 0.5)
})

test_that("effective_mesh_size is smaller for fragmented habitat", {
  # One big patch vs two halves — fragmentation reduces effective mesh size
  unfrag <- effective_mesh_size(10000^2, 10000)
  frag <- effective_mesh_size(c(5000^2, 5000^2), c(5000, 5000))
  expect_gt(unfrag, frag)
})

test_that("mean_patch_size returns mean of input", {
  expect_equal(mean_patch_size(c(100, 300)), 200)
  expect_equal(mean_patch_size(c(250)), 250)
})

test_that("n_patches counts correctly", {
  expect_equal(n_patches(c(100, 200, 300)), 3)
  expect_equal(n_patches(c(500)), 1)
  expect_equal(n_patches(numeric(0)), 0)
})

test_that("total_habitat_area converts square metres to hectares", {
  expect_equal(total_habitat_area(10000), 1)
  expect_equal(total_habitat_area(c(20000, 30000)), 5)
})

test_that("connectivity_probability is higher for unfragmented habitat", {
  unfrag_area <- 1000
  frag_area <- 5000

  effective_mesh_unfrag <- effective_mesh_size(
    area_squared = unfrag_area^2,
    area = unfrag_area
  )
  effective_mesh_frag <- effective_mesh_size(
    area_squared = c(frag_area^2, frag_area^2),
    area = c(frag_area, frag_area)
  )

  unfrag <- connectivity_probability(effective_mesh_unfrag, unfrag_area)
  frag <- connectivity_probability(effective_mesh_frag, c(frag_area, frag_area))
  expect_gt(unfrag, frag)
  expect_gt(unfrag, 0)
  expect_gt(frag, 0)
})

test_that("summarise_connectivity returns a tibble with expected columns", {
  result <- summarise_connectivity(
    area_squared = 10000^2,
    area_total = 10000,
    buffer_distance = 100,
    target_resolution = 500,
    data_resolution = 10,
    aggregation_factor = 50,
    species_name = "Test Species"
  )

  expect_s3_class(result, "tbl_df")
  expect_snapshot(names(result))
  expect_equal(nrow(result), 1)
  expect_snapshot(result)
})

test_that("summarise_connectivity rounds prob_connectedness to 6 decimal places", {
  result <- summarise_connectivity(
    area_squared = c(10000^2, 20000^2),
    area_total = c(10000, 20000),
    buffer_distance = 200,
    target_resolution = 500,
    data_resolution = 10,
    aggregation_factor = 50,
    species_name = "Test"
  )
  expect_equal(result$prob_connectedness, round(result$prob_connectedness, 6))
})
