# Helper: create a small raster with uniform values
small_rast <- function(nrows = 5, ncols = 5, val = NA) {
  terra::rast(nrows = nrows, ncols = ncols, vals = val)
}

# determine the flattened index from the
index_from_ij <- function(mat, i, j) {
  (ncol(mat) * (i - 1)) + j
}

index_mat <- function(i, j) {
  n <- seq_len(i * j)
  matrix(n, i, j, byrow = TRUE)
}

create_index_mat <- function(mat) {
  i <- nrow(mat)
  j <- ncol(mat)
  index_mat(i, j)
}

lookup_mat <- function(i, j, idx_i, idx_j) {
  idx_mat <- index_mat(i, j)
  idx_mat[idx_i, idx_j]
}


test_that("create_barrier_mask converts 1s to NA and NAs to 1s", {
  r <- small_rast(nrows = 5, ncols = 5, val = NA)
  row_i <- 2
  col_j <- 2
  r[row_i, col_j] <- 1 # centre cell is a barrier

  mask <- create_barrier_mask(r)
  vals <- as.numeric(terra::values(mask))

  centre_cell_idx <- index_from_ij(r, row_i, col_j)
  # Centre cell (index 7 in 5x5 row-major: row 2, col 2) should be NA
  expect_true(is.na(vals[centre_cell_idx]))
  # All other cells (previously NA) should be 1
  expect_true(all(vals[-centre_cell_idx] == 1, na.rm = TRUE))
  expect_false(any(is.na(vals[-centre_cell_idx])))
})

test_that("create_barrier_mask on all-NA raster produces all-1 raster", {
  r <- small_rast(val = NA)
  mask <- create_barrier_mask(r)
  vals <- as.numeric(terra::values(mask))

  expect_true(all(vals == 1))
  expect_false(any(is.na(vals)))
})

test_that("drop_habitat_under_barrier removes habitat cells covered by barriers", {
  # All-habitat raster
  habitat <- small_rast(val = 1)

  # Barrier at centre cell
  barrier <- small_rast(val = NA)
  idx_i <- 2
  idx_j <- 2
  barrier[idx_i, idx_j] <- 1
  barrier_mask <- create_barrier_mask(barrier)

  remaining <- drop_habitat_under_barrier(habitat, barrier_mask)
  vals <- as.numeric(terra::values(remaining))

  centre_cell_idx <- index_from_ij(barrier, idx_i, idx_j)
  # Centre cell should now be NA
  expect_true(is.na(vals[centre_cell_idx]))
  # All other cells should remain 1
  expect_true(all(vals[-centre_cell_idx] == 1, na.rm = TRUE))
})

test_that("fragment_habitat cuts buffer at barrier cells", {
  buffered <- small_rast(val = 1)

  # Vertical barrier through centre column (col 3 of 5)
  barrier <- small_rast(val = NA)
  idx_i <- 1:5
  idx_j <- 3
  barrier[idx_i, idx_j] <- 1
  barrier_mask <- create_barrier_mask(barrier)

  fragmented <- fragment_habitat(buffered, barrier_mask)
  vals <- as.numeric(terra::values(fragmented))

  # Centre column indices in a 5x5 row-major raster: 3, 8, 13, 18, 23
  centre_col <- create_index_mat(barrier)[idx_i, idx_j]
  expect_true(all(is.na(vals[centre_col])))
  # Cells outside the barrier should remain non-NA
  expect_all_false(is.na(vals[-centre_col]))
})

lizard_habitat <- example_habitat()
lizard_barrier <- example_barrier()

buffered <- habitat_buffer(lizard_habitat, distance = 50)
barrier_mask <- create_barrier_mask(lizard_barrier)
fragmented <- fragment_habitat(buffered, barrier_mask)
remaining <- drop_habitat_under_barrier(lizard_habitat, barrier_mask)
patches <- assign_patches_to_fragments(remaining, fragmented)
patch_areas <- add_patch_area(patches)

test_that("habitat_buffer expands non-NA habitat area", {
  n_before <- sum(!is.na(terra::values(lizard_habitat)))
  n_after <- sum(!is.na(terra::values(buffered)))

  expect_gt(n_after, n_before)
})

test_that("add_patch_area returns a two-layer raster named patch_id and area", {
  result <- add_patch_area(patches)
  expect_equal(terra::nlyr(result), 2)
  expect_snapshot(names(result))
})

res_con_patch <- aggregate_connected_patches(patch_areas)
test_that("aggregate_connected_patches returns tibble with correct columns", {
  expect_s3_class(res_con_patch, "tbl_df")
  expect_snapshot(names(res_con_patch))
  expect_gt(nrow(res_con_patch), 0)
  expect_all_true(res_con_patch$area > 0)
})

test_that("aggregate_connected_patches area_squared equals area squared", {
  # area_squared = area^2, then both rounded to 3 dp
  expect_equal(res_con_patch$area_squared, round(res_con_patch$area^2, 3))
})

test_that("habitat_connectivity returns a data frame with expected columns", {
  result <- habitat_connectivity(
    lizard_habitat,
    lizard_barrier,
    distance = 50,
    verbose = FALSE
  )

  expect_s3_class(result$result, "data.frame")
  expect_snapshot(names(result$result))
  expect_gt(nrow(result$result), 0)
})

test_that("habitat_connectivity_full returns list with expected elements", {
  result <- habitat_connectivity_full(
    lizard_habitat,
    lizard_barrier,
    distance = 50,
    verbose = FALSE
  )

  expect_type(result, "list")
  expect_snapshot(names(result))
  expect_s3_class(result$areas_connected, "data.frame")
  expect_s4_class(result$buffered_habitat, "SpatRaster")
})
