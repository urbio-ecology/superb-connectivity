test_that("patch_size_tbl works", {
  expect_snapshot(
    patch_size_tbl(
      data = data.frame(area = 1:10, patch_id = 1:10),
      species = "birds",
      interpatch_distance = 10,
      res = c(1, 1)
    )
  )
})

test_that("validate_patch_size_tbl works", {
  expect_snapshot(
    error = TRUE,
    validate_patch_size_tbl(iris)
  )
  expect_snapshot(
    validate_patch_size_tbl(lizard_areas_connected)
  )
  altered_lizard_areas <- lizard_areas_connected |>
    dplyr::rename(bananas = patch_id)
  expect_snapshot(
    error = TRUE,
    validate_patch_size_tbl(altered_lizard_areas)
  )
})

test_that("check_scalar works as expected", {
  expect_snapshot(
    error = TRUE,
    check_scalar(1:3)
  )
  expect_snapshot(
    error = TRUE,
    check_scalar(LETTERS[1:3])
  )
  expect_snapshot(
    error = TRUE,
    check_scalar(c(TRUE, FALSE, TRUE))
  )
  expect_snapshot(check_scalar(1))
  expect_snapshot(check_scalar("1"))
  expect_snapshot(check_scalar(TRUE))
})

birds_r1_i8 <- new_patch_size_tbl(
  data = data.frame(patch_id = 1:5, area = 5:1),
  res = c(1, 1),
  species = "birds",
  interpatch_distance = 8
)

birds_r1_i10 <- new_patch_size_tbl(
  data = data.frame(patch_id = 1:5, area = 5:1),
  res = c(1, 1),
  species = "birds",
  interpatch_distance = 10
)

birds_r2_i10 <- new_patch_size_tbl(
  data = data.frame(patch_id = 1:5, area = 5:1),
  res = c(2, 2),
  species = "birds",
  interpatch_distance = 10
)

birds_r2_i8 <- new_patch_size_tbl(
  data = data.frame(patch_id = 1:5, area = 5:1),
  res = c(2, 2),
  species = "birds",
  interpatch_distance = 8
)

cats_r1_i8 <- new_patch_size_tbl(
  data = data.frame(patch_id = 1:5, area = 5:1),
  res = c(1, 1),
  species = "cats",
  interpatch_distance = 8
)


test_that("check_pc_match errors appropriately", {
  expect_silent(check_pc_match(birds_r1_i8, birds_r1_i8))
  expect_snapshot(
    error = TRUE,
    check_pc_match(birds_r1_i8, birds_r1_i10)
  )
  expect_snapshot(
    error = TRUE,
    check_pc_match(birds_r1_i8, cats_r1_i8)
  )
  expect_snapshot(
    error = TRUE,
    check_pc_match(birds_r1_i8, birds_r2_i8)
  )
})
