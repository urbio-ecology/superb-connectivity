test_that("resolve_buffer_radius halves interpatch_distance, passes buffer_radius through", {
  # interpatch_distance is the full edge-to-edge distance -> halve to a radius
  expect_equal(
    resolve_buffer_radius(
      interpatch_distance = 250
    ),
    125
  )
  # buffer_radius is already the radius -> used as-is (same result from half the input)
  expect_equal(
    resolve_buffer_radius(
      buffer_radius = 125
    ),
    125
  )
})


test_that("resolve_buffer_radius requires exactly one of the two args", {
  expect_snapshot(
    error = TRUE,
    resolve_buffer_radius(
      interpatch_distance = 250,
      buffer_radius = 125
    )
  )
  expect_snapshot(
    error = TRUE,
    resolve_buffer_radius(
      interpatch_distance = NULL,
      buffer_radius = NULL
    )
  )
})


test_that("warn_buffer_resolution is silent when the radius aligns with the resolution", {
  # 500 is a clean multiple of a 250 m cell -> no discretisation loss
  expect_no_warning(
    warn_buffer_resolution(
      buffer_radius = 500,
      resolution = 250
    )
  )
})

test_that("warn_buffer_resolution warns when the radius is smaller than one cell", {
  # 100 m radius on a 500 m cell -> sub-cell, buffer is negligible
  expect_snapshot(
    warn_buffer_resolution(
      buffer_radius = 100,
      resolution = 500
    )
  )
})

test_that("warn_buffer_resolution reports the effective distance when not a clean multiple", {
  # 600 m radius on a 500 m cell snaps to 500 m (effective interpatch distance 1000 m)
  expect_snapshot(
    warn_buffer_resolution(
      buffer_radius = 600,
      resolution = 500
    )
  )
})
