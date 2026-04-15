test_that("empty_grid returns a SpatRaster with no values set", {
  habitat <- example_habitat()
  grid <- empty_grid(habitat, resolution = 10)

  expect_s4_class(grid, "SpatRaster")
  expect_false(terra::hasValues(grid))
})

test_that("empty_grid resolution matches requested resolution", {
  habitat <- example_habitat()
  grid <- empty_grid(habitat, resolution = 20)

  expect_equal(terra::res(grid), c(20, 20), tolerance = 0.01)
})

test_that("empty_grid extent matches habitat extent", {
  habitat <- example_habitat()
  grid <- empty_grid(habitat, resolution = 10)

  # Extents should be equal
  expect_true(terra::all.equal(terra::ext(grid), terra::ext(habitat)))
})

test_that("empty_grid CRS matches habitat CRS", {
  habitat <- example_habitat()
  grid <- empty_grid(habitat, resolution = 10)

  expect_equal(terra::crs(grid), terra::crs(habitat))
})

test_that("prepare_rasters returns list with habitat_raster and barrier_raster", {
  habitat_shp <- example_barrier_shp() # re-use as simple sf for testing
  barrier_shp <- example_barrier_shp()

  result <- prepare_rasters(
    habitat = habitat_shp,
    barrier = barrier_shp,
    data_resolution = 10,
    target_resolution = 100
  )

  expect_type(result, "list")
  expect_snapshot(names(result))
  expect_s4_class(result$habitat_raster, "SpatRaster")
  expect_s4_class(result$barrier_raster, "SpatRaster")
})

test_that("prepare_rasters habitat and barrier rasters share the same extent", {
  habitat_shp <- example_barrier_shp()
  barrier_shp <- example_barrier_shp()

  result <- prepare_rasters(
    habitat_shp,
    barrier_shp,
    data_resolution = 10,
    target_resolution = 100
  )

  expect_true(
    terra::compareGeom(
      result$habitat_raster,
      result$barrier_raster,
      stopOnError = FALSE
    )
  )
})
