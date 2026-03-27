test_that("clean returns a valid sfc geometry", {
  habitat <- sf::st_sfc(
    sf::st_polygon(list(cbind(c(0, 100, 100, 0, 0), c(0, 0, 100, 100, 0)))),
    crs = 32754
  )

  result <- clean(habitat)

  expect_s3_class(result, "sfc")
  expect_all_true(sf::st_is_valid(result))
})

test_that("clean unions multiple overlapping polygons into one", {
  # Two overlapping squares
  habitat <- sf::st_sfc(
    sf::st_polygon(list(cbind(c(0, 100, 100, 0, 0), c(0, 0, 100, 100, 0)))),
    sf::st_polygon(list(cbind(c(50, 150, 150, 50, 50), c(0, 0, 100, 100, 0)))),
    crs = 32754
  )

  result <- clean(habitat)

  expect_equal(length(result), 1)
})

test_that("clean unions non-overlapping polygons into one multipolygon", {
  habitat <- sf::st_sfc(
    sf::st_polygon(list(cbind(c(0, 100, 100, 0, 0), c(0, 0, 100, 100, 0)))),
    sf::st_polygon(list(cbind(
      c(500, 600, 600, 500, 500),
      c(0, 0, 100, 100, 0)
    ))),
    crs = 32754
  )

  result <- clean(habitat)

  # st_union always returns one geometry object (MULTIPOLYGON if non-contiguous)
  expect_equal(length(result), 1)
})
