# Helper
square <- function(xmin, ymin, size = 100) {
  sf::st_polygon(list(cbind(
    c(xmin, xmin + size, xmin + size, xmin, xmin),
    c(ymin, ymin, ymin + size, ymin + size, ymin)
  )))
}

# sf_fragment_habitat --------------------------------------------------------

test_that("sf_fragment_habitat returns an sf object with id column", {
  buffer <- sf::st_sfc(square(0, 0, size = 500), crs = 32754)
  # Vertical barrier cuts through the buffer
  barrier <- sf::st_sfc(
    sf::st_polygon(list(cbind(
      c(200, 300, 300, 200, 200),
      c(-50, -50, 550, 550, -50)
    ))),
    crs = 32754
  )

  result <- sf_fragment_habitat(buffer, barrier)

  expect_s3_class(result, "sf")
  expect_snapshot(names(result))
})

test_that("sf_fragment_habitat splits buffer into multiple fragments", {
  # Wide buffer, narrow barrier through the middle
  buffer <- sf::st_sfc(square(0, 0, size = 500), crs = 32754)
  barrier <- sf::st_sfc(
    sf::st_polygon(list(cbind(
      c(240, 260, 260, 240, 240),
      c(-50, -50, 550, 550, -50)
    ))),
    crs = 32754
  )

  result <- sf_fragment_habitat(buffer, barrier)

  expect_gte(nrow(result), 2)
  expect_equal(result$id, seq_len(nrow(result)))
})

# sf_assign_patches_to_fragments ---------------------------------------------

test_that("sf_assign_patches_to_fragments assigns patch_id column", {
  # Two remaining patches on either side of a barrier
  remaining <- sf::st_sfc(
    square(0, 0),
    square(400, 0),
    crs = 32754
  )

  # Two fragments covering each patch
  fragment_id <- sf::st_sf(
    id = 1:2,
    geometry = sf::st_sfc(
      square(-50, -50, size = 250),
      square(350, -50, size = 250),
      crs = 32754
    )
  )

  result <- sf_assign_patches_to_fragments(remaining, fragment_id)

  expect_snapshot(names(result))
  expect_equal(nrow(result), 2)
  # Each patch should be assigned to a different fragment
  expect_equal(length(unique(result$patch_id)), 2)
})

test_that("sf_assign_patches_to_fragments assigns connected patches same id", {
  # Two habitat patches both inside the same fragment
  remaining <- sf::st_sfc(
    square(10, 10),
    square(120, 10),
    crs = 32754
  )

  # One big fragment containing both patches
  fragment_id <- sf::st_sf(
    id = 1L,
    geometry = sf::st_sfc(square(0, 0, size = 300), crs = 32754)
  )

  result <- sf_assign_patches_to_fragments(remaining, fragment_id)

  # Both patches should share the same patch_id
  expect_snapshot(unique(result$patch_id))
  expect_equal(length(unique(result$patch_id)), 1)
})
