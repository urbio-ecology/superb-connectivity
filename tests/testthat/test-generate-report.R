test_that("generate_connectivity_report errors with an unrecognised output_format", {
  expect_error(
    generate_connectivity_report(
      species = "Test",
      interpatch_distances = 100,
      results_connect_habitat = data.frame(),
      areas_connected = list(),
      output_format = "word"
    ),
    class = "rlang_error"
  )
})

test_that("generate_connectivity_report errors when report template is missing", {
  # inst/templates/connectivity-report.qmd does not exist in this repo,
  # so the function should abort after passing arg_match and check_installed.
  skip_if_not_installed("quarto")
  expect_error(
    generate_connectivity_report(
      species = "Test",
      interpatch_distances = 100,
      results_connect_habitat = data.frame(),
      areas_connected = list(),
      output_format = "html"
    ),
    class = "rlang_error"
  )
})

test_that("generate_connectivity_report renders html with mocked quarto", {
  skip_if_not_installed("quarto")

  tmp_template <- withr::local_tempfile(fileext = ".qmd")
  writeLines("---\nformat: html\n---\n\nTest template", tmp_template)

  local_mocked_bindings(
    here = function(...) tmp_template,
    .package = "here"
  )
  local_mocked_bindings(
    quarto_render = function(...) invisible(NULL),
    .package = "quarto"
  )

  result <- withr::with_tempdir(
    generate_connectivity_report(
      species = "Superb Fairy Wren",
      interpatch_distances = c(100, 200),
      results_connect_habitat = data.frame(),
      areas_connected = list(),
      output_format = "html",
      output_dir = "reports/new"
    )
  )

  expect_type(result, "character")
  expect_true(grepl("\\.html$", result))
})

test_that("generate_connectivity_report renders pdf with mocked quarto", {
  skip_if_not_installed("quarto")

  tmp_template <- withr::local_tempfile(fileext = ".qmd")
  writeLines("---\nformat: pdf\n---\n\nTest template", tmp_template)

  local_mocked_bindings(
    here = function(...) tmp_template,
    .package = "here"
  )
  local_mocked_bindings(
    quarto_render = function(...) invisible(NULL),
    .package = "quarto"
  )

  result <- withr::with_tempdir(
    generate_connectivity_report(
      species = "Superb Fairy Wren",
      interpatch_distances = 100,
      results_connect_habitat = data.frame(),
      areas_connected = list(),
      output_format = "pdf"
    )
  )

  expect_type(result, "character")
  expect_true(grepl("\\.pdf$", result))
})
