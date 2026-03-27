test_that("run_connectivity_app launches the shiny app", {
  local_mocked_bindings(
    runApp = function(...) invisible(NULL),
    .package = "shiny"
  )
  expect_no_error(run_connectivity_app())
})
