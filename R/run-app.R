#' Run Urban Connectedness Shiny App
#'
#' Launches the Shiny app for interactive habitat connectivity analysis.
#'
#' @param launch.browser Logical. Should the app open in a browser? Default TRUE.
#' @param port Integer. Port number for the app. Default NULL (random port).
#' @param host Character. Host IP address. Default "127.0.0.1".
#'
#' @return Starts the Shiny app
#' @export
#'
#' @examples
#' \dontrun{
#' run_connectivity_app()
#' }
run_connectivity_app <- function(
  launch.browser = TRUE,
  port = NULL,
  host = "127.0.0.1"
) {
  # Look for app directory in project root
  app_dir <- here::here("app")

  if (!dir.exists(app_dir)) {
    cli::cli_abort(
      "Shiny app directory not found. Expected at: {.path {app_dir}}"
    )
  }

  # Check required files
  required_files <- c("app.R", "ui.R", "server.R")
  missing_files <- required_files[
    !file.exists(file.path(app_dir, required_files))
  ]

  if (length(missing_files) > 0) {
    cli::cli_abort(
      "Missing required app files: {paste(missing_files, collapse = ', ')}"
    )
  }

  cli::cli_inform("Starting Urban Connectedness app...")
  cli::cli_inform("App directory: {app_dir}")

  # Run the app
  shiny::runApp(
    appDir = app_dir,
    launch.browser = launch.browser,
    port = port,
    host = host
  )
}
