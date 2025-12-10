#' Launch the Connectivity Shiny App
#'
#' @description
#' Launches the connectivity analysis Shiny application.
#'
#' @return No return value, called for side effects (launches Shiny app)
#' @export
#'
#' @examples
#' \dontrun{
#' run_connectivity_app()
#' }
run_connectivity_app <- function() {
  app_dir <- system.file("shiny", package = "urbioconnect")
  
  if (app_dir == "" || !dir.exists(app_dir)) {
    cli::cli_abort(
      "Shiny app not found. Please reinstall the package."
    )
  }
  
  shiny::runApp(app_dir)
}