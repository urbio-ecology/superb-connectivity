#' Generate Connectivity Report
#'
#' Creates a parameterised Quarto report from connectivity analysis results.
#'
#' @param species_name Character. Name of the species being analysed.
#' @param buffer_distances Numeric vector. Buffer distances used in analysis
#' (in meters).
#' @param results_connect_habitat Data frame. Connectivity summary results.
#' @param areas_connected List of data frames. Connected patch areas for e
#'   ach buffer distance.
#' @param habitat SF object. Habitat spatial data (optional, for mapping).
#' @param barrier SF object. Barrier spatial data (optional, for mapping).
#' @param habitat_raster Terra SpatRaster. Habitat raster (optional, for
#' mapping).
#' @param data_resolution Numeric. Data resolution in meters.
#' @param target_resolution Numeric. Target resolution in meters.
#' @param output_file Character. Output filename (without extension).
#' @param output_format Character. Output format: "html" (default), "pdf", or
#' "both".
#' @param output_dir Character. Directory to save the report (default: current
#' directory).
#'
#' @return Character vector of generated report file path(s).
#' @export
#'
#' @examples
#' \dontrun{
#' report_path <- generate_connectivity_report(
#'   species_name = "Superb Fairy Wren",
#'   buffer_distances = c(100, 250, 400),
#'   results_connect_habitat = results_df,
#'   areas_connected = patches_list,
#'   output_format = "html"
#' )
#' }
generate_connectivity_report <- function(
  species_name,
  buffer_distances,
  results_connect_habitat,
  areas_connected,
  habitat = NULL,
  barrier = NULL,
  habitat_raster = NULL,
  data_resolution = 10,
  target_resolution = 500,
  output_file = NULL,
  output_format = c("html", "pdf", "both"),
  output_dir = getwd()
) {
  # Match argument
  output_format <- rlang::arg_match(output_format)

  rlang::check_installed("quarto")

  # Look for template in inst/templates
  template_path <- here::here("inst/templates/connectivity-report.qmd")

  report_template_not_found <- !file.exists(template_path)
  if (report_template_not_found) {
    cli::cli_abort("Report template not found at: {.path {template_path}}")
  }

  # Create output directory if needed
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Generate default filename if not provided
  if (is.null(output_file)) {
    species_slug <- gsub("[^A-Za-z0-9]+", "-", tolower(species_name))
    output_file <- glue::glue(
      "connectivity-report_{species_slug}_{format(Sys.Date(), '%Y%m%d')}"
    )
  }

  # Prepare parameters
  params <- list(
    species_name = species_name,
    buffer_distances = buffer_distances,
    results_connect_habitat = results_connect_habitat,
    areas_connected = areas_connected,
    habitat = habitat,
    barrier = barrier,
    habitat_raster = habitat_raster,
    data_resolution = data_resolution,
    target_resolution = target_resolution
  )

  # Render report(s)
  output_files <- c()

  if (output_format %in% c("html", "both")) {
    cli::cli_inform("Rendering HTML report...")
    html_file <- file.path(output_dir, paste0(output_file, ".html"))

    quarto::quarto_render(
      input = template_path,
      output_format = "html",
      output_file = basename(html_file),
      execute_dir = output_dir,
      execute_params = params,
      quiet = FALSE
    )

    output_files <- c(output_files, html_file)
    cli::cli_inform("HTML report created: {.path {html_file}}")
  }

  if (output_format %in% c("pdf", "both")) {
    cli::cli_inform("Rendering PDF report...")
    pdf_file <- file.path(output_dir, paste0(output_file, ".pdf"))

    quarto::quarto_render(
      input = template_path,
      output_format = "pdf",
      output_file = basename(pdf_file),
      execute_dir = output_dir,
      execute_params = params,
      quiet = FALSE
    )

    output_files <- c(output_files, pdf_file)
    cli::cli_inform("PDF report created: {.path {pdf_file}}")
  }

  invisible(output_files)
}
