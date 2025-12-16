#' Convert color name to hexadecimal
#'
#' @param color_name Character. Color name recognized by R.
#'
#' @returns Character. Hexadecimal color code.
#'
#' @examples
#' col2hex("forestgreen")
#' col2hex("blue")
#' @export
col2hex <- function(color_name) {
  grDevices::rgb(t(grDevices::col2rgb(color_name)), maxColorValue = 255)
}

#' Plot barrier, habitat, and buffer layers
#'
#' Creates a visualization showing habitat, buffer zone, and barriers using
#' terra rasters.
#'
#' @param barrier Terra SpatRaster. Barrier layer (e.g., roads).
#' @param buffered Terra SpatRaster. Buffered habitat layer.
#' @param habitat Terra SpatRaster. Original habitat layer.
#' @param distance Numeric. Buffer distance in meters.
#' @param species_name Character. Species name for plot title.
#' @param col_barrier Character. Color for barrier layer.
#' @param col_buffer Character. Color for buffer zone.
#' @param col_habitat Character. Color for habitat patches.
#' @param col_paper Character. Background color (default: "white").
#'
#' @returns A ggplot2 object.
#' @export
gg_barrier_habitat_buffer <- function(
  barrier,
  buffered,
  habitat,
  distance,
  species_name,
  col_barrier,
  col_buffer,
  col_habitat,
  col_paper = "white"
) {
  # First, reclassify your rasters to assign actual color values
  barrier_coloured <- terra::subst(barrier, 1, col_barrier)
  buffer_coloured <- terra::subst(buffered, 1, col_buffer)
  habitat_coloured <- terra::subst(habitat, 1, col_habitat)

  # Now plot them in layers (bottom to top)
  ggplot2::ggplot() +
    tidyterra::geom_spatraster(data = buffer_coloured) +
    tidyterra::geom_spatraster(data = barrier_coloured) +
    tidyterra::geom_spatraster(data = habitat_coloured) +
    ggplot2::theme_minimal(paper = col_paper) +
    ggplot2::scale_fill_identity(na.value = NA) +
    ggplot2::labs(
      title = marquee::marquee_glue(
        "{.{col_habitat} {species_name} Habitat}, \\
        {.{col_buffer} {distance}m buffer}, and barrier (white)"
      )
    ) +
    ggplot2::theme_sub_plot(
      title = marquee::element_marquee()
    )
}

#' Display plots in tabs
#'
#' Helper function to display a list of plots with tab headers in R Markdown
#' documents.
#'
#' @param the_list Named list. List of plot objects.
#' @param message Character. Prefix message for each tab heading.
#'
#' @returns Invisible NULL. Prints plots with markdown headers.
#' @export
show_tabs <- function(the_list, message = NULL) {
  for (iplot in names(the_list)) {
    cat(sprintf("## %s %s\n", message, iplot))
    print(the_list[[iplot]])
    cat(sprintf("\n\n"))
  }
}

#' Display images in tabs
#'
#' Helper function to display a list of image paths with tab headers in R
#' Markdown documents.
#'
#' @param images Named character vector. Paths to image files.
#' @param message Character. Prefix message for each tab heading.
#'
#' @returns Invisible NULL. Includes images with markdown headers.
#' @export
show_image_tabs <- function(images, message = NULL) {
  for (iplot in names(images)) {
    cat(sprintf("## %s %s\n", message, iplot))
    knitr::include_graphics(images[[iplot]])
    cat(sprintf("\n\n"))
  }
}

#' Convert snake_case to sentence case
#'
#' @param x Character vector. Text in snake_case format.
#'
#' @returns Character vector. Text converted to sentence case.
#' @export
to_sentence <- function(x) {
  x |>
    stringr::str_replace_all("_", " ") |>
    stringr::str_to_sentence()
}

#' Plot connected habitat patches
#'
#' Visualizes habitat patches colored by their connected fragment ID.
#'
#' @param patch_id Terra SpatRaster. Raster with patch IDs.
#' @param distance Numeric. Buffer distance used (for subtitle).
#' @param species_name Character. Species name (default: "Species").
#' @param n_cols Integer. Number of colors to cycle through (default: 7).
#'
#' @returns A ggplot2 object showing patches with distinct colors.
#' @export
plot_patches <- function(
  patch_id,
  distance,
  species_name = "Species",
  n_cols = 7
) {
  raster_patches <- as.factor(patch_id$patch_id)

  n_patches <- patch_id$patch_id |> unique() |> nrow()

  my_colours <- colorspace::qualitative_hcl(n = n_cols)

  unique_vals <- unique(terra::values(raster_patches))
  unique_vals <- unique_vals[!is.na(unique_vals)]

  # assign colours cyclically
  colour_indices <- ((unique_vals - 1) %% n_cols) + 1
  colour_map <- my_colours[colour_indices]
  names(colour_map) <- as.character(unique_vals)

  ggplot2::ggplot() +
    tidyterra::geom_spatraster(data = raster_patches) +
    ggplot2::scale_fill_manual(values = colour_map, na.value = NA) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::theme_sub_panel(
      border = ggplot2::element_rect(
        colour = "grey85"
      )
    ) +
    ggplot2::labs(
      title = glue::glue(
        "Patches of {species_name} habitat"
      ),
      subtitle = glue::glue(
        "# patches: {n_patches}\nBuffer size: {distance}m\n{n_cols} colours"
      )
    )
}


#' Plot connectivity metrics across buffer distances
#'
#' Creates faceted line plots showing how connectivity metrics change with
#' different buffer distances.
#'
#' @param results_connect_habitat Data frame. Connectivity summary results with
#'   columns for species_name, buffer_distance, and various metrics.
#'
#' @returns A ggplot2 object with faceted plots of connectivity metrics.
#'
#' @export
plot_connectivity <- function(results_connect_habitat) {
  geo_cols <- scico::scico(n = 6, palette = "bukavu") |> as.list()

  names(geo_cols) <- c(
    "dark_blue",
    "mid_blue",
    "light_blue",
    "dark_green",
    "tan",
    "offwhite"
  )
  results_connect_habitat |>
    dplyr::select(
      species_name:patch_area_total_ha,
      -effective_mesh_ha
    ) |>
    tidyr::pivot_longer(
      cols = -c(species_name, buffer_distance)
    ) |>
    ggplot2::ggplot(ggplot2::aes(x = buffer_distance, y = value)) +
    ggplot2::geom_point() +
    ggplot2::geom_line(colour = geo_cols$dark_green) +
    ggplot2::facet_wrap(
      ~name,
      scales = "free",
      ncol = 2,
      labeller = ggplot2::labeller(name = to_sentence)
    ) +
    ggplot2::scale_x_continuous(
      breaks = results_connect_habitat$buffer_distance,
      labels = \(x) glue::glue("{x}m")
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::label_number(scale_cut = scales::cut_short_scale())
    ) +
    ggplot2::labs(
      x = "Buffer distance (m)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme_sub_panel(
      border = ggplot2::element_rect(
        colour = "grey85",
        fill = NA
      )
    ) +
    ggplot2::theme(
      text = ggplot2::element_text(size = 14)
    )
}
