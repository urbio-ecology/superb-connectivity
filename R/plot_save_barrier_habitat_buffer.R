#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param barrier
#' @param buffer
#' @param habitat
#' @param distance
#' @return
#' @author njtierney
#' @export
plot_save_barrier_habitat_buffer <- function(
  barrier,
  buffer,
  habitat,
  distance,
  species_name
) {
  geo_cols <- scico(n = 6, palette = "bukavu") |> as.list()

  names(geo_cols) <- c(
    "dark_blue",
    "mid_blue",
    "light_blue",
    "dark_green",
    "tan",
    "offwhite"
  )
  col_barrier <- geo_cols$mid_blue
  col_habitat <- geo_cols$dark_green
  col_buffer <- geo_cols$tan

  # First, reclassify your rasters to assign actual color values
  barrier_coloured <- subst(barrier, 1, col_barrier)
  buffer_coloured <- subst(buffer, 1, col_buffer)
  habitat_coloured <- subst(habitat, 1, col_habitat)

  # Now plot them in layers (bottom to top)
  plot_barrier_habitat <- ggplot() +
    geom_spatraster(data = buffer_coloured) +
    geom_spatraster(data = barrier_coloured) +
    geom_spatraster(data = habitat_coloured) +
    theme_minimal(paper = geo_cols$offwhite) +
    scale_fill_identity(na.value = NA) +
    labs(
      title = marquee_glue(
        "{.{col_habitat} Habitat}, buffered {.{col_buffer} habitat}, and {.{col_barrier} barriers} for {.bold {species_name}}"
      )
    ) +
    theme(plot.title = element_marquee()) +
    labs(subtitle = glue("{distance}m buffer"))

  plot_barrier_habitat_name <- glue::glue(
    "doc/plot-barrier-buffer-habitat-{species_name}-buffer-{distance}.png"
  )

  ggsave(filename = plot_barrier_habitat_name, plot = plot_barrier_habitat)

  plot_barrier_habitat_name
}
