#' Save barrier habitat buffer plot
#'
#' @param barrier barrier layer
#' @param habitat habitat layer
#' @param buffered buffered layer
#' @param species_name character, species name, e.g., "Superb Fairy Wren"
#' @param col_barrier colour to colour the barrier layer
#' @param col_buffer colour to colour the buffer layer
#' @param col_habitat colour to colour the habitat layer
#' @param col_paper colour to colour the paper layer of ggplot
#' @param distance buffer distance, numeric
#'
#' @return saved ggplot and file path
#' @author njtierney
#' @export
plot_barrier_habitat_buffer <- function(
  barrier,
  buffered,
  habitat,
  distance,
  species_name,
  col_barrier,
  col_buffer,
  col_habitat,
  col_paper
) {
  plot_barrier_habitat <- gg_barrier_habitat_buffer(
    barrier = barrier,
    habitat = habitat,
    buffered = buffered,
    distance = distance,
    species_name = species_name,
    col_barrier = col_barrier,
    col_buffer = col_buffer,
    col_habitat = col_habitat,
    col_paper = col_paper
  )

  plot_barrier_habitat_name <- glue::glue(
    "doc/plot-barrier-buffer-habitat-{species_name}-buffer-{distance}.png"
  )

  ggplot2::ggsave(
    filename = plot_barrier_habitat_name,
    plot = plot_barrier_habitat
  )

  stats::setNames(plot_barrier_habitat_name, distance)
}
