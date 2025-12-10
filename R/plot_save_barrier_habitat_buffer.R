#' Save barrier habitat buffer plot
#' @param barrier barrier layer
#' @param buffer buffer layer
#' @param habitat habitat layer
#' @param distance buffer distance
#' @return saved ggplot and file path
#' @author njtierney
#' @export
plot_save_barrier_habitat_buffer <- function(
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
  plot_barrier_habitat <- plot_barrier_habitat_buffer(
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

  ggplot2::ggsave(filename = plot_barrier_habitat_name, plot = plot_barrier_habitat)

  setNames(plot_barrier_habitat_name, distance)
}
