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

  ggsave(filename = plot_barrier_habitat_name, plot = plot_barrier_habitat)

  setNames(plot_barrier_habitat_name, distance)
}
