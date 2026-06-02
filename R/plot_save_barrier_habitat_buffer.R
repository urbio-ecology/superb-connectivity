#' Save barrier habitat buffer plot
#'
#' Saved a plot created by [gg_barrier_habitat_buffer()] to file.
#'
#' @param barrier barrier layer
#' @param habitat habitat layer
#' @param buffered buffered layer
#' @param species character, species name, e.g., "Superb Fairy Wren"
#' @param col_barrier colour to colour the barrier layer
#' @param col_buffer colour to colour the buffer layer
#' @param col_habitat colour to colour the habitat layer
#' @param col_paper colour to colour the paper layer of ggplot
#' @param interpatch_distance interpatch distance, numeric
#'
#' @returns Named character vector. The file path, named by the interpatch
#'   distance.
#' @examples
#' \dontrun{
#' lizard_habitat <- example_habitat()
#' lizard_barrier <- example_barrier()
#' buffered <- habitat_buffer(lizard_habitat, interpatch_distance = 10)
#' # Creates plot-barrier-buffer-habitat-*.png in the working directory
#' plot_barrier_habitat_buffer(
#'   barrier = lizard_barrier,
#'   buffered = buffered,
#'   habitat = lizard_habitat,
#'   interpatch_distance = 10,
#'   species = "Blue-tongued Lizard",
#'   col_barrier = "white",
#'   col_buffer = "lightgreen",
#'   col_habitat = "seagreen",
#'   col_paper = "grey50"
#' )
#' }
#' @export
plot_barrier_habitat_buffer <- function(
  barrier,
  buffered,
  habitat,
  interpatch_distance,
  species,
  col_barrier,
  col_buffer,
  col_habitat,
  col_paper
) {
  plot_barrier_habitat <- gg_barrier_habitat_buffer(
    barrier = barrier,
    habitat = habitat,
    buffered = buffered,
    interpatch_distance = interpatch_distance,
    species = species,
    col_barrier = col_barrier,
    col_buffer = col_buffer,
    col_habitat = col_habitat,
    col_paper = col_paper
  )

  plot_barrier_habitat_name <- glue::glue(
    "plot-barrier-buffer-habitat-{species}-buffer-{interpatch_distance}.png"
  )

  ggplot2::ggsave(
    filename = plot_barrier_habitat_name,
    plot = plot_barrier_habitat
  )

  stats::setNames(plot_barrier_habitat_name, interpatch_distance)
}
