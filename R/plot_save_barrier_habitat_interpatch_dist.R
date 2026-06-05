#' Save barrier habitat interpatch distance plot
#'
#' Saved a plot created by [gg_barrier_habitat_interpatch_dist()] to file.
#'
#' @param barrier barrier layer
#' @param habitat habitat layer
#' @param buffered buffered layer
#' @param species character, species name, e.g., "Superb Fairy Wren"
#' @param col_barrier colour to colour the barrier layer
#' @param col_interpatch_dist colour to colour the interpatch distance layer
#' @param col_habitat colour to colour the habitat layer
#' @param col_paper colour to colour the paper layer of ggplot
#' @param interpatch_distance Numeric. The distance (in meters) where habitat
#'   patches are considered connected. E.g., if set to 500, patches 498m apart
#'   are connected, those 501m apart are not connected. This is passed
#'   internally to a spatial operation known as "buffering", where this
#'   distance is used as a radius from the edge of the habitat zone. This means
#'   the specified `interpatch_distance` is halved exactly. So an interpatch
#'   distance of 500 will be converted to 250.
#'
#' @returns Named character vector. The file path, named by the interpatch
#'   distance.
#' @examples
#' \dontrun{
#' lizard_habitat <- example_habitat()
#' lizard_barrier <- example_barrier()
#' buffered <- habitat_buffer(lizard_habitat, interpatch_distance = 10)
#' # Creates plot-barrier-interpatch-dist-habitat-*.png in the working directory
#' plot_barrier_habitat_interpatch_dist(
#'   barrier = lizard_barrier,
#'   buffered = buffered,
#'   habitat = lizard_habitat,
#'   interpatch_distance = 10,
#'   species = "Blue-tongued Lizard",
#'   col_barrier = "white",
#'   col_interpatch_dist = "lightgreen",
#'   col_habitat = "seagreen",
#'   col_paper = "grey50"
#' )
#' }
#' @export
plot_barrier_habitat_interpatch_dist <- function(
  barrier,
  buffered,
  habitat,
  interpatch_distance,
  species,
  col_barrier,
  col_interpatch_dist,
  col_habitat,
  col_paper
) {
  plot_barrier_habitat <- gg_barrier_habitat_interpatch_dist(
    barrier = barrier,
    habitat = habitat,
    buffered = buffered,
    interpatch_distance = interpatch_distance,
    species = species,
    col_barrier = col_barrier,
    col_interpatch_dist = col_interpatch_dist,
    col_habitat = col_habitat,
    col_paper = col_paper
  )

  plot_barrier_habitat_name <- glue::glue(
    "plot-barrier-interpatch-distance-habitat-{species}-\\
    -{interpatch_distance}.png"
  )

  ggplot2::ggsave(
    filename = plot_barrier_habitat_name,
    plot = plot_barrier_habitat
  )

  stats::setNames(plot_barrier_habitat_name, interpatch_distance)
}
