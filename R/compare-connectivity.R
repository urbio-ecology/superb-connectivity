#' Compare measurements the connectivity of different scenarios
#'
#' We can measure the connectivity of a given habitat and barrier with
#'   [habitat_connectivity()]. We can also compare the connectivity, say for
#'   example if you have the same area habitat and barrier, but you want to
#'   understand what the change in connectedness is when you remove, or add
#'   some habitat, or some barrier(s), or both. This function help you do that.
#'
#' @param new_area_squared Numeric vector. Squared areas of the new connected
#'   areas.
#' @param new_area Numeric vector. Total areas of new connected patch areas.
#' @param baseline_area Numeric vector. Total areas of original connected patch
#'   areas.
#' @param buffer_distance Numeric. Buffer distance used in analysis (meters).
#' @param species_name Character. Name of species analysed.
#'
#' @returns
#' @export
#'
#' @examples
compare_connectivity <- function(
  new_area_squared,
  new_area,
  baseline_area,
  buffer_distance,
  species_name
) {
  results <- tibble::tibble(
    species_name = species_name,
    buffer_distance = buffer_distance,
    n_patches = n_patches(new_area),
    effective_mesh_ha = effective_mesh_size(
      new_area_squared,
      baseline_area
    ),
    prob_connectedness = connectivity_probability(
      effective_mesh_size = effective_mesh_ha,
      area_total = baseline_area
    )
  ) |>
    dplyr::mutate(
      prob_connectedness = round(prob_connectedness, 6),
      effective_mesh_ha = round(effective_mesh_ha)
    )

  results
}
