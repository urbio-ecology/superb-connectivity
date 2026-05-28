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
#'
#' @returns tibble with "scenario", "n_patches", "effective_mesh", and
#'   "prob_connectedness".
#' @export
#'
#' @examples
#' # for demonstration purposes - let's imagine the area decreases by 20%
#' baseline_areas <- round(lizard_areas_connected$area)
#' new_areas <- baseline_areas[-1] * 0.8
#' compare_connectivity(
#'   new_area = new_areas,
#'   baseline_area = baseline_areas
#' )
compare_connectivity <- function(
  new_area,
  baseline_area
) {
  new_area_squared <- new_area * new_area

  baseline_results <- tibble::tibble(
    n_patches = n_patches(baseline_area),
    effective_mesh = effective_mesh_size(
      area = baseline_area,
      area_squared = baseline_area * baseline_area
    ),
    prob_connectedness = connectivity_probability(
      effective_mesh_size = effective_mesh,
      area = baseline_area
    )
  )

  new_results <- tibble::tibble(
    n_patches = n_patches(new_area),
    effective_mesh = effective_mesh_size(
      # this is the original
      area = baseline_area,
      # this is the new
      area_squared = new_area_squared
    ),
    prob_connectedness = connectivity_probability(
      effective_mesh_size = effective_mesh,
      area = baseline_area
    )
  )

  results <- dplyr::bind_rows(
    baseline = baseline_results,
    new = new_results,
    .id = "scenario"
  ) |>
    dplyr::mutate(
      prob_connectedness = round(prob_connectedness, 6),
      effective_mesh = round(effective_mesh)
    )

  results
}
