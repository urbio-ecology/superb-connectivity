#' Compare measurements the connectivity of different scenarios
#'
#' We can measure the connectivity of a given habitat and barrier with
#'   [habitat_connectivity()]. We can also compare the connectivity, say for
#'   example if you have the same area habitat and barrier, but you want to
#'   understand what the change in connectedness is when you remove, or add
#'   some habitat, or some barrier(s), or both. This function help you do that.
#'
#' @param area_new Numeric vector. Area of a connected patch.
#' @param area_baseline Numeric vector. Baseline area of a connected patch.
#' @param interpatch_distance Numeric. The distance (in meters) where habitat
#'   patches are considered connected. E.g., if set to 500, patches 498m apart
#'   are connected, those 501m apart are not connected. This is passed
#'   internally to a spatial operation known as "buffering", where this
#'   distance is used as a radius from the edge of the habitat zone. This means
#'   the specified `interpatch_distance` is halved exactly. So an interpatch
#'   distance of 500 will be converted to 250.
#' @param species name of species
#'
#' @returns tibble with "scenario", "interpatch_distance", "species",
#'   "n_patches", "effective_mesh_ha", and "prob_connectedness".
#' @export
#'
#' @examples
#' # for demonstration purposes - let's imagine the area decreases by 20%
#' baseline_areas <- round(lizard_areas_connected$area)
#' new_areas <- baseline_areas[-1] * 0.8
#' compare_connectivity(
#'   area_new = new_areas,
#'   area_baseline = baseline_areas,
#'   interpatch_distance = 10,
#'   species = "blue-tongued lizard"
#' )
compare_connectivity <- function(
  area_new,
  area_baseline,
  interpatch_distance,
  species
) {
  baseline <- connectivity_metrics(area = area_baseline)
  new <- connectivity_metrics(area = area_new, area_baseline = area_baseline)
  # (column-wise subtraction; direction is: baseline - new
  # positive = more in new vs baseline, connectivity increased in new scenario)
  difference <- new - baseline

  results <- dplyr::bind_rows(
    baseline = baseline,
    new = new,
    difference = difference,
    .id = "scenario"
  ) |>
    dplyr::mutate(
      interpatch_distance = interpatch_distance,
      species = species,
      .after = scenario
    )
  class(results) <- c("compare_connectivity", class(results))

  results
}
