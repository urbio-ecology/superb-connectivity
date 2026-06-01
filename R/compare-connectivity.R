#' Compare measurements the connectivity of different scenarios
#'
#' We can measure the connectivity of a given habitat and barrier with
#'   [habitat_connectivity()]. We can also compare the connectivity, say for
#'   example if you have the same area habitat and barrier, but you want to
#'   understand what the change in connectedness is when you remove, or add
#'   some habitat, or some barrier(s), or both. This function help you do that.
#'
#' @inheritParams summarise_connectivity
#' @param area_new new area
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
#'   area_new = new_areas,
#'   area_baseline = baseline_areas,
#'   buffer_distance = 10,
#'   target_resolution = 10,
#'   data_resolution = 10,
#'   aggregation_factor = 10,
#'   species_name = "blue-tongued lizard"
#' )

compare_connectivity <- function(
  area_new,
  area_baseline,
  buffer_distance,
  target_resolution,
  data_resolution,
  aggregation_factor,
  species_name
) {
  baseline <- summarise_connectivity(
    area = area_baseline,
    # area_baseline defaults to area — standalone case
    buffer_distance = buffer_distance,
    target_resolution = target_resolution,
    data_resolution = data_resolution,
    aggregation_factor = aggregation_factor,
    species_name = species_name
  )

  new <- summarise_connectivity(
    area = area_new,
    area_baseline = area_baseline, # comparison: new vs original
    buffer_distance = buffer_distance,
    target_resolution = target_resolution,
    data_resolution = data_resolution,
    aggregation_factor = aggregation_factor,
    species_name = species_name
  )

  numeric_cols <- c(
    "n_patches",
    "effective_mesh_ha",
    "prob_connectedness",
    "patch_area_mean",
    "patch_area_total_ha"
  )
  difference <- new
  difference[numeric_cols] <- baseline[numeric_cols] - new[numeric_cols]

  results <- dplyr::bind_rows(
    baseline = baseline,
    new = new,
    difference = difference,
    .id = "scenario"
  )
  class(results) <- c("compare_connectivity", class(results))
  results
}
