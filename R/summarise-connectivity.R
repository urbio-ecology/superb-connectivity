#' Summarise connectivity metrics
#'
#' Calculates a comprehensive set of habitat connectivity metrics including
#' effective mesh size, probability of connectedness, and patch statistics.
#' Intended for usage from objects created by [habitat_connectivity()].
#' See examples below.
#'
#' @param area Numeric vector. Areas of connected patches.
#' @param area_baseline Numeric vector. Areas of connected patch baseline.
#' @param distance Numeric. Buffer distance used in analysis (meters).
#' @param target_resolution Numeric. Target resolution in meters.
#' @param data_resolution Numeric. Data resolution in meters.
#' @param aggregation_factor Numeric. Factor by which Data resolution was
#'   aggregated.
#' @param species Character. Name of species analysed.
#'
#' @returns A tibble with connectivity metrics including number of patches,
#'   probability of connectedness, effective mesh size, mean and total patch
#'   areas.
#' @examples
#' summarise_connectivity(
#'   area = lizard_areas_connected$area,
#'   distance = 10,
#'   target_resolution = 500,
#'   data_resolution = 10,
#'   aggregation_factor = 50,
#'   species = "Blue-tongued Lizard"
#' )
#' @export
summarise_connectivity <- function(
  area,
  area_baseline = NULL,
  distance,
  target_resolution,
  data_resolution,
  aggregation_factor,
  species
) {
  area_baseline <- area_baseline %||% area
  result <- connectivity_metrics(
    area = area,
    area_baseline = area_baseline
  )

  extras <- tibble::tibble(
    distance = distance,
    species = species,
    patch_area_mean = mean_patch_size(area),
    patch_area_total_ha = total_habitat_area(area),
    target_resolution = target_resolution,
    data_resolution = data_resolution,
    aggregation_factor = aggregation_factor
  )

  full_results <- dplyr::bind_cols(
    result,
    extras
  ) |>
    dplyr::mutate(
      prob_connectedness = round(prob_connectedness, 6)
    ) |>
    dplyr::mutate(
      dplyr::across(
        .cols = c(effective_mesh_ha, patch_area_mean, patch_area_total_ha),
        round
      )
    ) |>
    dplyr::relocate(
      species,
      distance,
      .before = everything()
    )
  full_results
}
