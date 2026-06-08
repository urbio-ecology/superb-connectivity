#' Summarise connectivity metrics
#'
#' Calculates a comprehensive set of habitat connectivity metrics including
#' effective mesh size, probability of connectedness, and patch statistics.
#' Intended for usage from objects created by [habitat_connectivity()].
#' See examples below.
#'
#' @param area Numeric vector. Areas of connected patches.
#' @param area_baseline Numeric vector. Areas of connected patch baseline.
#' @param interpatch_distance Numeric. The distance (in meters) where habitat
#'   patches are considered connected. E.g., if set to 500, patches 498m apart
#'   are connected, those 501m apart are not connected. This is passed
#'   internally to a spatial operation known as "buffering", where this
#'   distance is used as a radius from the edge of the habitat zone. This means
#'   the specified `interpatch_distance` is halved exactly. So an interpatch
#'   distance of 500 will be converted to 250.
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
#'   interpatch_distance = 10,
#'   target_resolution = 500,
#'   data_resolution = 10,
#'   aggregation_factor = 50,
#'   species = "Blue-tongued Lizard"
#' )
#' @export
summarise_connectivity <- function(area, ...) {
  UseMethod("summarise_connectivity")
}

#' @export
summarise_connectivity.patch_connectivity <- function(area, ...) {
  # + bind metadata off pc_species()/pc_interpatch_distance()
  connectivity_metrics(area$area)
}

#' @export
summarise_connectivity.default <- function(area, area_baseline = area, ...) {
  # numeric vector entry point
  connectivity_metrics(area, area_baseline)
}

#' @export
compare_connectivity <- function(area_new, ...) {
  UseMethod("compare_connectivity")
}

#' @export
compare_connectivity.patch_connectivity <- function(
  area_new,
  area_baseline,
  ...
) {
  if (!identical(pc_species(area_new), pc_species(area_baseline))) {
    cli::cli_abort("Scenarios must be the same species.")
  }
  # summarise each, diff
}

# summarise_connectivity <- function(
#   area,
#   area_baseline = NULL,
#   interpatch_distance,
#   target_resolution,
#   data_resolution,
#   aggregation_factor,
#   species
# ) {
#   area_baseline <- area_baseline %||% area
#   result <- connectivity_metrics(
#     area = area,
#     area_baseline = area_baseline
#   )
#
#   extras <- tibble::tibble(
#     interpatch_distance = interpatch_distance,
#     species = species,
#     patch_area_mean = mean_patch_size(area),
#     patch_area_total_ha = total_habitat_area(area),
#     target_resolution = target_resolution,
#     data_resolution = data_resolution,
#     aggregation_factor = aggregation_factor
#   )
#
#   full_results <- dplyr::bind_cols(
#     result,
#     extras
#   ) |>
#     dplyr::mutate(
#       prob_connectedness = round(prob_connectedness, 6)
#     ) |>
#     dplyr::mutate(
#       dplyr::across(
#         .cols = c(effective_mesh_ha, patch_area_mean, patch_area_total_ha),
#         round
#       )
#     ) |>
#     dplyr::relocate(
#       species,
#       interpatch_distance,
#       .before = dplyr::everything()
#     )
#   full_results
# }
