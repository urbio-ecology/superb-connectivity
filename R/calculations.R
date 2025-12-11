#' Calculate effective mesh size
#'
#' Computes the effective mesh size metric for habitat connectivity, which
#' represents the probability that two randomly chosen points within habitat
#' remain connected.
#'
#' @param area_squared Numeric vector. Squared areas of connected patches.
#' @param area_total Numeric vector. Total areas of connected patches.
#'
#' @returns Numeric. Effective mesh size in hectares.
#'
#' @examples
#' areas <- c(100, 200, 150)
#' effective_mesh_size(areas^2, areas)
#' @export
effective_mesh_size <- function(area_squared, area_total) {
  effective_mesh <- sum(area_squared) / sum(area_total)
  effective_mesh_hectares <- effective_mesh * 0.0001
  effective_mesh_hectares
}

#' Calculate mean patch size
#'
#' @param area_total Numeric vector. Total areas of habitat patches.
#'
#' @returns Numeric. Mean patch size.
#' @export
mean_patch_size <- function(area_total) {
  mean_size <- mean(area_total)
  mean_size
}

#' Count number of habitat patches
#'
#' @param area_total Numeric vector. Total areas of habitat patches.
#'
#' @returns Integer. Number of patches.
#' @export
n_patches <- function(area_total) {
  n_areas <- length(area_total)
  n_areas
}

#' Calculate total habitat area
#'
#' @param area_total Numeric vector. Total areas of habitat patches in square
#'   meters.
#'
#' @returns Numeric. Total habitat area in hectares.
#' @export
total_habitat_area <- function(area_total) {
  total <- sum(area_total)
  total_hectares <- total * 0.0001
  total_hectares
}

#' Calculate connectivity probability
#'
#' Computes the probability that two randomly chosen points within habitat
#' are connected, accounting for fragmentation.
#'
#' @param area_squared Numeric vector. Squared areas of connected patches.
#' @param area_total Numeric vector. Total areas of connected patches.
#'
#' @returns Numeric. Probability of connectedness (0-1).
#' @export
connectivity_probability <- function(area_squared, area_total) {
  total_habitat <- sum(area_total)
  connect_value <- effective_mesh_size(area_squared, area_total)
  prob_connect <- connect_value / total_habitat
  prob_connect
}

#' Summarise connectivity metrics
#'
#' Calculates a comprehensive set of habitat connectivity metrics including
#' effective mesh size, probability of connectedness, and patch statistics.
#'
#' @param area_squared Numeric vector. Squared areas of connected patches.
#' @param area_total Numeric vector. Total areas of connected patches.
#' @param buffer_distance Numeric. Buffer distance used in analysis (meters).
#' @param overlay_resolution Numeric. Overlay resolution in meters.
#' @param base_resolution Numeric. Base resolution in meters.
#' @param aggregation_factor Numeric. Factor by which base resolution was
#'   aggregated.
#' @param species_name Character. Name of species analysed.
#'
#' @returns A tibble with connectivity metrics including number of patches,
#'   probability of connectedness, effective mesh size, mean and total patch
#'   areas.
#' @export
summarise_connectivity <- function(
  area_squared,
  area_total,
  buffer_distance,
  overlay_resolution,
  base_resolution,
  aggregation_factor,
  species_name
) {
  results <- tibble::tibble(
    species_name = species_name,
    buffer_distance = buffer_distance,
    n_patches = n_patches(area_total),
    prob_connectedness = connectivity_probability(area_squared, area_total),
    effective_mesh_ha = effective_mesh_size(area_squared, area_total),
    patch_area_mean = mean_patch_size(area_total),
    patch_area_total_ha = total_habitat_area(area_total),
    overlay_resolution = overlay_resolution,
    base_resolution = base_resolution,
    aggregation_factor = aggregation_factor
  ) |>
    dplyr::mutate(
      prob_connectedness = round(prob_connectedness, 6)
    ) |>
    dplyr::mutate(
      dplyr::across(
        .cols = c(effective_mesh_ha, patch_area_mean, patch_area_total_ha),
        round
      )
    )
  results
}
