#' Calculate effective mesh size
#'
#' Computes the effective mesh size metric for habitat connectivity, This
#'   represents the probability that two randomly chosen points within habitat
#'   remain connected. Intended for usage from objects created by
#'   [habitat_connectivity()]. See examples below.
#'
#' @param area_squared Numeric vector. Squared area of connected patches.
#' @param area Numeric vector. Area of a connected patch.
#'
#' @returns Numeric. Effective mesh size in hectares.
#'
#' @examples
#' effective_mesh_size(lizard_areas_connected$area_squared, lizard_areas_connected$area)
#' @export
effective_mesh_size <- function(area_squared, area) {
  effective_mesh <- sum(area_squared) / sum(area)
  effective_mesh_hectares <- effective_mesh * 0.0001
  effective_mesh_hectares
}

#' Calculate mean patch size
#'
#' This is just a wrapper around `mean()`, however it is written to clearly
#'   identify it's usage in the context of the area data. Intended for usage
#'   from objects created by [habitat_connectivity()]. See examples below.
#'
#' @param area Numeric vector. Area of a connected patch.
#' @param ... extra arguments to pass to `mean()`.
#'
#' @returns Numeric. Mean patch size.
#' @examples
#' mean_patch_size(lizard_areas_connected$area)
#' @export
mean_patch_size <- function(area, ...) {
  mean_size <- mean(area, ...)
  mean_size
}

#' Count number of habitat patches
#'
#' Identify the number of habitat patches. A wrapper around `length()`, but
#'   named to establish its context. Intended for usage from objects created by
#'   [habitat_connectivity()]. See examples below.
#'
#' @param area Numeric vector. Area of a connected patch.
#'
#' @returns Integer. Number of patches.
#' @examples
#' n_patches(lizard_areas_connected$area)
#' @export
n_patches <- function(area) {
  n_areas <- length(area)
  n_areas
}

#' Calculate total habitat area
#'
#' Calculate the total habitat area in hectars. A wrapper around summing the
#'   area and multiplying by 0.0001 to give the units in hectares. Intended
#'   for usage from objects created by [habitat_connectivity()]. See examples
#'   below.
#'
#' @param area Numeric vector. Area of a connected patch.
#'
#' @returns Numeric. Total habitat area in hectares.
#' @examples
#' total_habitat_area(lizard_areas_connected$area)
#' @export
total_habitat_area <- function(area) {
  total <- sum(area)
  total_hectares <- total * 0.0001
  total_hectares
}

#' Calculate connectivity probability
#'
#' Computes the probability two randomly chosen points within habitat
#' are connected, accounting for fragmentation. This requires the effective
#' mesh size (via [effective_mesh_size()]), and the area of patches. This means
#' that you can calculate the change in connectivity if you calculate the
#' effective mesh size of a new habitat/barrier plan, and then use the baseline
#'
#' @param effective_mesh_size As calculated by [effective_mesh_size()]
#' @param area Numeric vector. Area of a connected patch.
#'
#' @returns Numeric. Probability of connectedness (0-1).
#' @examples
#' effective_mesh <- effective_mesh_size(
#'   area_squared = lizard_areas_connected$area_squared,
#'   area = lizard_areas_connected$area
#'   )
#' connectivity_probability(
#'   effective_mesh_size = effective_mesh,
#'   area = lizard_areas_connected$area
#'   )
#' # if you wanted to compare to a scenario, you would consider the effective
#' # mesh size to be the new scenario level, and the baseline would be "area"
#' connectivity_probability(
#' # scenario 1
#'   effective_mesh_size = effective_mesh,
#' # baseline
#'   area = lizard_areas_connected$area
#'   )
#' @export
connectivity_probability <- function(effective_mesh_size, area) {
  total_habitat <- sum(area)
  prob_connect <- effective_mesh_size / total_habitat
  prob_connect
}

#' Summarise connectivity metrics
#'
#' Calculates a comprehensive set of habitat connectivity metrics including
#' effective mesh size, probability of connectedness, and patch statistics.
#' Intended for usage from objects created by [habitat_connectivity()].
#' See examples below.
#'
#' @param area_squared Numeric vector. Squared areas of connected patches.
#' @param area Numeric vector. Area of a connected patch.
#' @param buffer_distance Numeric. Buffer distance used in analysis (meters).
#' @param target_resolution Numeric. Target resolution in meters.
#' @param data_resolution Numeric. Data resolution in meters.
#' @param aggregation_factor Numeric. Factor by which Data resolution was
#'   aggregated.
#' @param species_name Character. Name of species analysed.
#'
#' @returns A tibble with connectivity metrics including number of patches,
#'   probability of connectedness, effective mesh size, mean and total patch
#'   areas.
#' @examples
#' summarise_connectivity(
#'   area_squared = lizard_areas_connected$area_squared,
#'   area = lizard_areas_connected$area,
#'   buffer_distance = 10,
#'   target_resolution = 500,
#'   data_resolution = 10,
#'   aggregation_factor = 50,
#'   species_name = "Blue-tongued Lizard"
#' )
#' @export
summarise_connectivity <- function(
  area_squared,
  area,
  buffer_distance,
  target_resolution,
  data_resolution,
  aggregation_factor,
  species_name
) {
  results <- tibble::tibble(
    species_name = species_name,
    buffer_distance = buffer_distance,
    n_patches = n_patches(area),
    effective_mesh_ha = effective_mesh_size(area_squared, area),
    prob_connectedness = connectivity_probability(effective_mesh_ha, area),
    patch_area_mean = mean_patch_size(area),
    patch_area_total_ha = total_habitat_area(area),
    target_resolution = target_resolution,
    data_resolution = data_resolution,
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
