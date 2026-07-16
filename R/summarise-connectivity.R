#' Summarise connectivity metrics
#'
#' Calculates a comprehensive set of habitat connectivity metrics including
#' effective mesh size, probability of connectedness, and patch statistics.
#' Intended for usage from objects created by [habitat_connectivity()], but
#' raw area vectors can be passed. See examples below.
#'
#' @name summarise-connectivity
#'
#' @param connectivity data.frame of class "patch_size", obtained from
#'   [habitat_connectivity()]. Contains area measurements of connected patches.
#' @param connectivity_baseline Optional. data.frame of class
#'   "patch_size", obtained from [habitat_connectivity()]. Contains
#'   baseline area measurements of connected patches. Default is NULL.
#' @param ... extra arguments to pass through for default method.
#'
#' @returns A tibble with connectivity metrics including number of patches,
#'   probability of connectedness, effective mesh size, mean and total patch
#'   areas.
#' @examples
#' summarise_connectivity(
#'   connectivity = lizard_areas_connected$area,
#'   interpatch_distance = 10,
#'   data_resolution = 10,
#'   species = "Blue-tongued Lizard"
#' )
#' @export
summarise_connectivity <- function(
  connectivity,
  connectivity_baseline = NULL,
  ...
) {
  UseMethod("summarise_connectivity")
}

#' @export
summarise_connectivity.patch_size <- function(
  connectivity,
  connectivity_baseline = NULL,
  ...
) {
  # check distance, species, and res match
  check_pc_match(connectivity, connectivity_baseline)
  connectivity_baseline <- connectivity_baseline %||% connectivity

  interpatch_distance <- pc_interpatch_distance(connectivity)
  data_resolution <- pc_res(connectivity)
  species <- pc_species(connectivity)

  connectivity_area <- connectivity$area
  connectivity_area_baseline <- connectivity_baseline$area

  result <- connectivity_metrics(
    area = connectivity_area,
    area_baseline = connectivity_area_baseline
  )

  extras <- tibble::tibble(
    interpatch_distance = interpatch_distance,
    species = species,
    patch_area_mean = mean_patch_size(connectivity_area),
    patch_area_total_ha = total_habitat_area(connectivity_area),
    data_resolution = data_resolution,
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
      interpatch_distance,
      .before = dplyr::everything()
    )
  full_results
}

#' @rdname summarise-connectivity
#' @param interpatch_distance Numeric. The distance (in meters) where habitat
#'   patches are considered connected. E.g., if set to 500, patches 498m apart
#'   are connected, those 501m apart are not connected. This is passed
#'   internally to a spatial operation known as "buffering", where this
#'   distance is used as a radius from the edge of the habitat zone. This means
#'   the specified `interpatch_distance` is halved exactly. So an interpatch
#'   distance of 500 will be converted to 250.
#' @param data_resolution Numeric. Data resolution in meters.
#' @param species Character. Name of species analysed.
#' @export
summarise_connectivity.default <- function(
  connectivity,
  connectivity_baseline = NULL,
  interpatch_distance,
  data_resolution,
  species,
  ...
) {
  connectivity_baseline <- connectivity_baseline %||% connectivity
  result <- connectivity_metrics(
    area = connectivity,
    area_baseline = connectivity_baseline
  )

  extras <- tibble::tibble(
    interpatch_distance = interpatch_distance,
    species = species,
    patch_area_mean = mean_patch_size(connectivity),
    patch_area_total_ha = total_habitat_area(connectivity),
    data_resolution = data_resolution,
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
      interpatch_distance,
      .before = dplyr::everything()
    )
  full_results
}
