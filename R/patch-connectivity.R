#' A set of connected habitat patches
#'
#' The object returned by [habitat_connectivity()]: a [tibble][tibble::tibble]
#' of connected patch areas that also carries the `species` and
#' `interpatch_distance` the analysis was run with as attributes.
#'
#' Because it is a tibble subclass it behaves like a data frame directly --
#' `$`, `[`, [DT::datatable()], [utils::write.csv()] and ggplot2 all work
#' without ceremony. Read the metadata back with [pc_species()] and
#' [pc_interpatch_distance()].
#'
#' @param data Data frame of connected patches. Must contain an `area` column.
#' @param species Character of length 1. Species the analysis was run for.
#' @param res resolution in pixels - defaults to NA, not required for vector
#'   based approaches.
#' @param interpatch_distance Numeric of length 1. The interpatch distance (m)
#'   the analysis used.
#' @returns A `patch_connectivity` object: a tibble with `species` and
#'   `interpatch_distance` attributes.
#' @export
patch_connectivity <- function(data, species, interpatch_distance, res = NA) {
  check_scalar_character(species)
  check_scalar_numeric(interpatch_distance)

  if (!"area" %in% names(data)) {
    cli::cli_abort("{.arg data} must contain an {.field area} column.")
  }

  vctrs::new_data_frame(
    # tibble::as_tibble(data),
    x = list(
      patch_id = data$patch_id,
      area = data$area
    ),
    species = species,
    interpatch_distance = interpatch_distance,
    res = res,
    patches = nrow(data),
    n = nrow(data),
    class = c("patch_connectivity", "tbl_df", "tbl")
  )
}

#' @export
print.patch_connectivity <- function(x, ..., n = NULL) {
  NextMethod(n = n %||% 5)
}

#' @export
tbl_sum.patch_connectivity <- function(x) {
  c(
    "patch_connectivity" = "data.frame",
    "Species" = pc_species(x),
    "Patches" = pc_patches(x),
    "Resolution" = paste(pc_res(x), collapse = "x"),
    "Interpatch Distance" = paste(pc_interpatch_distance(x), "m")
  )
}

#' Metadata from a `patch_connectivity` object
#'
#' @param x A [patch_connectivity()] object.
#' @returns
#'  * `pc_species()` Returns the species (character, length 1);
#'  * `pc_interpatch_distance()` returns the interpatch distance (numeric,
#'   length 1).
#'  * `pc_res()` returns the resolution (character, length 1 - e.g., "2x2"),
#'  * `pc_patches()` returns the number of patches (numeric, length 1)
#' @name pc-getters
#' @export
pc_species <- function(x) {
  attr(x, "species")
}

#' @rdname pc-getters
#' @export
pc_patches <- function(x) {
  attr(x, "patches")
}

#' @rdname pc-getters
#' @export
pc_res <- function(x) {
  x_res <- attr(x, "res")
  paste(x_res, collapse = "x")
}

#' @rdname pc-getters
#' @export
pc_interpatch_distance <- function(x) {
  attr(x, "interpatch_distance")
}
