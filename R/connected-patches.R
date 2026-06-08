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
#' @param interpatch_distance Numeric of length 1. The interpatch distance (m)
#'   the analysis used.
#' @returns A `patch_connectivity` object: a tibble with `species` and
#'   `interpatch_distance` attributes.
#' @export
patch_connectivity <- function(data, species, interpatch_distance) {
  if (!rlang::is_scalar_character(species)) {
    cli::cli_abort(
      "{.arg species} must be a character vector of length 1, not \\
      {.obj_type_friendly {species}} of length {length(species)}."
    )
  }
  if (!is.numeric(interpatch_distance) || length(interpatch_distance) != 1) {
    cli::cli_abort(
      "{.arg interpatch_distance} must be a numeric vector of length 1, not \\
      {.obj_type_friendly {interpatch_distance}} of length \\
      {length(interpatch_distance)}."
    )
  }
  if (!"area" %in% names(data)) {
    cli::cli_abort("{.arg data} must contain an {.field area} column.")
  }

  tibble::new_tibble(
    tibble::as_tibble(data),
    species = species,
    interpatch_distance = interpatch_distance,
    nrow = nrow(data),
    class = "patch_connectivity"
  )
}

#' Metadata from a `patch_connectivity` object
#'
#' @param x A [patch_connectivity()] object.
#' @returns `pc_species()` returns the species (character, length 1);
#'   `pc_interpatch_distance()` returns the interpatch distance (numeric,
#'   length 1).
#' @export
pc_species <- function(x) {
  attr(x, "species")
}

#' @rdname pc_species
#' @export
pc_interpatch_distance <- function(x) {
  attr(x, "interpatch_distance")
}

#' @export
print.patch_connectivity <- function(x, ...) {
  cli::cli_text(
    "{.cls patch_connectivity} for {.val {pc_species(x)}} at \\
    {pc_interpatch_distance(x)}m interpatch distance"
  )
  NextMethod()
  invisible(x)
}
