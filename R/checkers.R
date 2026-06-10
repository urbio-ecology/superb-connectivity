check_class <- function(
  x,
  class_predicate,
  class_name,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  if (!class_predicate(x)) {
    cli::cli_abort(
      message = c(
        "{.arg {arg}} must be {.cls {class_name}}, not {.cls {class(x)}}.",
        "i" = "You supplied: {.obj_type_friendly {x}}"
      ),
      call = call
    )
  }
  invisible(x)
}

check_numeric <- function(
  x,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  check_class(x, is.numeric, "numeric")
}

check_character <- function(
  x,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  check_class(x, is.character, "character")
}

check_scalar <- function(
  x,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  if (length(x) != 1) {
    cli::cli_abort(
      message = c(
        "{.arg {arg}} must be a scalar (length 1), not length {length(x)}.",
        "i" = "Did you mean to pass a single value?"
      ),
      call = call
    )
  }
  invisible(x)
}

#' @noRd
check_scalar_numeric <- function(
  x,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  check_numeric(x, arg, call)
  check_scalar(x, arg, call)

  invisible(x)
}

check_scalar_character <- function(
  x,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  check_character(x, arg, call)
  check_scalar(x, arg, call)
  invisible(x)
}

make_bullets <- function(category, new, baseline) {
  c(
    paste0("{.strong ", category, "}"),
    "*" = paste0("connectivity          = {.val ", new, "}"),
    "*" = paste0("connectivity_baseline = {.val ", baseline, "}")
  )
}

# check distance, species, and res match
check_pc_match <- function(
  connectivity,
  connectivity_baseline,
  call = rlang::caller_env()
) {
  if (is.null(connectivity_baseline)) {
    return(invisible())
  }
  connectivity_dist <- pc_interpatch_distance(connectivity)
  baseline_dist <- pc_interpatch_distance(connectivity_baseline)
  connectivity_res <- pc_res(connectivity)
  baseline_res <- pc_res(connectivity_baseline)
  connectivity_species <- pc_species(connectivity)
  baseline_species <- pc_species(connectivity_baseline)

  dist_match <- isTRUE(all.equal(connectivity_dist, baseline_dist))
  res_match <- isTRUE(all.equal(connectivity_res, baseline_res))
  species_match <- isTRUE(all.equal(connectivity_species, baseline_species))

  if (dist_match && res_match && species_match) {
    return(invisible())
  }

  cli::cli_abort(
    message = c(
      "{.arg connectivity} and {.arg connectivity_baseline} must have the same
       resolution, species, and interpatch_distance.",
      "!" = "One or more of these do not match:",
      if (!res_match) {
        make_bullets("resolution", connectivity_res, baseline_res)
      },
      if (!species_match) {
        make_bullets("species", connectivity_species, baseline_species)
      },
      if (!dist_match) {
        make_bullets("interpatch_distance", connectivity_dist, baseline_dist)
      }
    ),
    call = call
  )
}

check_names <- function(
  x,
  name,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  name_exists <- rlang::has_name(x, name)
  if (!name_exists) {
    cli::cli_abort(
      message = "{.arg x} must contain a {.field {name}} column.",
      call = call
    )
  }
}
