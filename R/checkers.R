#' @noRd
check_scalar_numeric <- function(
  x,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  if (!is.numeric(x)) {
    cli::cli_abort(
      message = c(
        "{.arg {arg}} must be {.cls numeric}, not {.cls {class(x)}}.",
        "i" = "You supplied: {.obj_type_friendly {x}}"
      ),
      call = call
    )
  }

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

