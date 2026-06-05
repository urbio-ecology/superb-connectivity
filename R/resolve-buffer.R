#' @noRd
resolve_buffer_radius <- function(
  interpatch_distance = NULL,
  buffer_radius = NULL
) {
  has_id <- !is.null(interpatch_distance)
  has_br <- !is.null(buffer_radius)
  if (has_id && has_br) {
    cli::cli_abort(
      "Specify only one of {.arg interpatch_distance} or {.arg buffer_radius}."
    )
  }
  if (!has_id && !has_br) {
    cli::cli_abort(
      "Specify one of {.arg interpatch_distance} or {.arg buffer_radius}."
    )
  }

  supplied <- if (has_id) "interpatch_distance" else "buffer_radius"
  buffer_radius <- switch(
    supplied,
    interpatch_distance = {
      check_scalar_numeric(interpatch_distance)
      interpatch_distance / 2
    },
    buffer_radius = {
      check_scalar_numeric(buffer_radius)
      buffer_radius
    }
  )
  buffer_radius
}

# warn if the radius can't be represented at this res
#' @noRd
warn_buffer_resolution <- function(buffer_radius, resolution) {
  # terra::focalMat() includes a cell when its CENTRE is within `buffer_radius`,
  # so the number of usable rings is floor(buffer_radius / resolution).
  # (Verified empirically: d=350/res=500 -> 1x1; d=500 -> 3x3; d=600 -> 3x3.)
  # Assumes square cells (callers pass terra::res(habitat)[1]).
  n_rings <- floor(buffer_radius / resolution)

  if (n_rings < 1) {
    cli::cli_warn(c(
      "Can't represent the buffer at a resolution of {resolution}m.",
      "x" = "Buffer radius ({buffer_radius}m) is smaller than one raster \\
      cell.",
      "i" = "This radius corresponds to an {.arg interpatch_distance} of \\
      {buffer_radius * 2}m.",
      "i" = "Gaps between patches aren't bridged; only touching patches are \\
      linked.",
      "i" = "Rule of thumb: keep resolution <= interpatch_distance / 2 (use \\
      finer cells, or a larger interpatch distance).",
      "i" = "See {.vignette urbioconnect::interpatch-distance-and-resolution}."
    ))
    return(invisible())
  }

  effective_radius <- n_rings * resolution
  if (!isTRUE(all.equal(effective_radius, buffer_radius))) {
    cli::cli_warn(c(
      "Buffer radius doesn't align with the raster resolution.",
      "x" = "{buffer_radius} m isn't a multiple of {resolution} m.",
      "i" = "It snaps to {effective_radius} m (interpatch distance \\
      {2 * effective_radius} m).",
      "i" = "Connectivity may shift for patches near the cut-off.",
      "i" = "See {.vignette urbioconnect::interpatch-distance-and-resolution}."
    ))
  }
  invisible()
}
