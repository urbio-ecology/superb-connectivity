#' Connected habitat patch areas for Blue-tongued Lizard
#'
#' Pre-computed per-patch areas for the lizard example data at a 50 metre
#' interpatch distance. This is a `patch_size_tbl` -- the per-patch table
#' carried inside the `connectivity` object returned by
#' [habitat_connectivity()], extracted with the [patch_sizes()] accessor.
#' Contains one row per connected habitat patch.
#'
#' @format A `patch_size_tbl` with columns:
#' \describe{
#'   \item{patch_id}{Integer. Connected fragment ID.}
#'   \item{area}{Numeric. Total area of the connected patch in square metres.}
#' }
#' @source Generated from [example_habitat()] and [example_barrier()] at
#'   50 metre interpatch distance.
#' @seealso [habitat_connectivity()], [patch_sizes()], [summarise_connectivity()]
#' @examples
#' # This was the code that was run to create this object. We don't run it
#' # as it takes some time to run
#' \dontrun{
#' lizard_connectivity <- habitat_connectivity(
#'     habitat = example_habitat(),
#'     barrier = example_barrier(),
#'     species = "Blue-tongued Lizard",
#'     interpatch_distance = 50,
#'     verbose = FALSE
#'   )
#' lizard_areas_connected <- patch_sizes(lizard_connectivity)[[1]]
#' }
#' lizard_areas_connected
#' @keywords datasets
"lizard_areas_connected"
