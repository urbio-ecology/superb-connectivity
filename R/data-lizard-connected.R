#' Connected habitat patch areas for Blue-tongued Lizard
#'
#' Pre-computed output of [habitat_connectivity()] run on the lizard example
#' data at a 50 metre buffer distance. Contains one row per connected habitat
#' patch.
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{patch_id}{Integer. Connected fragment ID.}
#'   \item{area}{Numeric. Total area of the connected patch in square metres.}
#'   \item{area_squared}{Numeric. Squared area, used in connectivity metrics.}
#' }
#' @source Generated from [example_habitat()] and [example_barrier()] at
#'   50 metre buffer distance.
#' @seealso [habitat_connectivity()], [summarise_connectivity()]
"lizard_areas_connected"
#'
#' @examples
#' # This was the code that was run to create this object. We don't run it
#' # as it takes some time to run
#' \dontrun{
#' lizard_areas_connected <- habitat_connectivity(
#'     habitat = example_habitat(),
#'     barrier = example_barrier(),
#'     distance = 50,
#'     verbose = FALSE
#'   )
#' }
#' lizard_areas_connected
#' @keywords datasets
"lizard_areas_connected"
