#' Get metadata for JATOS results
#'
#' @param jc JATOS connection info, e.g. from `define_connection`
#' @param batch_id Integer identifier for a particular experiment batch
#' @param component_id Integer identifier for a particular experiment component
#'
#' Only one of batch_id or component_id should be provided.
#'
#' @return HTTP response object returned by `httr2::req_perform()`. The actual metadata are stored in the response `body` and can be analyzed with `process_results`. The provided `batch_id` and `component_id` are added to the response data.
#' @export
#'
#' @examples
#' \dontrun{
#' cc <- define_connection(
#'   "https://www.myjatosinstance.org",
#'   "Bearer jap_xXxXxXxXxXxXxXxXxXxXxXxXxXxXxXxXxXxXx"
#' )
#' meta <- get_metadata(cc, batch_id = 100)
#' meta$batch_id
#' }
get_metadata <- function(jc, batch_id = NULL, component_id = NULL) {
  req_base <- base_request(jc, "/jatos/api/v1/results/metadata")
  stopifnot(
    "Either batch_id or component_id must be provided" = !(is.null(batch_id) &&
      is.null(component_id))
  )
  stopifnot(
    "Only one of batch_id or component_id must be provided" = (is.null(
      batch_id
    ) ||
      is.null(component_id))
  )
  if (!is.null(batch_id)) {
    req_results <-
      req_base |>
      httr2::req_url_query(batchId = batch_id)
  }
  if (!is.null(component_id)) {
    req_results <-
      req_base |>
      httr2::req_url_query(componentId = component_id)
  }
  resp_results <- req_results |> httr2::req_perform()
  resp_results$batch_id <- batch_id
  resp_results$component_id <- component_id
  resp_results
}
