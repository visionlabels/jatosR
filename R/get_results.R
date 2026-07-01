#' Get raw data for JATOS results
#'
#' @param jc JATOS connection info, e.g. from `define_connection`
#' @param batch_id Integer identifier for a particular experiment batch
#' @param component_id Integer identifier for a particular experiment component
#' @param filename Optional name of the file where the data should be stored. If not provided, temporary file is created in system folder for temporary files
#'
#' @return HTTP response object returned by `httr2::req_perform()`. The data are stored in the provided file and can be analyzed with `process_results`. The provided `batch_id` and `component_id` are added to the response data.
#' @export
#'
#' @examples
#' \dontrun{
#' cc <- define_connection(
#'   "https://www.myjatosinstance.org",
#'   "Bearer jap_xXxXxXxXxXxXxXxXxXxXxXxXxXxXxXxXxXxXx"
#' )
#' results <- get_results(cc, batch_id = 100)
#' results$batch_id
#' # Name of the temporary file with the zipped data
#' results$body
#' }
get_results <- function(
  jc,
  batch_id = NULL,
  component_id = NULL,
  filename = NULL
) {
  req_base <- base_request(jc, "/jatos/api/v1/results/data")
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
  if (!is.null(filename)) {
    folder <- dirname(filename)
    if (!dir.exists(folder)) dir.create(folder, recursive = TRUE)
  } else {
    filename <- tempfile()
  }
  resp_results <- req_results |> httr2::req_perform(path = filename)
  resp_results$batch_id <- batch_id
  resp_results$component_id <- component_id
  resp_results
}
