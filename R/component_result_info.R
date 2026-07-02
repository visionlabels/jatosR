#' Parser of componentResults data
#'
#' Extracts a fixed set of fields from a single `componentResults` entry (as
#' parsed from JATOS JSON metadata) into a flat list. Missing fields fall
#' back to a typed `NA`, so the result is always a list of size-one vectors
#' that can be safely row-bound into a tibble, e.g. with `dplyr::bind_rows()`
#' as done in `process_results`.
#'
#' @param cr list corresponding to a single `componentResults` entry, e.g.
#'   `json$data[[1]]$studyResults[[1]]$componentResults[[1]]`
#'
#' @return Named list of size-one vectors: `id_cr`, `component_id`,
#'   `component_uuid`, `start_date_cr`, `end_date_cr`, `duration_cr`
#'   (computed as `end_date_cr - start_date_cr`), `component_state`,
#'   `data_path`, `data_filename`, and `data_size`.
#' @export
#'
#' @examples
#' demo <- prepare_demo(nofile = TRUE)
#' json <- demo$meta |> httr2::resp_body_json()
#' cr <- component_result_info(
#'   json$data[[1]]$studyResults[[1]]$componentResults[[1]]
#' )
#' cr
component_result_info <- function(cr) {
  stopifnot(length(cr$id) == 1)
  r <-
    list(
      id_cr = cr$id %||% NA_integer_,
      component_id = cr$componentId %||% NA_integer_,
      component_uuid = cr$componentUuid %||% NA_character_,
      start_date_cr = convert_date(cr$startDate %||% NA_integer_),
      end_date_cr = convert_date(cr$endDate %||% NA_integer_),
      duration_cr = cr$duration %||% NA_character_,
      component_state = cr$componentState %||% NA_character_,
      data_path = cr$path %||% NA_character_,
      data_filename = cr$data$filename %||% NA_character_,
      data_size = cr$data$size %||% NA_integer_
    )
  r$duration_cr <- r$end_date_cr - r$start_date_cr
  r
}
