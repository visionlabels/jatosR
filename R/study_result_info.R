#' Parser of studyResults data
#'
#' Extracts a fixed set of fields from a single `studyResults` entry (as
#' parsed from JATOS JSON metadata) into a flat list. Missing fields fall
#' back to a typed `NA`, so the result is always a list of size-one vectors
#' that can be safely row-bound into a tibble, e.g. with `dplyr::bind_rows()`
#' as done in `process_results`.
#'
#' @param sr list corresponding to a single `studyResults` entry, e.g.
#'   `json$data[[1]]$studyResults[[1]]`
#'
#' @return Named list of size-one vectors: `id_sr`, `uuid`, `study_code`,
#'   `start_date_sr`, `end_date_sr`, `duration_sr` (computed as
#'   `end_date_sr - start_date_sr`), `last_seen_date`, `study_state`,
#'   `worker_id`, `worker_type`, `batch_id`, `batch_uuid`, `batch_title`,
#'   `group_id`, and `component_results_node_count`.
#' @export
#'
#' @examples
#' demo <- prepare_demo(nofile = TRUE)
#' json <- demo$meta |> httr2::resp_body_json()
#' sr <- study_result_info(json$data[[1]]$studyResults[[1]])
#' sr
study_result_info <- function(sr) {
  # we expect only a single study result
  stopifnot(length(sr$id) == 1)
  r <-
    list(
      id_sr = sr$id %||% NA_integer_,
      uuid = sr$uuid %||% NA_character_,
      study_code = sr$studyCode %||% NA_character_,
      start_date_sr = convert_date(sr$startDate %||% NA_integer_),
      end_date_sr = convert_date(sr$endDate %||% NA_integer_),
      duration_sr = sr$duration %||% NA_character_,
      last_seen_date = convert_date(sr$lastSeenDate %||% NA_integer_),
      study_state = sr$studyState %||% NA_character_,
      # url_query_parameters = sr$urlQueryParameters,
      worker_id = sr$workerId %||% NA_integer_,
      worker_type = sr$workerType %||% NA_character_,
      batch_id = sr$batchId %||% NA_integer_,
      batch_uuid = sr$batchUuid %||% NA_character_,
      batch_title = sr$batchTitle %||% NA_character_,
      group_id = sr$groupId %||% NA_integer_,
      component_results_node_count = length(sr$componentResults)
    )
  r$duration_sr <- r$end_date_sr - r$start_date_sr
  r
}
