#' Parser of studyResults data
#'
#' @param sr list corresponding to the studyResults part of the data
#'
#' @return List of size-one vectors, which could be converted to tibble if needed.
#' @export
#'
#' @examples
#' demo <- prepare_demo(nofile = TRUE)
#' json <- demo$meta |> httr2::resp_body_json()
#' sr <- study_result_info(json$data[[1]]$studyResults[[1]])
#' sr
study_result_info <- function(sr) {
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
      batch_uuid = sr$batchUuid %||% NA_integer_,
      batch_title = sr$batchTitle %||% NA_character_,
      group_id = sr$groupId %||% NA_integer_,
      component_results_node_count = length(sr$componentResults)
    )
  r$duration_sr <- r$end_date_sr - r$start_date_sr
  r
}
