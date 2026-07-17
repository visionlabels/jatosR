if (FALSE) {
  demo <- prepare_demo(nofile = TRUE)
  txt <- demo$meta |> httr2::resp_body_string()
  json <- jsonlite::fromJSON(txt, simplifyVector = FALSE)
  sr_data <- json$data[[1]]$studyResults
}
load_study_results <- function(sr, parse_dates = TRUE) {
  r <-
    tibble::tibble(
      id = sr$id %||% NA_integer_,
      uuid = sr$uuid %||% NA_character_,
      study_code = sr$studyCode %||% NA_character_,
      start_date = convert_date_conditional(sr$startDate, parse_dates),
      end_date = convert_date_conditional(sr$endDate, parse_dates),
      duration = convert_duration_conditional(sr$duration, parse_dates),
      last_seen_date = convert_date_conditional(sr$lastSeenDate, parse_dates),
      study_state = sr$studyState %||% NA_character_,
      message = sr$message %||% NA_character_,
      worker_id = sr$workerId %||% NA_integer_,
      worker_type = sr$workerType %||% NA_character_,
      batch_id = sr$batchId %||% NA_integer_,
      batch_uuid = sr$batchUuid %||% NA_character_,
      batch_title = sr$batchTitle %||% NA_character_,
      group_id = sr$groupId %||% NA_integer_,
      is_quota_reached = sr$isQuotaReached %||% NA_integer_,
      component_results = ifelse(
        length(sr$componentResults) == 0,
        list(NULL),
        purrr::map(
          sr$componentResults,
          \(x) load_component_results(x, parse_dates = parse_dates)
        ) |>
          dplyr::bind_rows() |>
          list()
      )
    )
  r
}
# load_study_results(sr_data[[1]])
