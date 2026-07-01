#' Parser of componentResults data
#'
#' @param cr list corresponding to the componentResults part of the data
#'
#' @return List of size-one vectors, which could be converted to tibble if needed.
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
