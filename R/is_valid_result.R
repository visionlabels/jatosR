is_valid_result <- function(rr) {
  expected_cols <- c("api_version", "study_id", "study_uuid", "study_title")
  # it is a tibble
  tibble::is_tibble(rr) &&
    # all expected columns are present
    all(expected_cols %in% names(rr)) &&
    # their types are correct
    is.integer(rr$study_id) &&
    is.character(rr$study_uuid) &&
    is.character(rr$study_title)
}
