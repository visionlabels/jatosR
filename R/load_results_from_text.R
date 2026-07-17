if (FALSE) {
  fn <- file.path("inst", "extdata", "srtdemo_jatos_results.zip")
  tmpfolder <- tempfile()
  utils::unzip(fn, exdir = tmpfolder)
  r <- process_metadata_from_text(file.path(tmpfolder, "metadata.json"))
  r
  txt <- file.path(tmpfolder, "metadata.json")
  rx <- load_results_from_text(file.path(tmpfolder, "metadata.json"))
  rx
  unlink(tmpfolder)
}

load_results_from_text <- function(txt) {
  mj <- jsonlite::fromJSON(txt, simplifyVector = F)

  rr <- tibble::tibble(
    api_version = mj$apiVersion %||% NA_character_,
    study_id = mj$data[[1]]$studyId,
    study_uuid = mj$data[[1]]$studyUuid,
    study_title = mj$data[[1]]$studyTitle,
    study_results = ifelse(
      length(mj$data[[1]]$studyResults) == 0,
      list(NULL),
      purrr::map(mj$data[[1]]$studyResults, load_study_results) |>
        dplyr::bind_rows() |>
        list()
    )
  )
  rr
}
# load_results_from_text(txt)
