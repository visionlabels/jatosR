fn <- system.file("extdata", "srtdemo_jatos_results.zip", package = "jatosR")
tmpfolder <- tempfile()
utils::unzip(fn, exdir = tmpfolder)
r <- process_metadata_from_text(file.path(tmpfolder, "metadata.json"))
r
rx <- load_results_from_text(file.path(tmpfolder, "metadata.json"))
rx
unlink(tmpfolder)

load_results_from_text <- function(txt) {
  mj <- jsonlite::fromJSON(txt)

  rr <- tibble::tibble(
    api_version = mj$apiVersion %||% NA_character_,
    study_id = mj$data$studyId,
    study_uuid = mj$data$studyUuid,
    study_title = mj$data$studyTitle
  )
  rr
}
