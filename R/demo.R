utils::globalVariables("demo_srt")

#' Simple Response Time demo
#'
#' Loads the bundled `demo_srt` dataset, a canned copy of the `meta` and
#' `results` responses `get_metadata`/`get_results` would return for the
#' [Simple Reaction Time Task](https://www.jatos.org/Example-Studies) example
#' study. No network connection is required.
#'
#' @param nofile Default `FALSE`. Use `TRUE` if you only need `meta` (e.g. to
#'   inspect fields with `study_result_info`/`component_result_info`) and
#'   don't intend to call `process_results()`, which requires the zip file
#'   copied to the working directory.
#'
#' @return List with `meta` and `results`, which can be passed to
#'   `process_results`. Unless `nofile = TRUE`, this also copies the file
#'   `srtdemo.zip` into the working directory, since `results$body` refers to
#'   it by relative path; remove it afterwards with `clean_demo()`.
#' @export
#'
#' @examples
#' demo <- prepare_demo()
#' r <- process_results(demo$meta, demo$results)
#' r
#' clean_demo()
prepare_demo <- function(nofile = FALSE) {
  if (!nofile) {
    file.copy(
      system.file("extdata", "srtdemo.zip", package = "jatosR"),
      getwd()
    )
  }
  demo_srt
}

#' Demo clean up
#'
#' Removes the `srtdemo.zip` file copied into the working directory by
#' `prepare_demo()`. Does nothing if the file is not present.
#'
#' @return Invisibly `NULL`; called for its side effect.
#' @export
#'
#' @examples
#' demo <- prepare_demo()
#' r <- process_results(demo$meta, demo$results)
#' r
#' clean_demo()
clean_demo <- function() {
  fn <- file.path(getwd(), "srtdemo.zip")
  if (file.exists(fn)) unlink(fn)
}
