#' Simple Response Time demo
#'
#' @param nofile Default `FALSE`. Use `TRUE` if you want to load only the metadata, without creating the local file.
#'
#' @return list with `meta` and `results`, which could be used in `process_results`. It also copies file `srt_demo.zip` in the working directory. After `process_results`, you can delete it, or use `clen_demo()`.
#' @export
#'
#' @examples
#' demo <- prepare_demo()
#' r <- process_results(demo$meta, demo$results)
#' r
#' clean_demo()
prepare_demo <- function(nofile = F) {
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
#' Cleans temporary file after `prepare_demo()`.
#'
#' @return Nothing
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
