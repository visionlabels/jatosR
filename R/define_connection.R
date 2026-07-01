#' Define JATOS connection
#'
#' @details
#' Check https://www.jatos.org/JATOS-API.html#personal-access-tokens
#' about the process of getting a token.
#'
#' You don't need to worry about trailing slash in URL
#' (it is automatically removed). If you omit "Bearer " part
#' of the token, but the token seems to be valid, because it starts
#' with "jap", the "Bearer " part is added.
#'
#' @param url Base URL to the JATOS instance (with no API endpoints)
#' @param token API token obtained from the JATOS instance. Defaults to the
#'   `JATOS_PAT` environment variable (e.g. set in `.Renviron`).
#'
#' @return A simple list with `url` and `token` values, to be passed as `jc`
#'   to other functions such as `test_connection`, `get_metadata`, and
#'   `get_results`.
#' @export
#'
#' @examples
#' \dontrun{
#' cc <- define_connection(
#'   "https://www.myjatosinstance.org",
#'   "Bearer jap_xXxXxXxXxXxXxXxXxXxXxXxXxXxXxXxXxXxXx"
#' )
#' test_connection(cc)
#' }
define_connection <- function(url, token = Sys.getenv("JATOS_PAT")) {
  # remove trailing slash (all endpoints start with /)
  url <- stringr::str_remove(url, "/*$")
  # if only japXXX part provided, add "Bearer " at start
  if (stringr::str_starts(token, "jap")) {
    token <- stringr::str_c("Bearer ", token)
  }
  stopifnot(
    "Bad token format" = stringr::str_starts(token, "Bearer jap")
  )
  list(url = url, token = token)
}
