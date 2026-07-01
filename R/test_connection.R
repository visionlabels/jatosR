#' Test JATOS connection
#'
#' Checks that the personal access token in `jc` is valid by calling the
#' JATOS admin token endpoint. Use `httr2::resp_status()` on the result to
#' verify a `200` response, or wrap the call in `tryCatch()` since
#' `httr2::req_perform()` raises an error for HTTP error statuses.
#'
#' @param jc JATOS connection info, e.g. from `define_connection`
#'
#' @return HTTP response object returned by `httr2::req_perform()`
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
test_connection <- function(jc) {
  req_base <- base_request(jc, "/jatos/api/v1/admin/token")
  resp <- req_base |> httr2::req_perform()
  resp
}
