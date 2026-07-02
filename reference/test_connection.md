# Test JATOS connection

Checks that the personal access token in `jc` is valid by calling the
JATOS admin token endpoint. Use
[`httr2::resp_status()`](https://httr2.r-lib.org/reference/resp_status.html)
on the result to verify a `200` response, or wrap the call in
[`tryCatch()`](https://rdrr.io/r/base/conditions.html) since
[`httr2::req_perform()`](https://httr2.r-lib.org/reference/req_perform.html)
raises an error for HTTP error statuses.

## Usage

``` r
test_connection(jc)
```

## Arguments

- jc:

  JATOS connection info, e.g. from `define_connection`

## Value

HTTP response object returned by
[`httr2::req_perform()`](https://httr2.r-lib.org/reference/req_perform.html)

## Examples

``` r
if (FALSE) { # \dontrun{
cc <- define_connection(
  "https://www.myjatosinstance.org",
  "Bearer jap_xXxXxXxXxXxXxXxXxXxXxXxXxXxXxXxXxXxXx"
)
test_connection(cc)
} # }
```
