# Basic JATOS HTTP request helper

Basic JATOS HTTP request helper

## Usage

``` r
base_request(jc, endpoint)
```

## Arguments

- jc:

  JATOS connection info, e.g. from `define_connection`

- endpoint:

  API endpoint address

## Value

[`httr2::request`](https://httr2.r-lib.org/reference/request.html)
object

## Examples

``` r
if (FALSE) { # \dontrun{
cc <- define_connection(
  "https://www.myjatosinstance.org",
  "Bearer jap_xXxXxXxXxXxXxXxXxXxXxXxXxXxXxXxXxXxXx"
)

req <- base_request(cc, "/jatos/api/v1/admin/token")
} # }
```
