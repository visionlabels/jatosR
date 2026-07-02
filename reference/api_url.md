# URL setup helper

URL setup helper

## Usage

``` r
api_url(jc, endpoint)
```

## Arguments

- jc:

  JATOS connection info, e.g. from `define_connection`

- endpoint:

  API endpoint address

## Value

URL address created by matching the connection url and endpoint string.

## Examples

``` r
if (FALSE) { # \dontrun{
cc <- define_connection(
  "https://www.myjatosinstance.org",
  "Bearer jap_xXxXxXxXxXxXxXxXxXxXxXxXxXxXxXxXxXxXx"
)
# API for token info
api_url(cc, "/jatos/api/v1/admin/token")
} # }
```
