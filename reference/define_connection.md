# Define JATOS connection

Define JATOS connection

## Usage

``` r
define_connection(url, token = Sys.getenv("JATOS_PAT"))
```

## Arguments

- url:

  Base URL to the JATOS instance (with no API endpoints)

- token:

  API token obtained from the JATOS instance. Defaults to the
  `JATOS_PAT` environment variable (e.g. set in `.Renviron`).

## Value

A simple list with `url` and `token` values, to be passed as `jc` to
other functions such as `test_connection`, `get_metadata`, and
`get_results`.

## Details

Check https://www.jatos.org/JATOS-API.html#personal-access-tokens about
the process of getting a token.

You don't need to worry about trailing slash in URL (it is automatically
removed). If you omit "Bearer " part of the token, but the token seems
to be valid, because it starts with "jap", the "Bearer " part is added.

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
