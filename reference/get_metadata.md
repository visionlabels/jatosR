# Get metadata for JATOS results

Get metadata for JATOS results

## Usage

``` r
get_metadata(jc, batch_id = NULL, component_id = NULL)
```

## Arguments

- jc:

  JATOS connection info, e.g. from `define_connection`

- batch_id:

  Integer identifier for a particular experiment batch. Mutually
  exclusive with `component_id`.

- component_id:

  Integer identifier for a particular experiment component. Mutually
  exclusive with `batch_id`.

## Value

HTTP response object returned by
[`httr2::req_perform()`](https://httr2.r-lib.org/reference/req_perform.html).
The actual metadata are stored in the response `body` and can be
analyzed with `process_results`. The provided `batch_id` and
`component_id` are added to the response object.

## Details

Exactly one of `batch_id` or `component_id` must be provided; the
function errors if both or neither are given.

## Examples

``` r
if (FALSE) { # \dontrun{
cc <- define_connection(
  "https://www.myjatosinstance.org",
  "Bearer jap_xXxXxXxXxXxXxXxXxXxXxXxXxXxXxXxXxXxXx"
)
meta <- get_metadata(cc, batch_id = 100)
meta$batch_id
} # }
```
