# Get raw data for JATOS results

Get raw data for JATOS results

## Usage

``` r
get_results(jc, batch_id = NULL, component_id = NULL, filename = NULL)
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

- filename:

  Optional path to the file where the zipped data should be stored;
  parent directories are created if needed. If not provided, a temporary
  file is created in the system's temporary directory.

## Value

HTTP response object returned by
[`httr2::req_perform()`](https://httr2.r-lib.org/reference/req_perform.html).
The zipped data are written to `filename` (or the temporary file), whose
path is available in `body`, and can be analyzed with `process_results`.
The provided `batch_id` and `component_id` are added to the response
object.

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
results <- get_results(cc, batch_id = 100)
results$batch_id
# Name of the temporary file with the zipped data
results$body
} # }
```
