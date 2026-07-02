# Turns metadata and data into a single table

Turns metadata and data into a single table

## Usage

``` r
process_results(meta, results)
```

## Arguments

- meta:

  Metadata from `get_metadata` function

- results:

  Results from `get_results` function, or `NULL` to process metadata
  only, leaving `data_raw` as `NA`.

## Value

list with

- `meta`:

  General info like study ID or API version

- `data`:

  Tibble with all meta data, column `data_raw` stores the results for
  each entry in text format. This can be parsed for example with
  `jsonlite` or `jspsychread` packages

## Details

Combines the metadata returned by `get_metadata` with the raw data
downloaded by `get_results`: the zip file referenced in `results$body`
is unzipped into a temporary folder, `data.txt` for each study/component
result is read into memory, and the temporary folder is removed again.

If `results` is `NULL` or its `body` isn't a downloaded file (i.e.
`get_results` wasn't called, or was called without a `filename`/default
temporary file), a warning is issued and `data_raw` is left as `NA` for
every row instead of erroring.

## Examples

``` r
if (FALSE) { # \dontrun{
cc <- define_connection(
  "https://www.myjatosinstance.org",
  "Bearer jap_xXxXxXxXxXxXxXxXxXxXxXxXxXxXxXxXxXxXx"
)
meta <- get_metadata(cc, batch_id = 100)
results <- get_results(cc, batch_id = 100)
r <- process_results(meta, results)
} # }
```
