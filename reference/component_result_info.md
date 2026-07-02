# Parser of componentResults data

Extracts a fixed set of fields from a single `componentResults` entry
(as parsed from JATOS JSON metadata) into a flat list. Missing fields
fall back to a typed `NA`, so the result is always a list of size-one
vectors that can be safely row-bound into a tibble, e.g. with
[`dplyr::bind_rows()`](https://dplyr.tidyverse.org/reference/bind_rows.html)
as done in `process_results`.

## Usage

``` r
component_result_info(cr)
```

## Arguments

- cr:

  list corresponding to a single `componentResults` entry, e.g.
  `json$data[[1]]$studyResults[[1]]$componentResults[[1]]`

## Value

Named list of size-one vectors: `id_cr`, `component_id`,
`component_uuid`, `start_date_cr`, `end_date_cr`, `duration_cr`
(computed as `end_date_cr - start_date_cr`), `component_state`,
`data_path`, `data_filename`, and `data_size`.

## Examples

``` r
demo <- prepare_demo(nofile = TRUE)
json <- demo$meta |> httr2::resp_body_json()
cr <- component_result_info(
  json$data[[1]]$studyResults[[1]]$componentResults[[1]]
)
cr
#> $id_cr
#> [1] 605082
#> 
#> $component_id
#> [1] 21204
#> 
#> $component_uuid
#> [1] "e4bf0e8e-c09a-424d-8ed4-236e0295d6d7"
#> 
#> $start_date_cr
#> [1] "2024-03-22 16:35:36 UTC"
#> 
#> $end_date_cr
#> [1] "2024-03-22 16:36:02 UTC"
#> 
#> $duration_cr
#> Time difference of 26 secs
#> 
#> $component_state
#> [1] "FINISHED"
#> 
#> $data_path
#> [1] "/study_result_442488/comp-result_605082"
#> 
#> $data_filename
#> [1] NA
#> 
#> $data_size
#> [1] 5775
#> 
```
