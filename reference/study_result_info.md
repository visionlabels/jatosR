# Parser of studyResults data

Extracts a fixed set of fields from a single `studyResults` entry (as
parsed from JATOS JSON metadata) into a flat list. Missing fields fall
back to a typed `NA`, so the result is always a list of size-one vectors
that can be safely row-bound into a tibble, e.g. with
[`dplyr::bind_rows()`](https://dplyr.tidyverse.org/reference/bind_rows.html)
as done in `process_results`.

## Usage

``` r
study_result_info(sr)
```

## Arguments

- sr:

  list corresponding to a single `studyResults` entry, e.g.
  `json$data[[1]]$studyResults[[1]]`

## Value

Named list of size-one vectors: `id_sr`, `uuid`, `study_code`,
`start_date_sr`, `end_date_sr`, `duration_sr` (computed as
`end_date_sr - start_date_sr`), `last_seen_date`, `study_state`,
`worker_id`, `worker_type`, `batch_id`, `batch_uuid`, `batch_title`,
`group_id`, and `component_results_node_count`.

## Examples

``` r
demo <- prepare_demo(nofile = TRUE)
json <- demo$meta |> httr2::resp_body_json()
sr <- study_result_info(json$data[[1]]$studyResults[[1]])
sr
#> $id_sr
#> [1] 442488
#> 
#> $uuid
#> [1] "41b96789-9b73-4185-97ad-f56455ad3ef4"
#> 
#> $study_code
#> [1] "aiJynkFlNJj"
#> 
#> $start_date_sr
#> [1] "2024-03-22 16:35:36 UTC"
#> 
#> $end_date_sr
#> [1] "2024-03-22 16:36:02 UTC"
#> 
#> $duration_sr
#> Time difference of 26 secs
#> 
#> $last_seen_date
#> [1] "2024-03-22 16:35:37 UTC"
#> 
#> $study_state
#> [1] "FINISHED"
#> 
#> $worker_id
#> [1] 471179
#> 
#> $worker_type
#> [1] "GeneralMultiple"
#> 
#> $batch_id
#> [1] 15883
#> 
#> $batch_uuid
#> [1] "204a2801-2a1b-42e9-8ae5-dbfa8d4fde67"
#> 
#> $batch_title
#> [1] "Default"
#> 
#> $group_id
#> [1] NA
#> 
#> $component_results_node_count
#> [1] 1
#> 
```
