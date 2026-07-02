# Turns JATOS metadata JSON into a single table

Parses JATOS metadata (the `studyResults`/`componentResults` tree)
shared by `process_results` and `process_results_from_file`, regardless
of whether it came from a live API response or a local export. Only the
metadata is processed here; neither function adds the actual response
data (column `data_raw`) until after this returns.

## Usage

``` r
process_metadata_from_text(txt)
```

## Arguments

- txt:

  JSON metadata as text, a URL, or a path to a JSON file — passed
  through to
  [`jsonlite::fromJSON()`](https://jeroen.r-universe.dev/jsonlite/reference/fromJSON.html),
  which accepts any of those forms

## Value

list with

- `meta`:

  General info like study ID or API version

- `data`:

  Tibble with all meta data, one row per study/component result

## Examples

``` r
fn <- system.file("extdata", "srtdemo_jatos_results.zip", package = "jatosR")
tmpfolder <- tempfile()
utils::unzip(fn, exdir = tmpfolder)
r <- process_metadata_from_text(file.path(tmpfolder, "metadata.json"))
r
#> $meta
#> $meta$api_version
#> [1] NA
#> 
#> $meta$study_id
#> [1] 13827
#> 
#> $meta$study_uuid
#> [1] "dd8432a4-7827-4d4f-8785-56aba5a5f61c"
#> 
#> $meta$study_title
#> [1] "jsPsych 7 Simple Reaction Time Task (clone)"
#> 
#> $meta$study_results_node_count
#> [1] 4
#> 
#> 
#> $data
#> # A tibble: 4 × 25
#>    id_sr uuid     study_code start_date_sr       end_date_sr         duration_sr
#>    <int> <chr>    <chr>      <dttm>              <dttm>              <drtn>     
#> 1 442488 41b9678… aiJynkFlN… 2024-03-22 16:35:36 2024-03-22 16:36:02 26 secs    
#> 2 442489 3d16e38… aiJynkFlN… 2024-03-22 16:36:10 2024-03-22 16:36:39 29 secs    
#> 3 442490 e317ffc… aiJynkFlN… 2024-03-22 16:36:56 2024-03-22 16:37:19 23 secs    
#> 4 442491 e97b34d… aiJynkFlN… 2024-03-22 16:37:26 2024-03-22 16:37:56 30 secs    
#> # ℹ 19 more variables: last_seen_date <dttm>, study_state <chr>,
#> #   worker_id <int>, worker_type <chr>, batch_id <int>, batch_uuid <chr>,
#> #   batch_title <chr>, group_id <lgl>, component_results_node_count <int>,
#> #   id_cr <int>, component_id <int>, component_uuid <chr>,
#> #   start_date_cr <dttm>, end_date_cr <dttm>, duration_cr <drtn>,
#> #   component_state <chr>, data_path <chr>, data_filename <chr>,
#> #   data_size <int>
#> 
```
