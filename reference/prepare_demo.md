# Simple Response Time demo

Loads the bundled `demo_srt` dataset, a canned copy of the `meta` and
`results` responses `get_metadata`/`get_results` would return for the
[Simple Reaction Time Task](https://www.jatos.org/Example-Studies)
example study. No network connection is required.

## Usage

``` r
prepare_demo(nofile = FALSE)
```

## Arguments

- nofile:

  Default `FALSE`. Use `TRUE` if you only need `meta` (e.g. to inspect
  fields with `study_result_info`/`component_result_info`) and don't
  intend to call
  [`process_results()`](https://visionlabels.github.io/jatosR/reference/process_results.md),
  which requires the zip file copied to the working directory.

## Value

List with `meta` and `results`, which can be passed to
`process_results`. Unless `nofile = TRUE`, this also copies the file
`srtdemo.zip` into the working directory, since `results$body` refers to
it by relative path; remove it afterwards with
[`clean_demo()`](https://visionlabels.github.io/jatosR/reference/clean_demo.md).

## Examples

``` r
demo <- prepare_demo()
r <- process_results(demo$meta, demo$results)
r
#> $meta
#> $meta$api_version
#> [1] "1.0.1"
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
#> # A tibble: 4 × 26
#>    id_sr uuid     study_code start_date_sr       end_date_sr         duration_sr
#>    <int> <chr>    <chr>      <dttm>              <dttm>              <drtn>     
#> 1 442488 41b9678… aiJynkFlN… 2024-03-22 16:35:36 2024-03-22 16:36:02 26 secs    
#> 2 442489 3d16e38… aiJynkFlN… 2024-03-22 16:36:10 2024-03-22 16:36:39 29 secs    
#> 3 442490 e317ffc… aiJynkFlN… 2024-03-22 16:36:56 2024-03-22 16:37:19 23 secs    
#> 4 442491 e97b34d… aiJynkFlN… 2024-03-22 16:37:26 2024-03-22 16:37:56 30 secs    
#> # ℹ 20 more variables: last_seen_date <dttm>, study_state <chr>,
#> #   worker_id <int>, worker_type <chr>, batch_id <int>, batch_uuid <chr>,
#> #   batch_title <chr>, group_id <lgl>, component_results_node_count <int>,
#> #   id_cr <int>, component_id <int>, component_uuid <chr>,
#> #   start_date_cr <dttm>, end_date_cr <dttm>, duration_cr <drtn>,
#> #   component_state <chr>, data_path <chr>, data_filename <chr>,
#> #   data_size <int>, data_raw <chr>
#> 
clean_demo()
```
