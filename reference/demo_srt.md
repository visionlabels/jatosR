# Simple Reaction Time Task raw response data

Simple reaction time study ([Simple Reaction Time
Task](https://github.com/JATOS/JATOS_examples/raw/main/examples/jspsych_7_simple_reaction_time_task.jzip)),
which is included in [JATOS
examples](https://www.jatos.org/Example-Studies). The example study was
uploaded to MindProbe server and run four times. See `process_results`
on how to turn the raw data into processed values.

## Usage

``` r
demo_srt
```

## Format

### `demo_srt`

A list with two items:

- meta:

  `httr2_response` as returned by `get_metadata`

- results:

  `httr2_response` as returned by `get_results`, with `body` referring
  to the bundled `srtdemo.zip` file, which you can copy into current
  working directory with `prepare_demo`

## Source

<https://github.com/visionlabels/jatosR>

## Details

This is the raw response data with two `httr2_response` values, as used
by
[`prepare_demo()`](https://visionlabels.github.io/jatosR/reference/prepare_demo.md).
