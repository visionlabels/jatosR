
# jatosR

<!-- badges: start -->
<!-- badges: end -->

The goal of jatosR is to help with downloading data from [JATOS server](https://www.jatos.org/) 
(for example from [MindProbe](https://mindprobe.eu/)). 
The package provides a thin wrapper around [JATOS api](https://www.jatos.org/JATOS-API.html).

The package simplifies the process by eliminating the need to download data manually. 
This improves reproducibility, simplifies team collaboration and enables building of dashboards showing the progress of data collection.

If your JATOS experiment is based on [jsPsych](https://www.jspsych.org/) and you want to analyze the data within R,
check also the sister package [jspsychread](https://github.com/visionlabels/jspsychread).

## Installation

The package is in active development. You can install the current version from [GitHub](https://github.com/visionlabels/jatosR) with following code:

``` r
# Install devtools package if necessary
if(!"devtools" %in% rownames(installed.packages())) install.packages("devtools")

# Install the stable verion from GitHub
devtools::install_github("visionlabels/jatosR")

```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(jatosR)

url <- "https://www.myjatosinstance.org"
# don't publish your token!
token <- "Bearer jap_xXxXxXxXxXxXxXxXxXxXxXxXxXxXxXxXxXxXx"

cc <- define_connection(url, token)
test_connection(cc)

meta <- get_metadata(cc, batch_id = 100)
results <- get_results(cc, batch_id = 100)
r <- process_results(meta, results)
r$data
```

If you want to check the process without API token and living JATOS instance:

``` r
library(jatosR)

# returns demo data and saves a copy of zipped data into working directory
demo <- prepare_demo()
r <- process_results(demo$meta, demo$results)
r$data
clean_demo() # cleans the local zipped data

```

You can also check the final state by:

``` r
library(dplyr)
library(jatosR)

data("demo_srt_processed")

```
## Handling tokens

For the sake of simplicity, the example above used the API token directly. 
However, you do not want to have your token in your code. 
You can store it into your `.Renviron` file and then any `jatosR` code,
which is being executed from the given computer, gets authorized.

1) [Get your JATOS API token](https://www.jatos.org/JATOS-API.html#how-to-generate-a-token). It might be a good idea to have a dedicated user account for API access.
2) Run `usethis::edit_r_environ()` or open your `.Renviron` file directly
3) Type `JATOS_PAT=""` and paste your API token. The line should resemble this:

```bash
JATOS_PAT="jap_xXxXxXxXxXxXxXxXxXxXxXxXxXxXxXxXxXxXx"
```

4) Restart R and check if the token gets loaded with

```r
Sys.getenv("JATOS_PAT")
```

