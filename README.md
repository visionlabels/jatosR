
# jatosR

<!-- badges: start -->
<!-- badges: end -->

The goal of jatosR is to help with downloading data from [JATOS server](https://www.jatos.org/) 
(for example from [MindProbe](https://mindprobe.eu/)). 
The package provides a thin wrapper around [JATOS api](https://www.jatos.org/JATOS-API.html).

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

If you want to check the process with API token and living JATOS instance:

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
data("demo_srt_processed")

```
