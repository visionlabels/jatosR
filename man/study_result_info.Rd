% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{study_result_info}
\alias{study_result_info}
\title{Parser of studyResults data}
\usage{
study_result_info(sr)
}
\arguments{
\item{sr}{list corresponding to the studyResults part of the data}
}
\value{
List of size-one vectors, which could be converted to tibble if needed.
}
\description{
Parser of studyResults data
}
\examples{
demo <- prepare_demo(nofile = TRUE)
json <- demo$meta |> httr2::resp_body_json()
sr <- study_result_info(json$data[[1]]$studyResults[[1]])
sr
}
