% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/format_ae_specific.R
\name{format_ae_specific}
\alias{format_ae_specific}
\title{Format AE specific analysis}
\usage{
format_ae_specific(
  outdata,
  display = c("n", "prop", "total"),
  hide_soc_stats = FALSE,
  digits_prop = 1,
  digits_ci = 1,
  digits_p = 3,
  digits_dur = c(1, 1),
  digits_events = c(1, 1),
  filter_method = c("percent", "count"),
  filter_criteria = 0,
  sort_order = c("alphabetical", "count_des", "count_asc"),
  sort_column = NULL,
  mock = FALSE
)
}
\arguments{
\item{outdata}{An \code{outdata} object created by \code{\link[=prepare_ae_specific]{prepare_ae_specific()}}.}

\item{display}{A character vector of measurement to be displayed:
\itemize{
\item \code{n}: Number of subjects with adverse event.
\item \code{prop}: Proportion of subjects with adverse event.
\item \code{total}: Total columns.
\item \code{diff}: Risk difference.
\item \code{diff_ci}: 95\% confidence interval of risk difference using M&N method.
\item \code{diff_p}: p-value of risk difference using M&N method.
\item \code{dur}: Average of adverse event duration.
\item \code{events_avg}: Average number of adverse event per subject.
\item \code{events_count}: Count number of adverse event per subject.
}}

\item{hide_soc_stats}{A boolean value to hide stats for SOC rows.}

\item{digits_prop}{A numeric value of number of digits for proportion value.}

\item{digits_ci}{A numeric value of number of digits for confidence interval.}

\item{digits_p}{A numeric value of number of digits for p-value.}

\item{digits_dur}{A numeric value of number of digits for average
duration of adverse event.}

\item{digits_events}{A numeric value of number of digits for average of
number of adverse events per subject.}

\item{filter_method}{A character value to specify how to filter rows:
\itemize{
\item \code{count}: Filtered based on participant count.
\item \code{percent}: Filtered based percent incidence.
}}

\item{filter_criteria}{A numeric value to display rows where at least
one therapy group has a percent incidence or participant count
greater than or equal to the specified value.
If \code{filter_method} is \code{percent}, the value should be between 0 and 100.
If \code{filter_method} is \code{count}, the value should be greater than 0.}

\item{sort_order}{A character value to specify sorting order:
\itemize{
\item \code{alphabetical}: Sort by alphabetical order.
\item \code{count_des}: Sort by count in descending order.
\item \code{count_asc}: Sort by count in ascending order.
}}

\item{sort_column}{A character value of \code{group} in \code{outdata} used to sort a table with.}

\item{mock}{A boolean value to display mock table.}
}
\value{
A list of analysis raw datasets.
}
\description{
Format AE specific analysis
}
\examples{
meta <- meta_ae_example()

outdata <- prepare_ae_specific(meta,
  population = "apat",
  observation = "wk12",
  parameter = "rel"
)

# Basic example
tbl <- outdata |>
  format_ae_specific()
head(tbl$tbl)

# Filtering
tbl <- outdata |>
  format_ae_specific(
    filter_method = "percent",
    filter_criteria = 10
  )
head(tbl$tbl)

# Display different measurements
tbl <- outdata |>
  extend_ae_specific_events() |>
  format_ae_specific(display = c("n", "prop", "events_count"))
head(tbl$tbl)
}
