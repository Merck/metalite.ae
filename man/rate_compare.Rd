% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rate_compare.R
\name{rate_compare}
\alias{rate_compare}
\title{Unstratified and stratified Miettinen and Nurminen test}
\usage{
rate_compare(
  formula,
  strata,
  data,
  delta = 0,
  weight = c("ss", "equal", "cmh"),
  test = c("one.sided", "two.sided"),
  bisection = 100,
  eps = 1e-06,
  alpha = 0.05
)
}
\arguments{
\item{formula}{A symbolic description of the model to be fitted,
which has the form \code{y ~ x}. Here, \code{y} is the numeric vector
with values of 0 or 1. \code{x} is the group information.}

\item{strata}{An optional vector of weights to be used in the analysis.
If not specified, unstratified MN analysis is used.
If specified, stratified MN analysis is conducted.}

\item{data}{An optional data frame, list, or environment containing
the variables in the model.
If not found in data, the variables are taken from \code{environment (formula)},
typically the environment from which \code{rate_compare} is called.}

\item{delta}{A numeric value to set the difference of two group
under the null.}

\item{weight}{Weighting schema used in stratified MN method.
Default is \code{"ss"}:
\itemize{
\item \code{"equal"} for equal weighting.
\item \code{"ss"} for sample size weighting.
\item \code{"cmh"} for Cochran–Mantel–Haenszel's weights.
}}

\item{test}{A character string specifying the side of p-value,
must be one of \code{"one.sided"}, or \code{"two.sided"}.}

\item{bisection}{The number of sections in the interval used in
bisection method. Default is 100.}

\item{eps}{The level of precision. Default is 1e-06.}

\item{alpha}{Pre-defined alpha level for two-sided confidence interval.}
}
\value{
A data frame with the test results.
}
\description{
Unstratified and stratified Miettinen and Nurminen test details can be found
in \code{vignette("rate-compare")}.
}
\examples{
# Conduct the stratified MN analysis with sample size weights
treatment <- c(rep("pbo", 100), rep("exp", 100))
response <- c(rep(0, 80), rep(1, 20), rep(0, 40), rep(1, 60))
stratum <- c(rep(1:4, 12), 1, 3, 3, 1, rep(1:4, 12), rep(1:4, 25))
rate_compare(
  response ~ factor(treatment, levels = c("pbo", "exp")),
  strata = stratum,
  delta = 0,
  weight = "ss",
  test = "one.sided",
  alpha = 0.05
)
}
\references{
Miettinen, O. and Nurminen, M, Comparative Analysis of Two Rates.
\emph{Statistics in Medicine}, 4(2):213--226, 1985.
}
