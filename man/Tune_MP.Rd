% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{Tune_MP}
\alias{Tune_MP}
\title{Scope and Tune an MP to the Tuning Targets}
\usage{
Tune_MP(
  MP_name,
  Tuning_OMs,
  TuneTargets,
  Hist_dir = "Hist_Objects/Reference",
  Tune_dir = "Tuning_Objects",
  test_vals = NULL,
  parallel = TRUE,
  skip_scope = NULL
)
}
\arguments{
\item{parallel}{}
}
\description{
Wrapper for `Tune` and `Scope`
}
