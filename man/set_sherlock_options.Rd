% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/options.R
\name{set_sherlock_options}
\alias{set_sherlock_options}
\title{Set Options for Causal Segmentation Analysis}
\usage{
set_sherlock_options(option, val)
}
\arguments{
\item{option}{A global option of \pkg{sherlock}'s routines for causal
segmentation analysis.}

\item{val}{The value to be assigned to the global option.}
}
\value{
None, called for the side effect of setting global options.
}
\description{
Setter for global options used in causal segmentation analysis
and supporting utilities. Currently supported are global options for the
desired coverage rate of confidence intervals for the CATE and segmentation
effect measures (TSMs, ATEs, HTEs, OTEs) and the critical p-value to be
used when performing hypothesis tests of the CATE for the assignment of a
treatment rule to discovered segments.
}
\keyword{internal}
