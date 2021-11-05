#' Natural experiment on the effect of a binary treatment on viewing metrics
#'
#' @format A \code{\link[data.table]{data.table}} with simulated data from a
#'  quasi-experiment (observational study) with five baseline covariates, a
#'  single timepoint binary treatment, and a continuous-valued outcome (metric
#'  for viewing time). There are 5000 unique units in the dataset, each in a
#'  single row, and 8 columns, described below.
#' \describe{
#'   \item{id}{Numeric ID of the observed unit. No repeated measures.}
#'   \item{num_devices}{The number of viewing devices recorded for the given
#'     user. A possible segmentation covariate.}
#'   \item{is_p2plus}{TODO: FILL IN. A possible segmentation covariate.}
#'   \item{is_newmarket}{A binary numeric indicator of whether the user falls
#'     in a region corresponding to a new market.}
#'   \item{baselin_ltv}{TODO:}
#'   \item{baseline_viewing}{TODO:}
#'   \item{treatment}{A binary numeric indicator of whether the unit received
#'     (non-randomly) the intervention of interest.}
#'   \item{outcome_viewing}{A continuous-valued measurement of viewing hours,
#'     the outcome of interest. Note that this mimics a metric derived from the
#'     viewing time, not the time itself.}
#' }
"data_example"

#' Natural experiment on the effect of a binary treatment on viewing metrics,
#' where treating units has a cost
#'
#' @format A \code{\link[data.table]{data.table}} with simulated data from a
#'  quasi-experiment (observational study) with five baseline covariates, a
#'  single timepoint binary treatment, and a continuous-valued outcome (metric
#'  for viewing time). There are 5000 unique units in the dataset, each in a
#'  single row, and 8 columns, described below.
#' \describe{
#'   \item{id}{Numeric ID of the observed unit. No repeated measures.}
#'   \item{num_devices}{The number of viewing devices recorded for the given
#'     user. A possible segmentation covariate.}
#'   \item{is_p2plus}{TODO: FILL IN. A possible segmentation covariate.}
#'   \item{is_newmarket}{A binary numeric indicator of whether the user falls
#'     in a region corresponding to a new market.}
#'   \item{baselin_ltv}{TODO:}
#'   \item{baseline_viewing}{TODO:}
#'   \item{treatment}{A binary numeric indicator of whether the unit received
#'     (non-randomly) the intervention of interest.}
#'   \item{outcome_viewing}{A continuous-valued measurement of viewing hours,
#'     the outcome of interest. Note that this mimics a metric derived from the
#'     viewing time, not the time itself.}
#'   \item{cost}{The cost associated with delivering treatment to the unit.}
#' }
"data_example_with_cost"
