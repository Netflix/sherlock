#' Cost Functions for Assignment of a Dynamic Treatment Rule
#'
#' Cost functions for the assignment of a dynamic treatment rule to individual
#' segments identified by realizations of segmentation covariate strata. These
#' functions are used in conjunction with \code{\link{assign_rule}} for the
#' assignment of a treatment rule. There are three types of cost functions:
#' 1. \code{cost_threshold}, which enforces a fixed boundary for learning the
#'    dynamic treatment rule. This \code{cost_fun} requires a given cutoff
#'    against which to threshold the estimated CATE. Those segments with a CATE
#'    exceeding the threshold are assigned the treatment, while the treatment
#'    is withheld from those segments whose estimated CATE falls below.
#' 2. \code{cost_budget}, which data adaptively learns a cost boundary for
#'    assignment of a treatment rule. This \code{cost_fun} requires as input a
#'    maximum proportion of units to receive treatment (and, optionally, any
#'    observation-level weights), used to derive a cutoff for thresholding of
#'    the estimated CATE. Those segments with estimated CATEs exceeding this
#'    learned threshold are assigned treatment and others not. Note that this
#'    procedure fails to solve the so-called knapsack problem, instead choosing
#'    to treat those segments with the highest estimated CATEs while possibly
#'    failing to exhaust the given budget.
#' 3. \code{cost_knapsack}, which, similar to \code{cost_budget}, searches for
#'    a cost boundary for the assigment of a treatment rule data adaptively. In
#'    assigning segments to treatment, this routine solves the binary knapsack
#'    problem, using the approximate combinatorial optimization routine through
#'    \code{\link[adagio]{knapsack}}. The result of this routine often mirrors
#'    that of \code{cost_budget}, except that solving the knapsack problem
#'    ensures that the budget is approximately exhausted.
#'
#' @param data_with_cate A \code{\link[data.table]{data.table}} containing the
#'  input data, augmented with cross-validated nuisance parameter estimates and
#'  an estimate of the CATE. This input object should b ecreated by successive
#'  calls to \code{\link{set_est_data}} and \code{\link{est_cate}}, or through
#'  a wrapper function that composes these function calls automatically, and is
#'  passed on to the cost functions through \code{\link{assign_rule}}.
#' @param threshold A \code{numeric} value indicating the cutoff to be used in
#'  determining the treatment decision based on the estimated CATE. The default
#'  of zero assigns treatment to segments that ought to benefit from treatment
#'  while withholding treatment from those segments that ought to be harmed. It
#'  may suit to adjust this value based on the problem context. This is used in
#'  \code{\link{cost_threshold}}.
#' @param budget A \code{numeric} indicating the proportion of units to receive
#'  treatment. This proportion is used to discover a cutoff for thresholding of
#'  the estimated CATE. This is used in \code{\link{cost_budget}}.
#' @param cost_weights A \code{numeric} vector giving the observation-level
#'  weights to be used in computing the cutoff for thresholding of the CATE in
#'  \code{\link{cost_budget}}. By default, all units are given equal weight.
#' @param use_segment_treatment_cost A \code{logical} indicating whether the
#'  cost associated with treating a segment should be taken into account when
#'  deciding which segments to treat via \code{\link{cost_knapsack}}. This
#'  option defaults to \code{FALSE}, but if set to \code{TRUE}, the treatment
#'  cost must have been specified as an attribute previously.
#' @param type A \code{character} string (of length one) specifying how the
#'  treatment decision based on the CATE is to be made. There are two options:
#'  - \code{"inferential"} (the default) uses a hypothesis test to evaluate
#'    whether the estimated CATE is statistically different from a threshold
#'    and allocates a treatment decision based on the resultant p-value.
#'  - \code{"analytic"} simply evaluates whether the estimated CATE exceeds a
#'    given threshold and assigns treatment to segments for which this holds.
#'  Note that in both cases the threshold must be either provided directly (as
#'  an input to \code{\link{cost_threshold}}) or will be discovered based on
#'  specified constraints (as in \code{\link{cost_budget}}).
#'
#' @importFrom assertthat assert_that
#' @importFrom data.table setattr setkey setkeyv setnames ":="
#' @importFrom stats optimize weighted.mean
#' @importFrom adagio knapsack
#'
#' @family cost functions for treatment assignment based on the estimated CATE
#'
#' @name cost_funs
NULL

#' @rdname cost_funs
#' @export
cost_threshold <- function(data_with_cate,
                           threshold = 0,
                           type = c("inferential", "analytic")) {
  # set default rule type to use the hypothesis testing approach
  type <- match.arg(type)

  # get segmentation covariates
  segment_by <- attr(data_with_cate, "segment_by")

  # compute population segmentation (dynamic rule) based on CATE and threshold
  if (type == "analytic") {
    data_with_cate[, rule := as.numeric(cate > threshold)]
  } else if (type == "inferential") {
    pval_crit <- getOption("sherlock.pval_crit")
    test_segments(data_with_cate, threshold, segment_by, type = "rule")
    data_with_cate[, rule := cate > threshold & pval_seg <= pval_crit]
    data_with_cate[, rule := as.numeric(rule)]
    data_with_cate[, pval_seg := NULL]
  }

  # output modified data.table with assigned dynamic treatment rule
  data.table::setattr(data_with_cate, "threshold", threshold)
  data.table::setattr(data_with_cate, "budget", NA)
  data.table::setattr(data_with_cate, "use_segment_cost", FALSE) 
  return(data_with_cate[])
}

#' @rdname cost_funs
cost_budget <- function(data_with_cate,
                        budget,
                        cost_weights = rep(1, nrow(data_with_cate)),
                        type = c("inferential", "analytic")) {
  # set default rule type to use the hypothesis testing approach
  type <- match.arg(type)

  # check that weights are of the correct length
  assertthat::assert_that(length(cost_weights) == data_with_cate[, .N])

  # get segmentation covariates
  segment_by <- attr(data_with_cate, "segment_by")

  # NOTE: estimate cutoff as the positive part of the smallest solution to
  # mean[Ind(CATE > cutoff)] <= budget
  set_threshold <- function(threshold, cate, budget, cost_weights, type) {
    as.numeric(
      stats::weighted.mean(cate > threshold, w = cost_weights) <= budget
    )
  }
  threshold_est <- stats::optimize(
    f = set_threshold,
    interval = c(0, max(data_with_cate$cate)),
    cate = data_with_cate$cate,
    budget = budget,
    cost_weights = cost_weights,
    type = type,
    maximum = TRUE
  )
  threshold <- threshold_est$maximum

  # compute population segmentation (dynamic rule) based on estimated threshold
  if (type == "analytic") {
    data_with_cate[, rule := as.numeric(cate > threshold)]
  } else if (type == "inferential") {
    pval_crit <- getOption("sherlock.pval_crit")
    test_segments(data_with_cate, threshold, segment_by, type = "rule")
    data_with_cate[, rule := cate > threshold & pval_seg <= pval_crit]
    data_with_cate[, rule := as.numeric(rule)]
    data_with_cate[, pval_seg := NULL]
  }

  # output modified data.table with assigned dynamic treatment rule
  data.table::setattr(data_with_cate, "threshold", threshold)
  data.table::setattr(data_with_cate, "budget", budget)
  return(data_with_cate[])
}

#' @rdname cost_funs
#' @export
cost_knapsack <- function(data_with_cate,
                          budget,
                          use_segment_treatment_cost = FALSE,
                          type = c("inferential", "analytic")) {
  # extract useful attributes
  segment_by <- attr(data_with_cate, "segment_by")
  npsem <- attr(data_with_cate, "npsem")
  treatment_cost <- attr(data_with_cate, "treatment_cost")

  if (is.null(treatment_cost) & use_segment_treatment_cost) {
    stop("Treatment cost should be included in the data")
  }

  # first, run through cost_threshold to find only segments with positive CATE
  assign_rule(
    data_with_cate = data_with_cate,
    segment_fun = cost_threshold,
    threshold = 0,
    type = type
  )

  # summarize segments and subset to those benefiting from treatment
  segment_summary <- summarize_segments(
    data_with_rule = data_with_cate
  )
  data.table::setnames(
    segment_summary,
    npsem$V,
    segment_by
  )

  ### add segment cost to segment_summary, if use_treatment_cost
  ### it's possible to have segment cost, but not interested to use it as part of the constraint. 
  if ("avg_treatment_cost" %in% colnames(segment_summary) & use_segment_treatment_cost) {
    segment_summary[, segment_cost := avg_treatment_cost * segment_proportion]
  } else {
    segment_summary[, segment_cost := segment_proportion]
  }

  segment_summary_rule1 <- segment_summary[rule == 1, ]
  segment_summary_rule1[, threshold := NULL]
  segment_summary_rule1[, rule := NULL]

  ### check that not all costs are > budget
  if (budget < min(segment_summary_rule1$segment_cost)) {
    stop("Your budget is less than the minimum cost of treating a segment.")
  }
  ### take only the segments that have cost <= budget,
  ### otherwise adagio::knapsack will throw an error
  segment_summary_rule1 <- segment_summary_rule1[segment_cost <= budget, ]

  # reset assigned rule from cost_threshold
  data_with_cate[, rule := 0]

  # solve the binary knapsack problem for the groups benefiting from treatment
  fill_knapsack <- with(
    segment_summary_rule1,
    adagio::knapsack(
      # NOTE: all inputs must be integer
      p = (round(cate, 3) * 1000),
      w = (round(segment_cost, 3) * 1000),
      cap = (round(budget, 3) * 1000)
    )
  )

  # extract segments that should be treatment and map back to input data
  segments_rule1 <- segment_summary_rule1[fill_knapsack$indices, ..segment_by]
  data.table::setkeyv(data_with_cate, segment_by)
  data_with_cate[segments_rule1, rule := 1]

  # re-order by reseting key
  data.table::setkeyv(data_with_cate, "ids")
  data.table::setkey(data_with_cate, NULL)

  # output modified data.table with assigned dynamic treatment rule
  data.table::setattr(data_with_cate, "threshold", 0)
  data.table::setattr(data_with_cate, "budget", budget)
  data.table::setattr(data_with_cate, "use_segment_cost", use_segment_treatment_cost) 
  return(data_with_cate[])
}
