#' Assign Dynamic Treatment Rule from Conditional Average Treatment Effect
#'
#' Perform the assignment of a treatment rule based on the conditional average
#' treatment effect (CATE) estimate for each individual in the observed data.
#' The procedure uses a cost function (see \code{\link{cost_funs}}) to decide
#' whether a given unit should be assigned to the treatment or the control
#' condition based on the estimated CATE. The treatment decision accommodates
#' both hard thresholding of the CATE (i.e., analytic evaluation of whether it
#' exceeds a cutoff) and soft thresholding (i.e., assessing difference of the
#' CATE estimate from a threshold via a one-sided hypothesis test), with the
#' latter being the default. Uniqueness of the assigned treatment rule within
#' segmentation strata is the default, allowing for stratum-specific inference;
#' however, when such inference is not of interest, this may be disabled.
#'
#' @param data_with_cate A \code{\link[data.table]{data.table}} containing the
#'  input data, augmented with cross-validated nuisance parameter estimates and
#'  an estimate of the CATE. This input object should b ecreated by successive
#'  calls to \code{\link{set_est_data}} and \code{\link{est_cate}}, or through
#'  a wrapper function that composes these function calls automatically.
#' @param segment_fun A particular choice of function for the assignment of a
#'  treatment rule to a segment based on a specified threshold or constraint.
#'  For details on these, consult the documentation in \code{\link{cost_funs}}.
#' @param ... Additional arguments passed to \code{segment_fun}. For details,
#'  see the documentation for the cost functions in \code{\link{cost_funs}}.
#' @param type A \code{character} string (of length one) specifying how the
#'  treatment decision based on the CATE is to be made. There are two options:
#'  - \code{"inferential"} (the default) uses a hypothesis test to evaluate
#'    whether the estimated CATE is statistically different from a threshold
#'    and assigns a treatment decision based on the resultant p-value.
#'  - \code{"analytic"} simply evaluates whether the estimated CATE exceeds a
#'    given threshold and assigns treatment to segments for which this holds.
#'  Note that in both cases the threshold must be either provided directly (as
#'  an input to \code{\link{cost_threshold}}) or will be discovered based on
#'  specified constraints (as in \code{\link{cost_budget}}).
#' @param unique_rule A \code{logical} indicating whether the treatment rule to
#'  be assigned ought to be unique within segments. The default of \code{TRUE}
#'  ensures that all units belonging the the same segment receive an identical
#'  treatment decision. When this is set to \code{FALSE}, subsequent utilities
#'  (e.g., \code{\link{summarize_segments}}) cannot be used to summarize the
#'  assigned treatment rule across segments.
#'
#' @importFrom assertthat assert_that
#' @importFrom data.table setattr ":="
#'
#' @return A \code{\link[data.table]{data.table}} of the full input data
#'  \code{data_cate_est}, augmented with a single additional column \code{rule}
#'  that specifies the treatment decision derived from the estimated CATE and a
#'  strategy specified by the segmentation function \code{segment_fun}.
#'
#' @keywords internal
assign_rule <- function(data_with_cate,
                        segment_fun = cost_threshold,
                        ...,
                        type = c("inferential", "analytic"),
                        unique_rule = TRUE) {
  # get segmentation covariates
  segment_by <- attr(data_with_cate, "segment_by")

  # check that input data contains CATE estimate
  assertthat::assert_that("cate" %in% colnames(data_with_cate))

  # set rule uniquity as DT attribute
  data.table::setattr(data_with_cate, "is_rule_unique", unique_rule)

  # set unique CATE by smoothing over units in a given segmentation stratum
  if (unique_rule) {
    data_with_cate[, cate := mean(cate), by = segment_by]
  }

  # use segmentation function to derive dynamic rule from estimated CATE
  segment_fun(data_with_cate = data_with_cate, ..., type = type)
  assertthat::assert_that("rule" %in% colnames(data_with_cate))

  # output modified CATE data and
  return(data_with_cate[])
}

#' Summarize CATE Estimates and Dynamic Treatment Rule Across Segments
#'
#' Produce a summary of the segments and the segment-specific treatment rule
#' assignment based on estimates of the conditional average treatment effect
#' (CATE) within each segment. When the dynamic treatment rule has not yet been
#' assigned via \code{\link{assign_rule}}, just segment specific summaries of
#' the CATE estimates and their standard error are returned.
#'
#' @param data_with_rule A \code{\link[data.table]{data.table}} containing the
#'  input data, augmented with cross-validated nuisance parameter estimates, an
#'  estimate of the CATE, and a treatment rule assigned based on the estimated
#'  CATE via \code{\link{assign_rule}}. This input object should be created by
#'  successive calls to \code{\link{set_est_data}}, \code{\link{est_cate}}, and
#'  \code{\link{assign_rule}} in a sequence, or through a wrapper function that
#'  composes these function calls automatically. When \code{\link{assign_rule}}
#'  has not been called, summary of the rule assignment and relevant evaluation
#'  of the CATE is skipped and only segment-specific CATE summaries are output.
#'
#' @importFrom assertthat assert_that
#' @importFrom data.table setcolorder setkeyv setorderv ":="
#'
#' @return A \code{\link[data.table]{data.table}} containing the discovered
#'  segments, the estimated CATE within each segment (with inference for the
#'  CATE), the segment-specific treatment rule assignment, and auxiliary info
#'  for assessing the quality of the segmentation.
#'
#' @keywords internal
summarize_segments <- function(data_with_rule) {
  # get segmentation covariates and NPSEM
  outcome_type <- attr(data_with_rule, "outcome_type")
  segment_by <- attr(data_with_rule, "segment_by")
  npsem <- attr(data_with_rule, "npsem")

  # check that input data contains CATE estimate
  assertthat::assert_that("cate" %in% colnames(data_with_rule))

  # conditional logic based on whether rule assignment has been performed
  if ("rule" %in% colnames(data_with_rule)) {
    # create summary tables based on segmentation covariates
    threshold <- attr(data_with_rule, "threshold")
    segment_summary <- test_segments(data_with_rule, threshold, segment_by,
      type = "summary"
    )
    rule_summary <- data_with_rule[, .(rule = unique(rule)), by = segment_by]

    # re-key summary DTs and add rule to segmentation summary
    data.table::setkeyv(segment_summary, segment_by)
    data.table::setkeyv(rule_summary, segment_by)
    segment_summary[rule_summary, rule := rule]
    segment_summary[, threshold := threshold]

    # clean up and output segmentation summary
    data.table::setcolorder(
      segment_summary,
      c(segment_by, "count", "threshold")
    )
    # to avoid conflict with routines externally adjusting p-values
    # segment_summary[, pval := NULL]
  } else {
    # smooth the EIF estimates over repeated IDs for variance estimation
    data_with_rule[, eif_dr := mean(eif_dr), by = ids]

    # compute segment-specific summary of the estimated CATE

    if (is.null(attr(data_with_rule, "treatment_cost"))) { ## if no treatment_cost to summarize
      segment_summary <- data_with_rule[, .(
        count = .N, cate = mean(cate),
        est_var = (stats::var(eif_dr) / .N)
      ),
      by = segment_by
      ]
    } else {
      segment_summary <- data_with_rule[, .(
        count = .N, cate = mean(cate),
        est_var = (stats::var(eif_dr) / .N),
        avg_treatment_cost = mean(get(attr(data_with_rule, "treatment_cost")))
      ),
      by = segment_by
      ]
    }

    # simply compute the segment-specific standard error and remove variance
    segment_summary[, std_err := sqrt(est_var)]
    segment_summary[, est_var := NULL]
  }

  # compute confidence interval for segment-specific CATE estimates
  ci_wald(
    estimation_results = segment_summary,
    segment_by = segment_by,
    param_type = "cate",
    outcome_type = "continuous"
  )

  # TODO: add proportion of units falling in each segment
  segment_summary[, count := count / sum(count)]
  data.table::setnames(segment_summary, "count", "segment_proportion")

  # re-arrange rows based on segmentation covariate realizations
  data.table::setorderv(segment_summary, segment_by)

  # replace generic names of segmentation columns with given names from input
  data.table::setnames(segment_summary, segment_by, npsem$V)
  return(segment_summary[])
}

#' Hypothesis Testing of the Segmented Conditional Average Treatment Effects
#'
#' Perform a one-sided hypothesis test of whether the estimate conditional
#' average treatment effect (CATE) exceeds a given threshold. In particular,
#' this is a test of the null hypothesis H0: CATE <= threshold against the
#' alternative hypothesis H1: CATE > threshold. This function returns the
#' relevant metrics (e.g., standard error, test statistic) for evaluating this
#' hypothesis test, augmenting the input \code{\link[data.table]{data.table}}.
#'
#' @param data_with_cate A \code{\link[data.table]{data.table}} containing the
#'  input data, augmented with cross-validated nuisance parameter estimates and
#'  an estimate of the CATE. This input object should b ecreated by successive
#'  calls to \code{\link{set_est_data}} and \code{\link{est_cate}}, or through
#'  a wrapper function that composes these function calls automatically.
#' @param threshold A \code{numeric} value indicating the cutoff to be used in
#'  determining the treatment decision based on the estimated CATE. The default
#'  of zero assigns treatment to segments that ought to benefit from treatment
#'  while withholding treatment from those segments that ought to be harmed. It
#'  may suit to adjust this value based on the problem context.
#' @param segment_by A \code{character} vector specifying the column names in
#'  \code{data_obs} that correspond to the covariates over which segmentation
#'  should be performed. This should be a strict subset of \code{baseline}.
#' @param type A \code{character} string (of length one) indicating whether the
#'  hypothesis testing procedure is meant to assign the segment-specific rule
#'  to all units in the input \code{data_with_cate} or to return a simple table
#'  summarizing the segment-specific rule assignment and inference across only
#'  the segmentation strata.
#'
#' @importFrom assertthat assert_that
#' @importFrom data.table setorderv ":="
#' @importFrom stats var pnorm p.adjust
#'
#' @return A \code{\link[data.table]{data.table}}, of one of two types:
#'  1. Either the full input data, augmented with the segment-specific CATE,
#'     the estimated standard error of the estimated CATE, a test statistic for
#'     evaluating the statistical difference of the CATE from a threshold, and
#'     the segment-specific p-value for this test of a difference. Note that
#'     this table is of the same length as the input data, that is, there is a
#'     row for each observation and observation-level nuisance estimates.
#'  2. A summary table containing identifying information for the segments as
#'     well as all of the same information noted above for the estimated CATE
#'     and the associated hypothesis test. Note that the length of this table
#'     matches the number of discovered segments, that is, it is a summary of
#'     the segment-specific information; consequently, all observation-level
#'     information is discarded from this summary table.
#'
#' @keywords internal
test_segments <- function(data_with_cate,
                          threshold,
                          segment_by,
                          type = c("rule", "summary")) {
  # check that assigned dynamic treatment rule is unique
  rule_uniquity <- attr(data_with_cate, "is_rule_unique")
  assertthat::assert_that(
    isTRUE(rule_uniquity),
    msg = "Treatment rule is non-unique, but this is needed to test segments."
  )

  # smooth the EIF estimates over repeated IDs for variance estimation
  data_with_cate[, eif_dr := mean(eif_dr), by = ids]

  # summarize data into segments for hypothesis testing of CATE > threshold

  if (is.null(attr(data_with_cate, "treatment_cost"))) { ## if no treatment_cost to summarize
    segment_test_summary <- data_with_cate[,
      .(count = .N, cate = unique(cate), est_var = (stats::var(eif_dr) / .N)),
      by = segment_by
    ]
  } else {
    segment_test_summary <- data_with_cate[,
      .(
        count = .N, cate = unique(cate), est_var = (stats::var(eif_dr) / .N),
        avg_treatment_cost = mean(get(attr(data_with_cate, "treatment_cost")))
      ),
      by = segment_by
    ]
  }
  data.table::setorderv(segment_test_summary, segment_by)

  # one-sided hypothesis test of CATE > threshold, with multiplicity correction
  segment_test_summary[, std_err := sqrt(est_var)]
  segment_test_summary[, est_var := NULL]
  segment_test_summary[, test_stat := ((cate - threshold) / std_err)]
  segment_test_summary[, pval := stats::pnorm(-test_stat)]
  segment_test_summary[, pval_adj := stats::p.adjust(pval, "fdr")]

  if (type == "rule") {
    # re-assign segment-specific summaries to input data.table
    data.table::setorderv(data_with_cate, segment_by)
    data_with_cate[, pval_seg := segment_test_summary[, rep(pval_adj, count)]]
    data.table::setorderv(data_with_cate, "ids")
    return(data_with_cate[])
  } else if (type == "summary") {
    # return just the summary table across segments
    return(segment_test_summary[])
  }
}
