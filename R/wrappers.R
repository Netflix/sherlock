#' Sherlock Consults By Inspecting the Data and Evaluating Conditional Effects
#'
#' Sherlock Holmes was a consulting detective who had spectacular powers of
#' deduction and logical reasoning. Within \pkg{sherlock}'s causal segmentation
#' framework, \code{sherlock_calculate} takes data from a segmentation "case",
#' the roles of the different variables, and specifications for assessing the
#' conditional treatment effects required for deriving a segmentation. Being
#' the workhorse, this function is the most demanding, as it computes all of
#' the nuisance parameters required for subsequent analyses. The complementary
#' functions \code{\link{watson_segment}} and \code{\link{mycroft_assess}} can
#' be used once Sherlock has consulted on the causal segmentation case.
#'
#' @param data_from_case Rectangular input data, whether a \code{data.frame},
#'  \code{\link[data.table]{data.table}}, or \code{\link[tibble]{tibble}}.
#' @param baseline A \code{character} vector specifying the column names in
#'  \code{data_obs} that correspond to the baseline covariates (conditioning
#'  set). These variables should temporally precede the exposure and outcome.
#' @param exposure A \code{character} string (of length one) specifying the
#'  column in \code{data_obs} corresponding to the exposure or treatment. This
#'  variable should follow those in \code{baseline} in time but precede the
#'  response variable \code{outcome}.
#' @param outcome A \code{character} string (of length one) specifying the
#'  column in \code{data_obs} corresponding to the response variable.
#' @param segment_by A \code{character} vector specifying the column names in
#'  \code{data_obs} that correspond to the covariates over which segmentation
#'  should be performed. This should be a strict subset of \code{baseline}.
#' @param ids A \code{character} string (of length one) specifying the column
#'  in \code{data_obs} that gives observation-level IDs. The default value of
#'  \code{NULL} assumes that all rows of \code{data_obs} are independent.
#' @param treatment_cost A \code{character} string (of length one) specifying
#'  the column in \code{data_obs} that provides the cost associated to treating
#'  the given unit. The default value of \code{NULL} assumes that all units are
#'  equally costly to treat.
#' @param cv_folds A \code{numeric} specifying the number of cross-validation
#'  folds to be used for sample-splitting when estimating nuisance parameters.
#' @param split_type A \code{character} string (of length one) indicating the
#'  sample-splitting "level" at which estimation of the CATE is performed. The
#'  choices are "inner", for estimation of the CATE within folds (i.e., at the
#'  the same level at which nuisance parameters are estimated), and "outer", in
#'  which case the CATE is estimated at the "full-sample" level.
#' @param ps_learner Either an instantiated learner object (class inheriting
#'  from \code{\link[sl3]{Lrnr_base}}), from \pkg{sl3}, or a \code{list} of
#'  specifications, or a constant rate between 0 and 1, to be used for
#'  estimation of the propensity score (the probability of receiving treatment,
#'  conditional on covariates). If \code{list}: each entry may be an
#'  instantiated learner object, or can be a list where one item is an
#'  instantiated learner object whose modeling requires specification, and the
#'  other item is a list of character vectors, where each vector specifies an
#'  interaction term. If constant rate, this rate represents the population
#'  probability of being assigned to treatment in an A/B tests. Note that the
#'  outcome of this estimation task is strictly binary and that algorithms or
#'  ensemble models should be set up accordingly.
#' @param or_learner Either an instantiated learner object (class inheriting
#'  from \code{\link[sl3]{Lrnr_base}}), from \pkg{sl3}, or a \code{list} of
#'  specifications, to be used for estimation of the outcome regression (the
#'  mean of the response variable, conditional on exposure and covariates). If
#'  \code{list}: each entry can be an instantiated learner object, or can be a
#'  list where one item is an instantiated learner object whose modeling
#'  requires specification, and the other item is a list of character vectors,
#'  where each vector specifies an interaction term.
#' @param cate_learner Either an instantiated learner object (class inheriting
#'  from \code{\link[sl3]{Lrnr_base}}), from \pkg{sl3}, or a \code{list} of
#'  specifications, to be used to estimate the CATE, based on a regression of a
#'  doubly robust pseudo-outcome on the specified segmentation covariates. If
#'  \code{list}: each entry can be an instantiated learner object, or can be a
#'  list where one item is an instantiated learner object whose modeling
#'  requires specification, and the other item is a list of character vectors,
#'  where each vector specifies an interaction term. Note that the outcome of
#'  this estimation task is derived from the other nuisance parameter estimates
#'  and should be expected to always be continuous-valued, so algorithms or
#'  ensemble models should be set up accordingly.
#' @param use_cv_selector If \code{TRUE}, then will use cross-validation to
#'  choose the best among a list of learners when fitting \code{ps_learner},
#'  \code{or_learner} or \code{cate_learner}. If \code{FALSE} (default), then
#'  the default metalearner for the outcome type (from \pkg{sl3}) will be used.
#'  This argument will not be ignored for a \code{learner} that is not a list,
#'  but is instead an instantiated learner object.
#'
#' @importFrom data.table setattr
#'
#' @export
sherlock_calculate <- function(data_from_case,
                               baseline,
                               exposure,
                               outcome,
                               segment_by,
                               ids = NULL,
                               treatment_cost = NULL,
                               cv_folds = 5L,
                               split_type = c("inner", "outer"),
                               ps_learner,
                               or_learner,
                               cate_learner,
                               use_cv_selector = FALSE) {
  # create estimation-ready data structure
  data_internal <- set_est_data(
    data_obs = data_from_case,
    baseline = baseline,
    exposure = exposure,
    outcome = outcome,
    ids = ids,
    segment_by = segment_by,
    treatment_cost = treatment_cost
  )

  # estimate all nuisance parameters
  data_with_cate_estimates <- est_cate(
    data_est_spec = data_internal,
    cv_folds = cv_folds,
    split_type = split_type,
    ps_learner = ps_learner,
    or_learner = or_learner,
    cate_learner = cate_learner,
    use_cv_selector = use_cv_selector
  )

  # create summary of segments without rule assignment
  segment_summary_norule <- summarize_segments(
    data_with_rule = data_with_cate_estimates
  )

  # output data.table with all nuisance parameter estimates
  data.table::setattr(
    x = data_with_cate_estimates,
    name = "summary",
    value = segment_summary_norule
  )

  # set custom class while preserving data.table
  data.table::setattr(
    x = data_with_cate_estimates,
    name = "class",
    value = unique(c("sherlock", class(data_with_cate_estimates)))
  )

  # output modified data.table, with custom class
  return(data_with_cate_estimates[])
}

#' Watson Advises Sherlock By Detecting Segments and Assigning Treatment Rules
#'
#' Dr. James H. Watson was Sherlock's friend, flatmate, and assistant. He often
#' contributed to Sherlock's work, usually by accompanying him in the field and
#' providing advice that could guide Sherlock towards a solution to the case.
#' Within \pkg{sherlock}'s causal segmentation framework, \code{watson_segment}
#' mirrors Dr. Watson's role by assigning a treatment rule after Sherlock has
#' completed a preliminary consultation via \code{\link{sherlock_calculate}}.
#' The output is a slightly augmented version of the data structure produced by
#' \code{\link{sherlock_calculate}}; however, this step is necessary. After Dr.
#' Watson has helped Sherlock in finalizing the segmentation analysis, the
#' segmentation quality may be evaluated via \code{\link{mycroft_assess}}.
#'
#' @param data_with_consult A \code{\link[data.table]{data.table}} containing
#'  the output \code{\link{sherlock_calculate}}, which matches the input data,
#'  augmented with cross-validated nuisance parameter estimates and an estimate
#'  of the CATE. A summary of the estimates across segmentation strata is made
#'  available as an attribute of the \code{\link[data.table]{data.table}}.
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
#'
#' @importFrom data.table as.data.table setattr
#'
#' @export
watson_segment <- function(data_with_consult,
                           segment_fun,
                           ...,
                           type = c("inferential", "analytic")) {
  # assign the treatment rule based on the cost function
  assign_rule(
    data_with_cate = data_with_consult,
    segment_fun = segment_fun,
    ...,
    type = type
  )

  # create summary of segments based on rule assignment
  segment_summary_withrule <- summarize_segments(
    data_with_rule = data_with_consult
  )
  segment_summary_withrule <- data.table::as.data.table(
    segment_summary_withrule
  )

  # output data.table with all nuisance parameter estimates
  data.table::setattr(
    x = data_with_consult,
    name = "summary",
    value = segment_summary_withrule
  )

  # set custom class while preserving data.table
  data.table::setattr(
    x = data_with_consult,
    name = "class",
    value = unique(c("sherlock", class(data_with_consult)))
  )

  # output modified data.table, with custom class
  return(data_with_consult[])
}

#' Mycroft Assesses the Population-level Causal Effects of the Segmentation
#'
#' Mycroft Holmes, Sherlock's elder brother, was a government official who
#' specialized in taking together facts from a variety of disparate sources and
#' putting together the best course of government action. His deductive skills
#' and logical reasoning abilities exceeded even those of Sherlock. Mycroft was
#' concerned more with the "big picture" than with the details of particular
#' cases. As such, the role of \code{mycroft_assess} within the \pkg{sherlock}
#' causal segmentation framework is to evaluate the population-level causal
#' effects that would result from following the course of action (i.e., dynamic
#' treatment) prescribed by Sherlock and Watson. For \code{mycroft_assess} to
#' work, both \code{\link{sherlock_calculate}} and \code{\link{watson_segment}}
#' must be called in sequence to evaluate the segmentation case and assign a
#' treatment decision to segments.
#'
#' @param data_from_advising A \code{\link[data.table]{data.table}} containing
#'  the output from successive calls to \code{\link{sherlock_calculate}} and
#'  \code{\link{watson_segment}}, which matches the input data, augmented with
#'  cross-validated nuisance parameter estimates and an estimate of the CATE. A
#'  summary of the estimates across segmentation strata is made available as an
#'  attribute of the \code{\link[data.table]{data.table}}.
#' @param param_type A \code{character} providing a specification for a family
#'  of target parameters to be estimated. The two choices provide estimates of
#'  a range of target parameters. Specifically, the choices correspond to
#'  1. \code{"hte"}, which computes the average treatment effect (ATE), which
#'     contrasts static interventions for treatment assignment and treatment
#'     being withhold, for (1) the full population, (2) within the subgroup of
#'     units dynamically assigned treatment, and (3) within the subgroup of
#'     units from whom treatment was withheld dynamically. Also included is the
#'     heterogeneous treatment effect (HTE), defined as a difference of the two
#'     subgroup ATEs, which captures the benefit attributable to treating those
#'     units who should receive treatment and withholding treatment from those
#'     that could be harmed by the treatment.
#'  2. \code{"ote"}, which computes several counterfactual means, for (1) the
#'     static intervention of assigning treatment to all units, (2) the static
#'     intervention of withholding treatment from all units, (3) the dynamic
#'     intervention of assigning treatment to those predicted to benefit from
#'     it while withholding treatment from those units that could be harmed. In
#'     addition, two average treatment effects are evaluated, each contrasting
#'     the dynamic treatment rule against the static interventions of assigning
#'     or withholding treatment. The latter three parameters capture contrasts
#'     based on the optimal treatment effect (OTE).
#' @param est_type A \code{character} specifying the type of estimator to be
#'  computed. Both estimators are asymptotically linear when flexible modeling
#'  choices are used to estimate nuisance parameters, doubly robust (consistent
#'  when at least one nuisance parameter is correctly estimated), and achieve
#'  the best possible variance (i.e., asymptotically efficient) among the class
#'  of regular asymptotically linear estimators. The two options are
#'  1. \code{"onestep"}, corresponding to the one-step estimator, a first-order
#'     solution to the efficient influence function (EIF) estimating equation.
#'     This is not a substitution (direct) estimator and may be unstable in the
#'     sense of yielding estimates outside the bounds of the target parameter.
#'  2. \code{"tmle"}, corresponding to the targeted minimum loss estimator, an
#'     approach that updates initial estimates of the outcome model by way of a
#'     one-dimensional fluctuation model that aims to approximately solve the
#'     EIF estimating equation.
#'
#' @export
mycroft_assess <- function(data_from_advising,
                           param_type = c("hte", "ote"),
                           est_type = c("onestep")) {
  # just a call to the effect estimation machinery
  # NOTE: we may eventually like to add other information here
  effect_est <- est_effect(
    data_from_advising,
    param_type = param_type,
    est_type = est_type
  )

  ## create custom class with print method
  data.table::setattr(
    x = effect_est,
    name = "class",
    value = unique(c("sherlock_effects", class(effect_est)))
  )

  # just output the effect measures
  return(effect_est[])
}

#' Print Method for Sherlock Summary Output
#'
#' @param x An object with class \code{sherlock}, which should be a modified
#'  \code{\link[data.table]{data.table}}.
#' @param ... Other options (not currently used).
#' @param digits A \code{numeric} integer giving the number of digits to print
#'  in the entries of the summary table.
#'
#' @method print sherlock
#'
#' @importFrom assertthat assert_that
#'
#' @return None. Called for the side effect of printing an informative summary
#'  table from objects of class \code{sherlock}.
#'
#' @export
print.sherlock <- function(x, ..., digits = 4L) {
  if (!is.null(attr(x, "summary"))) {
    # print the summary via data.table's print method
    print(attr(x, "summary"), digits = digits)
  } else {
    print(x)
  }
}

