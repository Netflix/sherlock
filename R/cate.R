#' Estimate the Conditional Average Treatment Effect Across Segments
#'
#' A procedure to estimate the conditional average treatment effect (CATE) via
#' a strategy that utilizes a regression of a doubly robust pseudo-outcome
#' derived from the form of the efficient influence function (a key quantity in
#' semiparametric statistics) on the segmentation covariates. Note that the
#' data for this estimation procedure is created based upon the specifications
#' provided in \code{\link{set_est_data}}, so this function only takes those
#' arguments directly relevant to nuisance parameter estimation.
#'
#' @param data_est_spec An input \code{\link[data.table]{data.table}} object
#'  created from the input data by \code{\link{set_est_data}}. Note that this
#'  data container object has specialized attributes appended to it, so it must
#'  be created by that internal utility function.
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
#'  @param use_cv_selector If \code{TRUE}, then will use Cross-Validation to 
#'  choose the best among a list of learners when fitting \code{ps_learner}, 
#'  \code{or_learner} or \code{cate_learner}. 
#'  If \code{FALSE} (default), then will use the default metalearner for the outcome type. 
#'  This argument will not be applied for a \code{learner} that is not a list but one instantiated learner object. 
#'  
#' @importFrom assertthat assert_that
#' @importFrom data.table rbindlist setattr setkeyv setcolorder setorderv ":="
#' @importFrom origami cross_validate make_folds folds_vfold
#' @importFrom Rsolnp solnp
#'
#' @return A \code{\link[data.table]{data.table}} of the full data, augmented
#'  with additional columns that specify estimates of the nuisance parameters,
#'  the doubly robust pseudo-outcome, and the estimated CATE.
#'
#' @keywords internal
est_cate <- function(data_est_spec,
                     cv_folds = 5L,
                     split_type = c("inner", "outer"),
                     ps_learner,
                     or_learner,
                     cate_learner,
                     use_cv_selector=F) {
  # make sure that more than one fold is specified for CV
  assertthat::assert_that(cv_folds > 1L)

  # set default for sample-split reuse
  split_type <- match.arg(split_type)

  # get names of segmentation covariates and extract NPSEM nodes
  outcome_type <- attr(data_est_spec, "outcome_type")
  segment_by <- attr(data_est_spec, "segment_by")
  npsem <- attr(data_est_spec, "npsem")
  treatment_cost <- attr(data_est_spec, "treatment_cost")

  # create cross-validation folds
  folds <- origami::make_folds(data_est_spec,
    fold_fun = origami::folds_vfold,
    V = cv_folds,
    cluster_ids = data_est_spec$ids
  )

  # handle known propensity score case by instantiating custom learner
  if (is.numeric(ps_learner)) {
    ps_learner <- Lrnr_prob_known$new(ps_learner)
  }

  ps_learner <- convert_to_learners(ps_learner, npsem = npsem,use_cv_selector=use_cv_selector)
  or_learner <- convert_to_learners(or_learner, npsem = npsem,use_cv_selector=use_cv_selector)
  cate_learner <- convert_to_learners(cate_learner, npsem = npsem,use_cv_selector=use_cv_selector)

  # compute PS, OR, and EIF estimates on a per-fold basis
  cv_dr_est_results <- origami::cross_validate(
    cv_fun = cv_dr_transform,
    folds = folds,
    data_est_spec = data_est_spec,
    ps_learner = ps_learner,
    or_learner = or_learner,
    cate_learner = cate_learner,
    split_type = split_type,
    use_future = FALSE,
    .combine = FALSE
  )

  # extract modified validation data and merge data.tables
  cv_cate_data_est <- data.table::rbindlist(cv_dr_est_results[[1]])
  reindex_by_ids <- order(cv_cate_data_est$ids)
  data.table::setorderv(cv_cate_data_est, "ids")

  # estimate CATE using validation predictions in a cross-validated manner
  # NOTE: this breaks the sample-split independence assumed by CV-TMLE
  if (split_type == "outer") {
    # compute pseudo-outcome regression on segmentation covariates
    cv_cate_pred <- origami::cross_validate(
      cv_fun = cv_fit_cate,
      folds = folds,
      data_for_cate = cv_cate_data_est,
      segment_by = segment_by,
      cate_learner = cate_learner,
      use_future = FALSE,
      .combine = FALSE
    )

    # extract CATE predictions and append to data structure
    cv_cate_pred <- do.call(c, lapply(cv_cate_pred[[1]], `[[`, "pred"))
    cv_cate_data_est[, cate := cv_cate_pred[reindex_by_ids]]
  } else if (split_type == "inner") {
    # NOTE: just a sanity check that CATE predictions already exist
    assertthat::assert_that("cate" %in% colnames(cv_cate_data_est))
  }

  # return CATE estimates for segmentation
  data.table::setattr(cv_cate_data_est, "outcome_type", outcome_type)
  data.table::setattr(cv_cate_data_est, "segment_by", segment_by)
  data.table::setattr(cv_cate_data_est, "npsem", npsem)
  data.table::setattr(cv_cate_data_est, "treatment_cost", treatment_cost)
  data.table::setkeyv(cv_cate_data_est, c("ids", "fold"))
  data.table::setcolorder(cv_cate_data_est)
  return(cv_cate_data_est[])
}

#' Cross-Validated Fitting of the Conditional Average Treatment Effect
#'
#' Estimation of the conditional average treatment effect (CATE), either within
#' particular folds induced by cross-validation sample-splitting or upon the
#' full data. In the latter case, a copy of the full data is made (which is NOT
#' recommended). Whether the CATE is estimated within folds or not is specified
#' by the \code{split_type} argument of \code{\link{est_cate}}. Estimation of
#' the CATE is performed by computing the conditional mean of the doubly robust
#' transformed pseudo-outcome on the specified set of segmenation covariates.
#'
#' @param fold An object specifying the cross-validation folds into which the
#'  observations fall, as generated by \code{\link[origami]{make_folds}}.
#' @param data_for_cate A \code{\link[data.table]{data.table}} with additional
#'  columns containing nuisance parameter estimates (propensity score, outcome
#'  regression, doubly robust transformed pseudo-outcome) appended. A dataset
#'  of this form can be used directly for estimation of the CATE.
#' @param segment_by A \code{character} vector specifying the column names in
#'  \code{data_obs} that correspond to the covariates over which segmentation
#'  should be performed. This should be a strict subset of \code{baseline}.
#' @param cate_learner An instantiated learner object, with class inheriting
#'  from \code{\link[sl3]{Lrnr_base}}, from \pkg{sl3}, to be used to estimate
#'  the CATE, based on a regression of a doubly robust pseudo-outcome on the
#'  specified segmentation covariates. Note that the outcome of this estimation
#'  task is derived from the other nuisance parameter estimates and should be
#'  expected to always be continuous-valued, so algorithms or ensembles should
#'  be set up accordingly.
#'
#' @importFrom assertthat assert_that
#' @importFrom data.table copy ":="
#' @importFrom origami training validation
#' @importFrom sl3 sl3_Task
#'
#' @return A \code{list} (as required by \code{\link[origami]{cross_validate}})
#'  containing a trained \pkg{sl3} learner object in its first slot and the
#'  predicted values of the CATE in its second slot.
#'
#' @keywords internal
cv_fit_cate <- function(fold = NULL, data_for_cate, segment_by, cate_learner) {
  if (!is.null(fold)) {
    cate_data_train <- origami::training(data_for_cate)
    cate_data_valid <- origami::validation(data_for_cate)
  } else {
    cate_data_train <- data.table::copy(data_for_cate)
    cate_data_valid <- data.table::copy(data_for_cate)
  }

  # NOTE: check that learner is from sl3 to set up training/validation data
  assertthat::assert_that(inherits(cate_learner, "Lrnr_base"))

  # task to fit pseudo-outcome regression for CATE estimation
  cate_train_task <- sl3::sl3_Task$new(
    data = cate_data_train,
    covariates = segment_by,
    outcome = "eif_dr",
    outcome_type = "continuous",
    id = "ids"
  )

  # task to predict from trained pseudo-outcome regression for CATE
  cate_valid_task <- sl3::sl3_Task$new(
    data = cate_data_valid,
    covariates = segment_by,
    outcome = "eif_dr",
    outcome_type = "continuous",
    id = "ids"
  )

  # fit learner on training data and get CATE predictions
  cate_learner_trained <- cate_learner$train(cate_train_task)
  pred_cate <- cate_learner_trained$predict(cate_valid_task)

  # output CATE predictions
  cate_out <- list(trained_learner = cate_learner_trained, pred = pred_cate)
  return(list(cate_out))
}

#' Cross-Validated Doubly Robust Pseudo-Outcome Transformation
#'
#' Generation of the doubly robust pseudo-outcome required for estimation of
#' the conditional average treatment effect (CATE) based upon a transformation
#' using the form of the efficient influence function (EIF; a key quantity in
#' semiparametric statistics) of the average treatment effect. Creation of the
#' pseudo-outcome happens within a particular cross-validation fold; however,
#' depending on the value of the argument \code{split_type} either the CATE is
#' estimated by a call to \code{\link{cv_fit_cate}} or a placeholder column for
#' the CATE estimate is generated. In the latter case, actual estimation of the
#' CATE is deferred to an optional step in \code{\link{est_cate}}, in which the
#' CATE is estimated on the full data.
#'
#' @param fold An object specifying the cross-validation folds into which the
#'  observations fall, as generated by \code{\link[origami]{make_folds}}.
#' @param data_est_spec An input \code{\link[data.table]{data.table}} object
#'  created from the input data by \code{\link{set_est_data}}. Note that this
#'  data container object has specialized attributes appended to it, so it must
#'  be created by that internal utility function.
#' @param ps_learner An instantiated learner object, with class inheriting from
#'  \code{\link[sl3]{Lrnr_base}}, from \pkg{sl3}, to be used for estimation of
#'  the propensity score (the probability of receiving treatment, conditional
#'  on covariates). Note that the outcome of this estimation task is strictly
#'  binary and that algorithms or ensembles should be set up accordingly.
#' @param or_learner An instantiated learner object, with class inheriting from
#'  \code{\link[sl3]{Lrnr_base}}, from \pkg{sl3}, to be used for estimation of
#'  the outcome regression (the mean of the response variable, conditional on
#'  exposure and covariates).
#' @param cate_learner An instantiated learner object, with class inheriting
#'  from \code{\link[sl3]{Lrnr_base}}, from \pkg{sl3}, to be used to estimate
#'  the CATE, based on a regression of a doubly robust pseudo-outcome on the
#'  specified segmentation covariates. Note that the outcome of this estimation
#'  task is derived from the other nuisance parameter estimates and should be
#'  expected to always be continuous-valued, so algorithms or ensembles should
#'  be set up accordingly.
#' @param split_type A \code{character} string (of length one) indicating the
#'  sample-splitting "level" at which estimation of the CATE is performed. The
#'  choices are "inner", for estimation of the CATE within folds (i.e., at the
#'  the same level at which nuisance parameters are estimated), and "outer", in
#'  which case the CATE is estimated at the "full-sample" level.
#'
#' @importFrom data.table ":="
#' @importFrom origami fold_index training validation
#' @importFrom sl3 sl3_Task
#'
#' @return A \code{list} (as required by \code{\link[origami]{cross_validate}})
#'  containing a \code{\link[data.table]{data.table}} of the validation sample
#'  for the given cross-validation fold, augmented with additional columns that
#'  specify the nuisance parameter estimates, the doubly robust pseudo-outcome,
#'  and (possibly) the estimated CATE.
#'
#' @keywords internal
cv_dr_transform <- function(fold,
                            data_est_spec,
                            ps_learner,
                            or_learner,
                            cate_learner,
                            split_type) {
  # get names of segmentation covariates
  segment_by <- attr(data_est_spec, "segment_by")

  # make training and validation data
  train_data <- origami::training(data_est_spec)
  valid_data <- origami::validation(data_est_spec)

  # fit models and get predictions for treatment/outcome mechanisms
  ps_est <- fit_ps_mech(train_data, valid_data, ps_learner)
  or_est <- fit_or_mech(train_data, valid_data, or_learner)

  # append predictions to training data and compute doubly robust transform
  # NOTE: D = [(2A-1) / pA(A|W)] * [Y - EY(Y|A,W)] + [EY(Y|1,W) - EY(Y|0,W)]
  if (split_type == "inner") {
    train_data[, (names(ps_est$train)) := ps_est$train]
    train_data[, (names(or_est$train)) := or_est$train]
    train_data[, eif_dr := (((2 * A - 1) / (A * g1 + (1 - A) * g0)) * (Y - QA)
      + (Q1 - Q0))]

    # fit CATE model in training data to get predictions in validation data
    cate_fitted_out <- cv_fit_cate(
      data_for_cate = train_data,
      segment_by = segment_by,
      cate_learner = cate_learner
    )
  }

  # append predictions to validation data
  valid_data[, (names(ps_est$valid)) := ps_est$valid]
  valid_data[, (names(or_est$valid)) := or_est$valid]

  # construct doubly robust transformation of CATE in validation data
  valid_data[, eif_dr := (((2 * A - 1) / (A * g1 + (1 - A) * g0)) * (Y - QA) +
    (Q1 - Q0))]

  # get CATE predictions in validation data if appropriate
  if (split_type == "inner") {
    # extract trained CATE learner
    cate_learner_trained <- cate_fitted_out[[1]]$trained_learner

    # create task for prediction from validation data
    cate_valid_task <- sl3::sl3_Task$new(
      data = valid_data,
      covariates = segment_by,
      outcome = "eif_dr",
      outcome_type = "continuous",
      id = "ids"
    )

    # append validation predictions from trained CATE learner
    valid_data[, cate := cate_learner_trained$predict(cate_valid_task)]
  } else if (split_type == "outer") {
    # initialize CATE prediction column
    valid_data[, cate := NA_real_]
  }

  # return modified validation data
  valid_data[, fold := origami::fold_index()]
  return(list(valid_data[]))
}
