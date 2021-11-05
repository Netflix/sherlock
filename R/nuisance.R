#' Propensity Score Estimation
#'
#' Estimate the propensity score (PS), g(1|W), the probability of receiving
#' treatment conditional on baseline covariates. In a randomized experiment or
#' A/B test, this is usually known to be g(1|W) = 0.5. In such a case, this
#' known probability is passed in via \code{\link{Lrnr_prob_known}}.
#'
#' @param train_data A \code{\link[data.table]{data.table}} containing those
#'  observations falling in the training set for a particular cross-validation
#'  sample split. This data object is created by the internal call of
#'  \code{\link{est_cate}} to \code{\link[origami]{cross_validate}} and is
#'  (unfortunately) a copy of a subset of the full estimation data.
#' @param valid_data A \code{\link[data.table]{data.table}} containing those
#'  observations falling in the holdout (validation) set for a particular
#'  cross-validation sample split. This data object is created by the internal
#'  call of \code{\link{est_cate}} to \code{\link[origami]{cross_validate}} and
#'  is (unfortunately) a copy of a subset of the full estimation data.
#' @param learner An instantiated learner object, with class inheriting from
#'  \code{\link[sl3]{Lrnr_base}}, from \pkg{sl3}, to be used for estimation of
#'  the propensity score (the probability of receiving treatment, conditional
#'  on covariates). Note that the outcome of this estimation task is strictly
#'  binary and that algorithms or ensembles should be set up accordingly.
#'
#' @importFrom assertthat assert_that
#' @importFrom data.table as.data.table
#' @importFrom stringr str_subset
#' @importFrom sl3 sl3_Task
#'
#' @return A \code{list} (as required by \code{\link[origami]{cross_validate}})
#'  containing two slots populated with \code{\link[data.table]{data.table}}s
#'  for the training and validation data (for a given cross-validation split).
#'  Each \code{\link[data.table]{data.table}} has two columns, corresponding to
#'  estimates of the conditional probability of receiving the treatment ("g1")
#'  or the conditional probability of the treatment being withheld ("g0"). Note
#'  that values of these two columns should add up to one for any given row.
#'
#' @keywords internal
fit_ps_mech <- function(train_data, valid_data, learner) {
  # NOTE: check that learner is from sl3 to set up training/validation data
  assertthat::assert_that(inherits(learner, "Lrnr_base"))

  # task to fit learning algorithm for training data
  train_task <- sl3::sl3_Task$new(
    data = train_data,
    covariates = stringr::str_subset(colnames(train_data), "W"),
    outcome = "A",
    outcome_type = "binomial",
    id = "ids"
  )

  # task to predict from trained learning algorithm on validation data
  valid_task <- sl3::sl3_Task$new(
    data = valid_data,
    covariates = stringr::str_subset(colnames(valid_data), "W"),
    outcome = "A",
    outcome_type = "binomial",
    id = "ids"
  )

  # fit learner on training data
  learner_trained <- learner$train(train_task)

  # get "in-sample" predictions on training data
  pred_g1_train <- bound_propensity(learner_trained$predict(train_task))
  pred_train <- data.table::as.data.table(
    list(g0 = 1 - pred_g1_train, g1 = pred_g1_train)
  )

  # get holdout predictions on validation data
  pred_g1_valid <- bound_propensity(learner_trained$predict(valid_task))
  pred_valid <- data.table::as.data.table(
    list(g0 = 1 - pred_g1_valid, g1 = pred_g1_valid)
  )

  # truncate and return propensity score predictions
  pred_g <- list(train = pred_train, valid = pred_valid)
  return(pred_g)
}

#' Outcome Regression Estimation
#'
#' Estimate the outcome regression (OR), Q(A,W), the conditional mean of the
#' outcome/response variable, conditional on both the expsoure and the baseline
#' covariates.
#'
#' @param train_data A \code{\link[data.table]{data.table}} containing those
#'  observations falling in the training set for a particular cross-validation
#'  sample split. This data object is created by the internal call of
#'  \code{\link{est_cate}} to \code{\link[origami]{cross_validate}} and is
#'  (unfortunately) a copy of a subset of the full estimation data.
#' @param valid_data A \code{\link[data.table]{data.table}} containing those
#'  observations falling in the holdout (validation) set for a particular
#'  cross-validation sample split. This data object is created by the internal
#'  call of \code{\link{est_cate}} to \code{\link[origami]{cross_validate}} and
#'  is (unfortunately) a copy of a subset of the full estimation data.
#' @param learner An instantiated learner object, with class inheriting from
#'  \code{\link[sl3]{Lrnr_base}}, from \pkg{sl3}, to be used for estimation of
#'  the outcome regression (the mean of the response variable, conditional on
#'  exposure and covariates).
#'
#' @importFrom assertthat assert_that
#' @importFrom data.table as.data.table ":="
#' @importFrom stringr str_subset
#' @importFrom sl3 sl3_Task
#'
#' @return A \code{list} (as required by \code{\link[origami]{cross_validate}})
#'  containing two slots populated with \code{\link[data.table]{data.table}}s
#'  for the training and validation data (for a given cross-validation split).
#'  Each \code{\link[data.table]{data.table}} has three columns, corresponding
#'  to estimates of the conditional mean of the outcome under the natural value
#'  of the exposure ("QA"), under the control condition ("Q0"), and under the
#'  treatment condition ("Q1").
#'
#' @keywords internal
fit_or_mech <- function(train_data, valid_data, learner) {
  # NOTE: check that learner is from sl3 to set up training/validation data
  assertthat::assert_that(inherits(learner, "Lrnr_base"))

  # task to fit learning algorithm for training data
  train_task <- sl3::sl3_Task$new(
    data = train_data,
    covariates = c(stringr::str_subset(colnames(train_data), "W"), "A"),
    outcome = "Y",
    id = "ids"
  )

  # task to predict from trained learning algorithm on validation data
  valid_task <- sl3::sl3_Task$new(
    data = valid_data,
    covariates = c(stringr::str_subset(colnames(valid_data), "W"), "A"),
    outcome = "Y",
    id = "ids"
  )

  # fit learner on training data
  learner_trained <- learner$train(train_task)

  # get outcome mechanism predictions on counterfactual training data
  pred_QA_train <- learner_trained$predict(train_task)
  train_task$internal_data$raw_data[, A := 0]
  pred_Q0_train <- learner_trained$predict(train_task)
  train_task$internal_data$raw_data[, A := 1]
  pred_Q1_train <- learner_trained$predict(train_task)
  pred_train <- data.table::as.data.table(
    list(QA = pred_QA_train, Q0 = pred_Q0_train, Q1 = pred_Q1_train)
  )

  # get outcome mechanism predictions on counterfactual validation data
  pred_QA_valid <- learner_trained$predict(valid_task)
  valid_task$internal_data$raw_data[, A := 0]
  pred_Q0_valid <- learner_trained$predict(valid_task)
  valid_task$internal_data$raw_data[, A := 1]
  pred_Q1_valid <- learner_trained$predict(valid_task)
  pred_valid <- data.table::as.data.table(
    list(QA = pred_QA_valid, Q0 = pred_Q0_valid, Q1 = pred_Q1_valid)
  )

  # return outcome mechanism predictions
  pred_Q <- list(train = pred_train, valid = pred_valid)
  return(pred_Q)
}
