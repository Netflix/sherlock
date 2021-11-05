utils::globalVariables(c("HnA", "Hn0", "Hn1"))

#' Fit Fluctuation Model for Targeted Minimum Loss Estimation
#'
#' @param data_with_rule A \code{\link[data.table]{data.table}} containing the
#'  input data, augmented with cross-validated nuisance parameter estimates, an
#'  estimate of the CATE, and a treatment rule assigned based on the estimated
#'  CATE via \code{\link{assign_rule}}. This input object should be created by
#'  successive calls to \code{\link{set_est_data}}, \code{\link{est_cate}}, and
#'  \code{\link{assign_rule}} in a sequence, or through a wrapper function that
#'  composes these function calls automatically.
#' @param aux_covar A \code{character} string (of length one) indicating the
#'  column of the input data object, \code{data_with_rule}, that corresponds to
#'  the auxiliary ("clever") covariate of the fluctuation model. Note that the
#'  form of this term depends on the efficient influence function of the target
#'  parameter and should be computed externally and appended to the input data.
#' @param tol A \code{numeric} indicating the maximum size of the parameter
#'  estimate of the one-dimensional fluctuation model. Any estimates larger
#'  than this tolerance value are set to zero.
#'
#' @importFrom assertthat assert_that
#' @importFrom data.table as.data.table ":="
#' @importFrom stats as.formula coef glm qlogis plogis
#'
#' @return A \code{\link[data.table]{data.table}} with all of the information
#'  contained in the input but augmented to include additional columns for the
#'  updated outcome mechanism based on the one-dimensional fluctuation model.
#'
#' @keywords internal
fluc_update <- function(data_with_rule, aux_covar, tol = 5) {
  stop("TMLE implementation is under construction. Use the one-step instead.")

  # browser()

  # run fluctuation model and extract estimated coefficient
  fluc_mod_fit <- stats::glm(
    stats::as.formula("Y_01 ~ -1 + offset(QA_logit) + Hn"),
    data = data.table::as.data.table(list(
      Y_01 = data_with_rule[, scale_to_unit(Y)],
      QA_logit = data_with_rule[
        ,
        stats::qlogis(bound_tolerance(scale_to_unit(QA)))
      ],
      Hn = data_with_rule[, .SD, .SDcols = aux_covar]
    )),
    family = "binomial",
    start = 0
  )
  fluc_coef <- stats::coef(fluc_mod_fit)

  # check fluctuation model and implement safeguards
  if (any(is.na(fluc_coef))) {
    fluc_mod_fit$coefficients[unname(which(is.na(fluc_coef)))] <- 0
  } else if (!fluc_mod_fit$converged || abs(max(fluc_coef, na.rm = T)) > tol) {
    fluc_mod_fit$coefficients <- 0
  }

  # browser()

  # update components of substitution estimator based on fluctuation
  # NOTE: make sure to use the correct (counterfactual) clever covariates!
  data_with_rule[
    ,
    QA_star := stats::plogis(fluc_coef * HnA +
      stats::qlogis(bound_tolerance(scale_to_unit(QA))))
  ]
  data_with_rule[
    ,
    Q0_star := stats::plogis(fluc_coef * Hn0 +
      stats::qlogis(bound_tolerance(scale_to_unit(Q0))))
  ]
  data_with_rule[
    ,
    Q1_star := stats::plogis(fluc_coef * Hn1 +
      stats::qlogis(bound_tolerance(scale_to_unit(Q1))))
  ]

  # rescale updated nuisance components to original outcome bounds
  data_with_rule[, QA_star := scale_from_unit(QA_star, max(QA), min(QA))]
  data_with_rule[, Q0_star := scale_from_unit(Q0_star, max(Q0), min(Q0))]
  data_with_rule[, Q1_star := scale_from_unit(Q1_star, max(Q1), min(Q1))]

  # output updated data.table, including fluctuated outcome mechanism
  return(data_with_rule[])
}
