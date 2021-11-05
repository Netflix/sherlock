#' Efficient Estimation of Causal Effects Under Dynamic Treatment Rules
#'
#' Compute efficient estimates of the heterogeneous treatment effect (HTE) or
#' the optimal treatment effect (OTE), as well as related parameters, including
#' subgroup average treatment effects and the counterfactual mean of assigning
#' the dynamic treatment rule.
#'
#' @param data_with_rule A \code{\link[data.table]{data.table}} containing the
#'  input data, augmented with cross-validated nuisance parameter estimates, an
#'  estimate of the CATE, and a treatment rule assigned based on the estimated
#'  CATE via \code{\link{assign_rule}}. This input object should be created by
#'  successive calls to \code{\link{set_est_data}}, \code{\link{est_cate}}, and
#'  \code{\link{assign_rule}} in a sequence, or through a wrapper function that
#'  composes these function calls automatically.
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
#' @importFrom assertthat assert_that
#' @importFrom data.table ":=" rbindlist as.data.table setattr setcolorder
#' @importFrom stats plogis qlogis
#'
#' @return A \code{\link[data.table]{data.table}} with a handful of rows, one
#'  for each target parameter estimated, and columns giving the parameter name
#'  the point estimate of the target parameter, and the standard error of the
#'  estimate. Also included are the lower and upper confidence limits for the
#'  point estimated based on Wald-style confidence intervals.
#'
#' @keywords internal
est_effect <- function(data_with_rule,
                       param_type = c("hte", "ote"),
                       est_type = c("onestep", "tmle")) {
  # set default estimator as one step
  est_type <- match.arg(est_type)

  # check that choice of target parameter correctly specified
  assertthat::assert_that(
    length(param_type) == 1,
    msg = "A single target parameter family must be selected."
  )

  # build out auxiliary covariates for all parameters for convenience
  data_with_rule[, Hn_tsm_trt := A / g1]
  data_with_rule[, Hn_tsm_ctl := (1 - A) / g0]
  data_with_rule[, Hn_tsm_dopt := (A == rule) / (rule * g1 + (1 - rule) * g0)]
  data_with_rule[, Hn_ate_static := (2 * A - 1) / (A * g1 + (1 - A) * g0)]

  if (param_type == "hte") {
    # population-level ATE estimate, standard error, and Wald-style CI
    est_ate_static <- est_ate(
      data_with_rule,
      ate_param = "ate_static", est_type
    )
    est_ate_static[, param := "ate_popn"]
    ci_wald(
      est_ate_static,
      param_type = "effect_measure",
      outcome_type = "continuous"
    )

    # estimated EIF for population-level ATE
    eif_ate_static <- data.table::setnames(
      data.table::as.data.table(attr(est_ate_static, "eif")),
      "ate_popn"
    )

    if (all(data_with_rule[, table(rule)] > 0)) {
      # subgroup-specific ATE
      est_ate_static_rule1 <- est_ate(
        data_with_rule[rule == 1, ],
        ate_param = "ate_static", est_type
      )
      est_ate_static_rule1[, param := "ate_rule1"]
      ci_wald(
        est_ate_static_rule1,
        param_type = "effect_measure",
        outcome_type = "continuous"
      )

      est_ate_static_rule0 <- est_ate(
        data_with_rule[rule == 0, ],
        ate_param = "ate_static", est_type
      )
      est_ate_static_rule0[, param := "ate_rule0"]
      ci_wald(
        est_ate_static_rule0,
        param_type = "effect_measure",
        outcome_type = "continuous"
      )

      # HTE based on subgroup-specific ATE estimates
      est_hte <- data.table::data.table(
        estimate = est_ate_static_rule1$estimate -
          est_ate_static_rule0$estimate,
        std_err = sqrt(est_ate_static_rule1$std_err^2 +
          est_ate_static_rule0$std_err^2),
        param = "hte_rule1_vs_rule0"
      )
      ci_wald(
        est_hte,
        param_type = "effect_measure",
        outcome_type = "continuous"
      )

      # combine subgroup-specific estimates with static ATE estimate
      est_combined <- data.table::rbindlist(
        list(
          est_ate_static, est_ate_static_rule1,
          est_ate_static_rule0, est_hte
        )
      )
    }
  } else if (param_type == "ote") {
    # TSM for static intervention of treating uniformly and add Wald-style CI
    est_tsm_trt <- est_tsm(
      data_with_rule,
      tsm_param = "tsm_static_trt", est_type
    )
    ci_wald(
      est_tsm_trt,
      param_type = "effect_measure",
      outcome_type = attr(data_with_rule, "outcome_type")
    )

    # TSM for static intervention of withholding treatment uniformly
    est_tsm_ctl <- est_tsm(
      data_with_rule,
      tsm_param = "tsm_static_ctl", est_type
    )
    ci_wald(
      est_tsm_ctl,
      param_type = "effect_measure",
      outcome_type = attr(data_with_rule, "outcome_type")
    )

    # TSM for the dynamic intervention of treating those benefiting
    est_tsm_dopt <- est_tsm(
      data_with_rule,
      tsm_param = "tsm_dynamic_opt", est_type
    )
    ci_wald(
      est_tsm_dopt,
      param_type = "effect_measure",
      outcome_type = attr(data_with_rule, "outcome_type")
    )

    # combine estimates into a single data.table
    est_tsm_combined <- data.table::rbindlist(
      list(
        tsm_trt_all = est_tsm_trt,
        tsm_ctl_all = est_tsm_ctl,
        tsm_trt_rule = est_tsm_dopt
      ),
      idcol = "param"
    )
    eif_tsm_combined <- data.table::as.data.table(
      list(
        tsm_trt_all = attr(est_tsm_trt, "eif"),
        tsm_ctl_all = attr(est_tsm_ctl, "eif"),
        tsm_trt_rule = attr(est_tsm_dopt, "eif")
      )
    )

    # ATE of the dynamic intervention against control E(Y(d)-Y(0))
    est_ate_dopt_ctl <- est_ate(
      data_with_rule,
      ate_param = "ate_dopt_ctl", est_type
    )
    ci_wald(
      est_ate_dopt_ctl,
      param_type = "effect_measure",
      outcome_type = "continuous"
    )

    # ATE of the dynamic intervention against treatment E(Y(d)-Y(1))
    est_ate_dopt_trt <- est_ate(
      data_with_rule,
      ate_param = "ate_dopt_trt", est_type
    )
    ci_wald(
      est_ate_dopt_trt,
      param_type = "effect_measure",
      outcome_type = "continuous"
    )

    # combine tables of estimates for the optimal treatment effect
    est_ote_combined <- data.table::rbindlist(
      list(
        ate_trt_rule_vs_trt_all = est_ate_dopt_trt,
        ate_trt_rule_vs_ctl_all = est_ate_dopt_ctl
      ),
      idcol = "param"
    )
    eif_ote_combined <- data.table::as.data.table(
      list(
        ate_trt_rule_vs_trt_all = attr(est_ate_dopt_ctl, "eif"),
        ate_trt_rule_vs_ctl_all = attr(est_ate_dopt_trt, "eif")
      )
    )

    # combine outputs, set estimated EIF as attribute, and improve column names
    est_combined <- rbind(est_tsm_combined, est_ote_combined)
    data.table::setattr(est_combined, "tsm_eif", eif_tsm_combined)
    data.table::setattr(est_combined, "ote_eif", eif_ote_combined)
  }

  data.table::setattr(est_combined, "param_type", param_type)
  data.table::setcolorder(
    est_combined,
    c("param", "estimate", "lwr_ci", "upr_ci", "std_err")
  )
  return(est_combined)
}

#' Average Treatment Effect of Treating the Optimal Segment
#'
#' Evaluate the average treatment effect (ATE) based on static interventions or
#' within subgroups depending on treatment assignment. Efficient one-step and
#' TML estimators are available for each of these target parameters.
#'
#' @param data_with_rule A \code{\link[data.table]{data.table}} containing the
#'  input data, augmented with cross-validated nuisance parameter estimates, an
#'  estimate of the CATE, and a treatment rule assigned based on the estimated
#'  CATE via \code{\link{assign_rule}}. This input object should be created by
#'  successive calls to \code{\link{set_est_data}}, \code{\link{est_cate}}, and
#'  \code{\link{assign_rule}} in a sequence, or through a wrapper function that
#'  composes these function calls automatically.
#' @param ate_param A \code{character} string (of length one) identifying the
#'  target parameter to be estimated. Choices are \code{"ate_static"} for the
#'  ATE attributable to the static intervention contrasting the two treatment
#'  options at the population level, \code{"ate_dopt_ctl"} for the ATE in the
#'  subgroup identified as potentially being harmed by treatment, as well as
#'  \code{"ate_dopt_trt"} for the ATE in the subgroup identified as benefiting
#'  from treatment.
#' @param est_type Specification of either the one-step or TML estimator. See
#'  the documentation of \code{\link{est_effect}} for details.
#'
#' @importFrom data.table as.data.table setattr
#' @importFrom stats var
#'
#' @return A \code{\link[data.table]{data.table}} containing point estimates
#'  and the standard error of the estimates of the specified target parameter.
#'  The resulting object contains the estimated efficient influence function as
#'  an attribute appended to the object via \code{\link[data.table]{setattr}}.
#'
#' @keywords internal
est_ate <- function(data_with_rule, ate_param, est_type) {
  # compute the selected efficient estimator
  if (est_type == "tmle") {
    # compute the fluctuation model update for TMLE
    suppressWarnings(
      fluc_update(data_with_rule, "Hn_ate_static")
    )

    if (ate_param == "ate_static") {
      # compute updated substitution estimator and EIF
      ate_est <- data_with_rule[, mean(Q1_star - Q0_star)]
      ate_eif <- data_with_rule[
        ,
        Hn_ate_static * (Y - QA_star) + (Q1_star - Q0_star)
      ] - ate_est
    } else if (ate_param == "ate_dopt_ctl") {
      # compute updated substitution estimator and EIF
      ate_est <- data_with_rule[, mean(rule * (Q1_star - Q0_star))]
      ate_eif <- data_with_rule[
        ,
        rule * (Hn_ate_static * (Y - QA_star) + (Q1_star - Q0_star))
      ] - ate_est
    } else if (ate_param == "ate_dopt_trt") {
      # compute updated substitution estimator and EIF
      ate_est <- data_with_rule[, -1 * mean((1 - rule) * (Q1_star - Q0_star))]
      ate_eif <- data_with_rule[
        ,
        -1 * (1 - rule) * (Hn_ate_static * (Y - QA_star) + (Q1_star - Q0_star))
      ] - ate_est
    } else {
      stop(paste(ate_param, "not a supported `ate_param`."))
    }
  } else if (est_type == "onestep") {
    if (ate_param == "ate_static") {
      # compute uncentered EIF for the one-step estimator
      ate_os_eif <- data_with_rule[, Hn_ate_static * (Y - QA) + (Q1 - Q0)]
    } else if (ate_param == "ate_dopt_ctl") {
      # write out expression to simplify debugging
      ate_os_eif <- data_with_rule[
        ,
        (Hn_tsm_dopt - Hn_tsm_ctl) * (Y - QA) +
          (rule * Q1 + (1 - rule) * Q0) - Q0
      ]
    } else if (ate_param == "ate_dopt_trt") {
      # compute uncentered EIF for the one-step estimator
      ate_os_eif <- data_with_rule[
        ,
        (Hn_tsm_dopt - Hn_tsm_trt) * (Y - QA) +
          (rule * Q1 + (1 - rule) * Q0) - Q1
      ]
    }
    # compute one-step updated estimator as solution to the EIF
    ate_est <- mean(ate_os_eif)
    ate_eif <- ate_os_eif - ate_est
  }

  # smooth the EIF estimates over repeated IDs for variance estimation
  # and compute variance and standard error estimates from reduced EIF
  data_with_rule[, ate_eif := ate_eif]
  ate_eif_summary <- data_with_rule[, .(ate_eif_mean = mean(ate_eif)),
    by = ids
  ]
  data_with_rule[, ate_eif := NULL]
  ate_var <- ate_eif_summary[, stats::var(ate_eif_mean) / .N]
  ate_std_err <- sqrt(ate_var)

  # return object with point and standard error estimates
  out <- data.table::as.data.table(
    list(estimate = ate_est, std_err = ate_std_err)
  )
  data.table::setattr(out, "eif", as.numeric(ate_eif))
  return(out)
}

#' Efficient Estimation of Counterfactual Means of Dynamic Rules
#'
#' Evaluate the treatment-specific mean (TSM) based on static interventions or
#' on the dynamic treatment rule. Efficient one-step and TML estimators are
#' available for each of these target parameters.
#'
#' @param data_with_rule A \code{\link[data.table]{data.table}} containing the
#'  input data, augmented with cross-validated nuisance parameter estimates, an
#'  estimate of the CATE, and a treatment rule assigned based on the estimated
#'  CATE via \code{\link{assign_rule}}. This input object should be created by
#'  successive calls to \code{\link{set_est_data}}, \code{\link{est_cate}}, and
#'  \code{\link{assign_rule}} in a sequence, or through a wrapper function that
#'  composes these function calls automatically.
#' @param tsm_param A \code{character} string (of length one) identifying the
#'  treatment-specific mean to be estimated. \code{"tsm_static_trt"} gives the
#'  counterfactual mean under the static intervention assigning treatment to
#'  all units; \code{"tsm_static_ctl"} gives the counterfactual mean under the
#'  static intervention withholding treatment from all units; and, lastly,
#'  \code{"tsm_dynamic_opt"} gives the counterfactual mean under the dynamic
#'  rule assigning treatment based on potential benefit/harm from treatment.
#' @param est_type Specification of either the one-step or TML estimator. See
#'  the documentation of \code{\link{est_effect}} for details.
#'
#' @importFrom data.table as.data.table setattr
#' @importFrom stats var
#'
#' @return A \code{\link[data.table]{data.table}} containing point estimates
#'  and the standard error of the estimates of the specified target parameter.
#'  The resulting object contains the estimated efficient influence function as
#'  an attribute appended to the object via \code{\link[data.table]{setattr}}.
#'
#' @keywords internal
est_tsm <- function(data_with_rule, tsm_param, est_type) {
  # compute the selected efficient estimator
  if (est_type == "tmle") {
    if (tsm_param == "tsm_static_trt") {
      # compute the fluctuation model update for TMLE
      suppressWarnings(
        fluc_update(data_with_rule, "Hn_tsm_trt")
      )

      # compute updated substitution estimator and EIF
      tsm_est <- data_with_rule[, mean(Q1_star)]
      tsm_eif <- data_with_rule[
        ,
        Hn_tsm_trt * (Y - QA_star) + Q1_star
      ] - tsm_est
    } else if (tsm_param == "tsm_static_ctl") {
      # compute the fluctuation model update for TMLE
      suppressWarnings(
        fluc_update(data_with_rule, "Hn_tsm_ctl")
      )

      # compute updated substitution estimator and EIF
      tsm_est <- data_with_rule[, mean(Q0_star)]
      tsm_eif <- data_with_rule[
        ,
        Hn_tsm_ctl * (Y - QA_star) + Q0_star
      ] - tsm_est
    } else if (tsm_param == "tsm_dynamic_opt") {
      # compute the fluctuation model update for TMLE
      suppressWarnings(
        fluc_update(data_with_rule, "Hn_tsm_dopt")
      )

      # compute updated substitution estimator and EIF
      tsm_est <- data_with_rule[, mean(rule * Q1_star + (1 - rule) * Q0_star)]
      tsm_eif <- data_with_rule[
        ,
        Hn_tsm_dopt * (Y - QA_star) + (rule * Q1_star + (1 - rule) * Q0_star)
      ] - tsm_est
    }
  } else if (est_type == "onestep") {
    if (tsm_param == "tsm_static_trt") {
      # compute uncentered EIF for the one-step estimator
      tsm_os_eif <- data_with_rule[, Hn_tsm_trt * (Y - QA) + Q1]
    } else if (tsm_param == "tsm_static_ctl") {
      # compute uncentered EIF for the one-step estimator
      tsm_os_eif <- data_with_rule[, Hn_tsm_ctl * (Y - QA) + Q0]
    } else if (tsm_param == "tsm_dynamic_opt") {
      # compute uncentered EIF for the one-step estimator
      tsm_os_eif <- data_with_rule[
        ,
        Hn_tsm_dopt * (Y - QA) + (rule * Q1 + (1 - rule) * Q0)
      ]
    }
    # compute one-step updated estimator as solution to the EIF
    tsm_est <- mean(tsm_os_eif)
    tsm_eif <- tsm_os_eif - tsm_est
  }

  # smooth the EIF estimates over repeated IDs for variance estimation
  # and compute variance and standard error estimates from reduced EIF
  data_with_rule[, tsm_eif := tsm_eif]
  tsm_eif_summary <- data_with_rule[, .(tsm_eif_mean = mean(tsm_eif)),
    by = ids
  ]
  data_with_rule[, tsm_eif := NULL]
  tsm_var <- tsm_eif_summary[, stats::var(tsm_eif_mean) / .N]
  tsm_std_err <- sqrt(tsm_var)

  # return object with point and standard error estimates
  out <- data.table::as.data.table(
    list(estimate = tsm_est, std_err = tsm_std_err)
  )
  data.table::setattr(out, "eif", as.numeric(tsm_eif))
  return(out)
}
