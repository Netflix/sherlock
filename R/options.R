#' Set Options for Causal Segmentation Analysis
#'
#' @description Setter for global options used in causal segmentation analysis
#'  and supporting utilities. Currently supported are global options for the
#'  desired coverage rate of confidence intervals for the CATE and segmentation
#'  effect measures (TSMs, ATEs, HTEs, OTEs) and the critical p-value to be
#'  used when performing hypothesis tests of the CATE for the assignment of a
#'  treatment rule to discovered segments.
#'
#' @param option A global option of \pkg{sherlock}'s routines for causal
#'  segmentation analysis.
#' @param val The value to be assigned to the global option.
#'
#' @importFrom assertthat assert_that
#'
#' @return None, called for the side effect of setting global options.
#'
#' @keywords internal
set_sherlock_options <- function(option, val) {
  # check validity of candidate option
  if (option == "pval_crit") {
    assertthat::assert_that(is.numeric(val), val > 0, val < 1)
  } else if (option == "ci_covers") {
    assertthat::assert_that(is.numeric(val), val > 0, val < 1)
  } else {
    stop("Attempt to set unsupported option.")
  }

  # set given option
  switch(option,
    "pval_crit" = options(sherlock.pval_crit = val),
    "ci_covers" = options(sherlock.ci_covers = val)
  )
}
