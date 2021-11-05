#' Bounding to Numerical Tolerance
#'
#' Bounds extreme values to a specified tolerance level, for use with sensitive
#' quantities that must be transformed, e.g., via \code{\link[stats]{qlogis}}.
#'
#' @param vals A \code{numeric} vector of values in the closed unit interval.
#' @param tol A \code{numeric} indicating the tolerance limit to which extreme
#'  values should be truncated. Realizations of \code{val} less than \code{tol}
#'  are truncated to \code{tol} while those greater than (1 - \code{tol}) are
#'  truncated to (1 - \code{tol}).
#'
#' @keywords internal
bound_tolerance <- function(vals, tol = 1e-4) {
  vals <- bound_propensity(vals, bounds = c(tol, 1 - tol))
  return(vals)
}

#' Bounding Propensity Scores
#'
#' Bounds estimated propensity score values to be within a specified range.
#'
#' @param vals A \code{numeric} vector of values in the closed unit interval.
#' @param bounds A \code{numeric} vector containing two values, the first being
#'  the minimum allowable value and the second being the maximum allowable for
#'  values appearing in the vector \code{vals} (the previous argument).
#'
#'
#' @keywords internal
bound_propensity <- function(vals, bounds = c(0.025, 0.975)) {
  vals <- pmin(pmax(vals, bounds[1]), bounds[2])
  return(vals)
}
