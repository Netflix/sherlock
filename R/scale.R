#' Scale values to the unit interval
#'
#' @param vals A \code{numeric} vector of values to be scaled into the closed
#'  unit interval.
#'
#' @keywords internal
scale_to_unit <- function(vals) {
  vals_scaled <- (vals - min(vals)) / (max(vals) - min(vals))
  return(vals_scaled)
}

#' Scale values from the unit interval to their original scale
#'
#' @param scaled_vals A \code{numeric} vector of values scaled to lie in the
#'  closed unit interval by the use of \code{\link{scale_to_unit}}.
#' @param max_orig The maximum of the values on the original scale.
#' @param min_orig The minimum of the values on the original scale.
#'
#' @keywords internal
scale_from_unit <- function(scaled_vals, max_orig, min_orig) {
  vals_orig <- (scaled_vals * (max_orig - min_orig)) + min_orig
  return(vals_orig)
}
