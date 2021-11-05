#' Print Method for Effects Summary Output
#'
#' @param x An object with class \code{sherlock_effects}, which should be a
#'  modified \code{\link[data.table]{data.table}}.
#' @param ... Other options (not currently used).
#' @param digits A \code{numeric} integer giving the number of digits to print
#'  in the entries of the summary table.
#'
#' @method print sherlock_effects
#'
#' @importFrom assertthat assert_that
#' @importFrom data.table as.data.table
#'
#' @return None. Called for the side effect of printing an informative summary
#'  table from objects of class \code{sherlock_effects}.
#'
#' @export
print.sherlock_effects <- function(x, ..., digits = 4L) {
  # check that a summary attribute slot has been populated
  param_type <- attr(x, "param_type")
  x <- data.table::as.data.table(x)
  print_row_output <- function(row_name) {
    cat(
      "\t Parameter Estimate: ",
      signif(x[param == row_name, ]$estimate, digits), "\n"
    )
    cat(
      "\t 95% Conf Interval:",
      paste("(", signif(x[param == row_name, ]$lwr_ci, digits), ", ",
            signif(x[param == row_name, ]$upr_ci, digits), ")",
            sep = ""
      ),
      "\n"
    )
    cat(
      "\t Standard Error: ",
      signif(x[param == row_name, ]$std_err, digits), "\n"
    )
  }

  if (param_type == "ote") {
    if ("tsm_trt_all" %in% x$param) {
      row_name <- "tsm_trt_all"
      cat("Counterfactual Mean Outcome If All Get Treatment\n")
      print_row_output(row_name)
    }

    if ("tsm_ctl_all" %in% x$param) {
      row_name <- "tsm_ctl_all"
      cat("Counterfactual Mean Outcome If All Get Control\n")
      print_row_output(row_name)
    }

    if ("tsm_trt_rule" %in% x$param) {
      row_name <- "tsm_trt_rule"
      cat("Counterfactual Mean Outcome If Treat Based on Segment Rule\n")
      print_row_output(row_name)
    }

    if ("ate_trt_rule_vs_trt_all" %in% x$param) {
      row_name <- "ate_trt_rule_vs_trt_all"
      cat("Optimal Treatment Effect: Treat based on Segment Rule vs. All Get Treatment\n")
      print_row_output(row_name)
    }

    if ("ate_trt_rule_vs_ctl_all" %in% x$param) {
      row_name <- "ate_trt_rule_vs_ctl_all"
      cat("Optimal Treatment Effect: Treat based on Segment Rule vs All Get Control\n")
      print_row_output(row_name)
    }
  } else if (param_type == "hte") {
    if ("ate_popn" %in% x$param) {
      row_name <- "ate_popn"
      cat("Average Treatment Effect (All Get Treatment vs All Get Control) on Population:\n")
      print_row_output(row_name)
    }
    if ("ate_rule1" %in% x$param) {
      row_name <- "ate_rule1"
      cat("Average Treatment Effect on Segments with Recommended Treatment (Rule = 1)\n")
      print_row_output(row_name)
    }
    if ("ate_rule0" %in% x$param) {
      row_name <- "ate_rule0"
      cat("Average Treatment Effect on Segments with Recommended Controle (Rule = 0)\n")
      print_row_output(row_name)
    }

    if ("hte_rule1_vs_rule0" %in% x$param) {
      row_name <- "hte_rule1_vs_rule0"
      cat("Difference in Average Treatment Effect: Segments with Recommended Treatment vs Segments with Recommended Control \n")
      print_row_output(row_name)
    }
  }
}
