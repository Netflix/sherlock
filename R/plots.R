#' Plot Method for Sherlock Summary Output
#'
#' @param x An object with class \code{sherlock}, which should be a modified
#'  \code{\link[data.table]{data.table}}.
#' @param plot_type A \code{character} indicating the type of plot to be made.
#'  Use \code{"cate"} to plot only the CATE values across segments. When
#'  treatment decisions have been assigned by \code{\link{watson_segment}},
#'  then \code{"treatment_decisions"} can be used to plot out the treatment
#'  rules, alongside the CATE estimates.
#' @param sort_by_significance A \code{logical}. If \code{TRUE}, the results
#'  will be sorted based on the size of CATE estimates and treatment rules;
#'  if \code{FALSE}, then values will be sorted by the segment dimensions.
#' @param ... Other options (not currently used).
#' @param digits A \code{numeric} integer giving the number of digits to print
#'  in the entries of the summary table.
#'
#' @method plot sherlock
#'
#' @import ggplot2
#' @importFrom dplyr case_when "%>%"
#'
#' @return ggplot object.
#'
#' @export
plot.sherlock <- function(x, plot_type = c("cate", "treatment_decisions"),
                          sort_by_significance = TRUE, digits = 3L, ...) {
  # check for summary slot
  if (is.null(attr(x, "summary"))) {
    stop("There is no summary to plot")
  }

  # select segmentation covariates and make summary data frame for plotting
  segment_by <- attr(x, "npsem")$V
  df <- attr(x, "summary") %>% as.data.frame
  df$plot_segments <-
    Map(paste, segment_by, attr(x, "summary")[, ..segment_by], sep = "=") %>%
    data.frame() %>%
    apply(1, paste, collapse =  "\n")

  # check for treatment decisions if requested as plot type
  if (plot_type == "treatment_decisions" & !("rule" %in% names(df))) {
    stop("First use `watson_segment` to make treatment decisions for segments")
  }

  # color codes
  inf_codes <- c(
    negative = "coral4", positive = "darkolivegreen4",
    indeterminate = "darkgrey"
  )
  decision_codes <- c(
    `treatment=1` = "darkviolet", `treatment=0` = "darkcyan"
  )

  if (plot_type == "cate") {
    df$Inference <- dplyr::case_when(
      df$lwr_ci > 0 ~ "positive",
      df$upr_ci < 0 ~ "negative",
      TRUE ~ "indeterminate"
    )

    ## order by size of CATE
    if (sort_by_significance) df <- df[order(abs(df$cate)), ]
    df$seg_ind <- seq_along(df$plot_segments)

    threshold <- 0
    title <- "Treatment Effect Heterogeneity by Segment"
    color_legend <- "CATE Inference"
    color_codes <- inf_codes
    vline_label <- ""
  }
  if (plot_type == "treatment_decisions") {
    df$Inference <- paste0("treatment=", df$rule)

    ## order by size of CATE
    if (sort_by_significance) {
      df <- df[order(df$rule, abs(df$cate)), ]
    }
    df$seg_ind <- 1:length(df$plot_segments)
    df$treatment_cost <- paste0(round(df$segment_proportion, 2) * 100, "%")
    sec_axis_lab <- "% popn"

    threshold <- attr(x, "threshold")
    budget <- attr(x, "budget")
    use_segment_cost <- attr(x, "use_segment_cost")

    title <- "Treatment decision rule by segment"

    if (is.na(budget)) {
      title <- paste0(title, ": CATE > ", threshold)
    } else {
      if (!use_segment_cost) {
        title <- paste0(title, ", \nif treat at most ",
                        budget * 100, "% of popn")
      } else {
        title <- paste0(
          title, ", \nconstrained to average treatment cost of ", budget,
          " per popn unit"
        )
        df$treatment_cost <-
          round(df$segment_proportion * df$avg_treatment_cost, digits)
        sec_axis_lab <- "avg segment cost"
      }
    }
    color_legend <- "Treatment decisions rule: "
    color_codes <- decision_codes
    vline_label <- paste0("threshold=", threshold)
  }

  plot_object <- ggplot(
      data = df, aes(x = cate, y = seg_ind, color = Inference)
    ) +
    # geom_crossbar(aes(xmin=lwr_ci, xmax=upr_ci),) +
    geom_point(size = 4) +
    geom_errorbar(aes(xmin = lwr_ci, xmax = upr_ci), width = 0.4) +
    scale_color_manual(values = color_codes, drop = TRUE) +
    scale_fill_manual(values = color_codes)

  if (plot_type == "cate" || is.na(budget)) {
    plot_object <- plot_object +
      scale_y_continuous(
        breaks = df$seg_ind,
        labels = df$plot_segments,
        name = "Segments"
      )
  } else {
    plot_object <- plot_object +
      scale_y_continuous(
        breaks = df$seg_ind,
        labels = df$plot_segments,
        name = "Segments",
        sec.axis = dup_axis(labels = df$treatment_cost, name = sec_axis_lab)
      )
  }

  plot_object <- plot_object +
    theme_minimal() +
    theme(
      strip.text = element_blank(),
      legend.position = "bottom"
    ) +
    xlab("CATE effect estimates with confidence intervals for each segment") +
    labs(color = color_legend) +
    geom_vline(xintercept = threshold, linetype = "dashed") +
    geom_text(x = threshold, y = 0.5, label = vline_label, color = "black") +
    ggtitle(title)
  print(plot_object)
}
