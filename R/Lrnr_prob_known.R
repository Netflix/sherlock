#' Predict with Probability Known A Priori
#'
#' This learner predicts a binary outcome with a known probability, provided by
#' the user as input.
#'
#' @docType class
#'
#' @importFrom sl3 Lrnr_base args_to_list
#' @importFrom R6 R6Class
#'
#' @export
#'
#' @keywords data
#'
#' @return A learner object inheriting from \code{\link{Lrnr_base}} with
#'  methods for training and prediction. For a full list of learner
#'  functionality, see the complete documentation of \code{\link{Lrnr_base}}.
#'
#' @format An \code{\link[R6]{R6Class}} object inheriting from
#'  \code{\link{Lrnr_base}}.
#'
#' @family Learners
#'
#' @section Parameters:
#'   - \code{prob}: The probability of the binary outcome. When not provided,
#'       this defaults to the mean proportion of observed ones.
#'
#' @examples
#' library(sl3)
#' data(cpp_imputed)
#' covs <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs")
#' task <- sl3_Task$new(cpp_imputed, covariates = covs, outcome = "smoked")
#'
#' # known probability, as in an RCT
#' lrnr_rct <- Lrnr_prob_known$new(0.5)
#' rct_fit <- lrnr_rct$train(task)
#' rct_preds <- rct_fit$predict()
Lrnr_prob_known <- R6::R6Class(
  classname = "Lrnr_prob_known",
  inherit = sl3::Lrnr_base, portable = TRUE, class = TRUE,
  public = list(
    initialize = function(prob = NULL, ...) {
      super$initialize(params = sl3::args_to_list(), ...)
    }
  ),
  private = list(
    .properties = c("binary"),
    .train = function(task) {
      args <- self$params
      outcome_type <- self$get_outcome_type(task)

      # explicitly check that outcome type is binary
      assertthat::assert_that(outcome_type$type == "binomial")

      # specify data
      y <- outcome_type$format(task$Y)

      if (task$has_node("weights")) {
        wts <- task$weights
      } else {
        wts <- rep(1, task$nrow)
      }

      # create fit object
      fit_object <- list(mean = weighted.mean(y, wts))
      if (!is.null(args$prob)) {
        fit_object$mean <- args$prob
      }
      return(fit_object)
    },
    .predict = function(task) {
      predictions <- rep(private$.fit_object$mean, task$nrow)
      predictions <- as.matrix(predictions, ncol = 1)
      return(predictions)
    }
  )
)
