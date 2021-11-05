#' Converts List of Base Learner Specs into one Super Learner Spec
#'
#' The "Learner Spec" objects in both \code{\link{sherlock_calculate}} and
#' \code{\link{est_cate}} can be an instantiated learner object, or a
#' \code{list} of learners to be instantiated. To use base learners that
#' require model specification (e.g., GLMs or regularized regression), then we
#' need to specify the interaction terms using the original column names from
#' the input dataset. This internal function converts those names to the
#' abstracted names.
#'
#' @param learner Either an instantiated learner object, with class inheriting
#'  from \code{\link[sl3]{Lrnr_base}}, from \pkg{sl3}, or a \code{list}. If
#'  \code{list}, each entry can be an instantiated learner object, or can be a
#'  list where one item is an instantiated learner object whose modeling
#'  requires specification, and the other item is a list of character vectors,
#'  where each vector specifies an interaction term.
#'  
#'  @param use_cv_selector If \code{TRUE}, then will use Cross-Validation to 
#'  choose the best among a list of learners. If \code{FALSE}, then will use the default
#'  metalearner for the outcome type. 
#'  If the \code{learner} argument is not a list but one instantiated learner object, 
#'  then this argument is ignored.
#'
#' @importFrom dplyr bind_rows
#' @importFrom sl3 Lrnr_define_interactions Lrnr_sl Lrnr_cv_selector Pipeline
#'
#' @return an instantiated learner object, with class inheriting from
#'  \code{\link[sl3]{Lrnr_base}}, from \pkg{sl3}.
#'
#' @keywords internal
convert_to_learners <- function(learner, npsem, use_cv_selector=F) {
  ## if already a Lrnr_base object
  if ("Lrnr_base" %in% class(learner)) {
    return(learner)
  }
  
  ## if not, check if all list are base learners:
  is_base_lrnr <- sapply(learner, function(ll) "Lrnr_base" %in% class(ll))
  
  ## if if not all base learner, then convert interaction terms spec to base learners
  if (!all(is_base_lrnr)) {
    
    ### data_dict 
    data_dict <- dplyr::bind_rows(
      lapply(setdiff(names(npsem), "V"), function(node_group) {
        ret <- data.frame(
          node = npsem[[node_group]],
          abstract = node_group
        )
        if (nrow(ret) > 1) {
          ret$abstract <- paste0(ret$abstract, 1:nrow(ret))
        }
        ret
      })
    )
    
    for (ind in which(!is_base_lrnr)) {
      if (length(learner[[ind]]) != 2) {
        stop("Failure to specify a list of the interaction terms and the fitting method (learner). See the vignette for examples.")
      }
      
      base_idx <- sapply(learner[[ind]], function(ll) {
        "Lrnr_base" %in% class(ll)
      })
      
      if (sum(base_idx) != 1 | sum(!base_idx) != 1) {
        stop("must have one base learner pair with an interaction list")
      }
      
      if (!all(unlist(learner[[ind]][[which(!base_idx)]]) %in%
               unlist(npsem))) {
        stop("interaction terms need to be in the data")
      }
      
      ## translate the interactions from data names to abstract names
      interactions <- lapply(learner[[ind]][[which(!base_idx)]], function(tt) {
        data_dict$abstract[match(tt, data_dict$node)]
      })
      
      lrn_interaction <- sl3::Lrnr_define_interactions$new(interactions)
      learner[[ind]] <- sl3::Pipeline$new(
        lrn_interaction, learner[[ind]][[which(base_idx)]]
      )
    }
  }
  
  if(!use_cv_selector)
    return(sl3::Lrnr_sl$new(learners = learner))
  else 
    return(sl3::Lrnr_sl$new(learners = learner,metalearner = Lrnr_cv_selector$new())) 
}
