#' Inspect the Input Data and Prepare for Estimation Routines
#'
#' A utility function to map input data to abstractions useful for estimation.
#' This procedure makes a single copy of the input data, storing it as a
#' \code{\link[data.table]{data.table}} for internal usage in later methods.
#'
#' @param data_obs Rectangular input data object, whether a \code{data.frame},
#'  \code{\link[data.table]{data.table}}, or \code{\link[tibble]{tibble}}.
#' @param baseline A \code{character} vector specifying the column names in
#'  \code{data_obs} that correspond to the baseline covariates (conditioning
#'  set). These variables should temporally precede the exposure and outcome.
#' @param exposure A \code{character} string (of length one) specifying the
#'  column in \code{data_obs} corresponding to the exposure or treatment. This
#'  variable should follow those in \code{baseline} in time but precede the
#'  response variable \code{outcome}.
#' @param outcome A \code{character} string (of length one) specifying the
#'  column in \code{data_obs} corresponding to the response variable.
#' @param segment_by A \code{character} vector specifying the column names in
#'  \code{data_obs} that correspond to the covariates over which segmentation
#'  should be performed. This should be a strict subset of \code{baseline}.
#' @param ids A \code{character} string (of length one) specifying the column
#'  in \code{data_obs} that gives observation-level IDs. The default value of
#'  \code{NULL} assumes that all rows of \code{data_obs} are independent.
#' @param treatment_cost A \code{character} string (of length one) specifying the column
#'  in \code{data_obs} that gives observation-level treatment cost. The default value of
#'  \code{NULL} indicates that this is not a cost constrained
#'  problem or that cost is constant for all units.
#'
#' @importFrom assertthat assert_that
#' @importFrom data.table as.data.table is.data.table copy set setDT setattr
#' @importFrom tibble is_tibble
#'
#' @keywords internal
set_est_data <- function(data_obs,
                         baseline,
                         exposure,
                         outcome,
                         segment_by,
                         ids = NULL, treatment_cost = NULL) {
  # internal data representation uses data.table for efficient processing
  # NOTE: hopefully, we'll only ever make this one copy of the input data
  if (is.matrix(data_obs)) {
    data_est <- data.table::as.data.table(data.table::copy(data_obs))
  } else if (is.data.frame(data_obs) ||
    data.table::is.data.table(data_obs) ||
    tibble::is_tibble(data_obs)) {
    data_est <- data.table::copy(data_obs)
    data.table::setDT(data_est)
  } else {
    stop("Input data must be matrix, data.frame, data.table, or tibble")
  }

  # set ID if not specified (as per default)
  if (is.null(ids)) {
    data_est[, ids := seq_len(.N)]
    ids <- "ids"
  }

  if (!is.null(treatment_cost)) {
    assertthat::assert_that(treatment_cost %in% colnames(data_est))
    if (treatment_cost %in% c(baseline, exposure, outcome, ids)) {
      stop("treatment cost cannot be baseline, exposure, outcome or id variables")
    }
  }

  # create dictionary of names of NPSEM nodes
  npsem_nodes <- list(
    W = baseline,
    A = exposure,
    Y = outcome,
    V = segment_by
  )

  # create dictionary of standardized names in data set for each NPSEM node
  data_dict <- list(
    W = paste0("W", seq_along(npsem_nodes$W)),
    A = "A",
    Y = "Y"
  )

  # standardize names in dataset using NPSEM and data dictionaries
  lapply(seq_along(npsem_nodes[-4]), function(node_idx) {
    # set old name as given in user-created input
    old_names <- npsem_nodes[[node_idx]]

    # set new name from internal bookkeeping list
    new_names <- data_dict[[names(npsem_nodes[node_idx])]]

    # overwrite original column names with standardized names
    if (!is.null(old_names)) {
      data.table::setnames(
        data_est,
        old = old_names, new = new_names, skip_absent = TRUE
      )
    }
  })

  # get the standardized names of the segmentation covariates
  segment_names_abstract <- data_dict[[1]][npsem_nodes[[1]] %in%
    npsem_nodes[[4]]]

  # check duplication of variables across NPSEM nodes
  nodes_duped <- duplicated(unname(do.call(c, npsem_nodes[-4])))
  if (any(nodes_duped)) {
    names_nodes_duped <- unname(do.call(c, npsem_nodes))[nodes_duped]
    stop("Variable ", names_nodes_duped, " appears in multiple NPSEM nodes.")
  }

  # check that segmentation covariates are a strict subset of baseline
  assertthat::assert_that(
    all(npsem_nodes[[4]] %in% npsem_nodes[[1]]),
    msg = "Segmentation covariates not a subset of baseline covariates."
  )

  # set all columns to numeric
  for (j in colnames(data_est)) {
    data.table::set(data_est, j = j, value = as.numeric(data_est[[j]]))
  }

  # guess whether input data has binary or continous-valued outcome
  outcome_levels <- data_est[, length(unique(Y))]
  outcome_type <- ifelse(outcome_levels == 2, "binary", "continuous")

  # output, data.table modified with attributes
  data.table::setattr(data_est, "outcome_type", outcome_type)
  data.table::setattr(data_est, "segment_by", segment_names_abstract)
  data.table::setattr(data_est, "npsem", npsem_nodes)
  data.table::setattr(data_est, "treatment_cost", treatment_cost)
  return(data_est[])
}
