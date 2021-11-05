# load package data
library(data.table)
library(sl3)
data(data_example)
baseline_names <- c("num_devices", "is_p2plus", "is_newmarket", "baseline_ltv",
                    "baseline_viewing")
segmentation_names <- c("num_devices", "is_p2plus")

# prepare data for estimation procedure
est_spec <- set_est_data(
  data_obs = data_example,
  baseline = baseline_names,
  exposure = "treatment",
  outcome = "outcome_viewing",
  segment_by = segmentation_names
)

# compute the CATE estimate
cate_results <- est_cate(
  data_est_spec = est_spec,
  cv_folds = 5L,
  ps_learner = Lrnr_glm_fast$new(),
  or_learner = Lrnr_glm_fast$new(),
  cate_learner = Lrnr_glm_fast$new()
)

# adaptive-thresholding of the CATE, solving the binary knapsack problem
assign_rule(
  cate_results,
  segment_fun = cost_knapsack,
  budget = 0.5,
  type = "analytic"
)

# effect estimates for the HTE/OTE under the assigned rule
hte_est_os <- est_effect(
  cate_results,
  param_type = "hte",
  est_type = "onestep"
)
hte_est_os

ote_est_os <- est_effect(
  cate_results,
  param_type = "ote",
  est_type = "onestep"
)
ote_est_os
