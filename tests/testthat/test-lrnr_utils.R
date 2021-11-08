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

###############################################################################
# NOTE: testing Lrnr_prob_known.R
###############################################################################

# set treatment probability based on observed mean
ps_prob <- data_example[, mean(treatment)]

# compute the CATE estimate
cate_results <- est_cate(
  data_est_spec = est_spec,
  cv_folds = 5L,
  ps_learner = Lrnr_prob_known$new(ps_prob),
  or_learner = Lrnr_glm_fast$new(),
  cate_learner = Lrnr_glm_fast$new()
)

# check that known probability is used for propensity score calculation
test_that("Known empirical probability used for propensity score estimates", {
  # propensity score estimates sum to 1
  expect_equal(unique(cate_results[, g0 + g1]), 1)

  # propensity score estimates match known probability
  expect_true(all(cate_results[, g0] == 1 - ps_prob))
  expect_true(all(cate_results[, g1] == ps_prob))
})


###############################################################################
# TODO: testing convert_learners.R
###############################################################################

