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

# checks that CATE estimation output contains valid nuisance estimates
test_that("Propensity score estimates for treatment arms are compatible", {
  # propensity score estimates sum to 1
  expect_equal(unique(cate_results[, g0 + g1]), 1)

  # propensity score estimates are below 1 and above 0
  expect_true(all(cate_results[, g0] <= 1))
  expect_true(all(cate_results[, g0] >= 0))
  expect_true(all(cate_results[, g1] <= 1))
  expect_true(all(cate_results[, g1] >= 0))
})

# checks that CATE estimation output contains valid nuisance estimates
test_that("Outcome mechanism estimates for treatment arms are compatible", {
  # counterfactual outcome predictions are outcome predictions within the
  # appropriate treatment arms
  expect_equal(cate_results[, (A * Q1) + (1 - A) * Q0], cate_results[, QA])
})

# checks about properties of treatment rule assignments
test_that("Hard-thresholding CATE only assigns treatment for CATE > cutoff", {
  # 1) hard-thresholding of the CATE at zero
  assign_rule(
    cate_results,
    segment_fun = cost_threshold,
    threshold = 0,
    type = "analytic"
  )
  expect_true(all(cate_results[rule == 1, cate] > 0))
  expect_equal(attr(cate_results, "threshold"), 0)
})

test_that("Adaptive-thresholding of CATE respects treatment budget", {
  # 2) adaptive-thresholding of the CATE
  assign_rule(
    cate_results,
    segment_fun = cost_budget,
    budget = 0.2,
    type = "analytic"
  )
  expect_lte(cate_results[, sum(rule == 1) / .N] , 0.2)
  expect_true(all(cate_results[rule == 1, cate] > 0))
})

test_that("Knapsack solution of CATE respects treatment budget closely", {
  # 3) adaptive-thresholding of the CATE, solving the binary knapsack problem
  assign_rule(
    cate_results,
    segment_fun = cost_knapsack,
    budget = 0.5,
    type = "analytic"
  )
  expect_lte(cate_results[, sum(rule == 1) / .N] , 0.5)
  expect_true(all(cate_results[rule == 1, cate] > 0))
})
