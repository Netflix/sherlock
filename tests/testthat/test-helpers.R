# load package data
library(data.table)
data(data_example)

# scale.R: scaling to and from the unit interval
test_that("Minima and maxima lie in the unit interval after scaling", {
  outcome_scaled <- data_example[, scale_to_unit(outcome_viewing)]
  expect_gte(min(outcome_scaled), 0)
  expect_lte(max(outcome_scaled), 1)
})

test_that("Unscaling reverts values in the unit interval to original scale", {
  outcome_scaled <- data_example[, scale_to_unit(outcome_viewing)]
  outcome_unscaled <- data_example[,
    scale_from_unit(outcome_scaled,
                    max(outcome_viewing),
                    min(outcome_viewing))
  ]
  expect_equal(min(outcome_unscaled), data_example[, min(outcome_viewing)])
  expect_equal(max(outcome_unscaled), data_example[, max(outcome_viewing)])
})

# bound.R: bounding to set tolerance and for propensity score limits
test_that("Bounding to tolerance enforces specified lower/upper limits", {
  outcome_scaled <- data_example[, scale_to_unit(outcome_viewing)]
  outcome_scaled_bounded <- bound_tolerance(outcome_scaled, tol = 1e-2)
  expect_equal(min(outcome_scaled_bounded), 1e-2)
  expect_equal(max(outcome_scaled_bounded), 1 - 1e-2)
})

test_that("Bounding propensity scores enforces specified lower/upper limits", {
  # simple propensity score estimator
  ps_simple_mod <- glm(
    formula = treatment ~ .^2,
    data = data_example[, .SD, .SDcols = !("outcome_viewing")],
    family = "binomial"
  )
  ps_simple_preds <- unname(predict(ps_simple_mod, type = "response"))
  ps_simple_preds_bounded <- bound_propensity(
    ps_simple_preds,
    bounds = c(0.15, 0.95)
  )
  expect_equal(min(ps_simple_preds_bounded), 0.15)
  expect_equal(max(ps_simple_preds_bounded), 0.95)
})
