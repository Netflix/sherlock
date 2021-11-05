# load package data
library(data.table)
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

test_that("Check that data estimation spec has columns matching input data", {
  # baseline covariate columns match
  baseline_idxs <- which(colnames(data_example) %in% baseline_names)
  lapply(baseline_idxs, function(idx) {
    expect_equal(
      unname(unlist(data_example[, .SD, .SDcols = idx])),
      unname(unlist(est_spec[, .SD, .SDcols = idx]))
    )
  })

  # treatment columns match
  expect_equal(data_example[, treatment], est_spec[, A])

  # outcome columns match
  expect_equal(data_example[, outcome_viewing], est_spec[, Y])

  # segmentation covariate columns match
  segmentation_idxs <- which(colnames(data_example) %in% segmentation_names)
  lapply(segmentation_idxs, function(idx) {
    expect_equal(
      unname(unlist(data_example[, .SD, .SDcols = idx])),
      unname(unlist(est_spec[, .SD, .SDcols = idx]))
    )
  })

})

test_that("Check custom attributes of the generated data estimation spec", {
  # check segmentation covariate generic names
  segmentation_idxs <- which(colnames(data_example) %in% segmentation_names)
  expect_equal(attr(est_spec, "segment_by"),
               colnames(est_spec)[segmentation_idxs])

  # check outcome type (NOTE: hardcoded based on knowledge of dataset)
  outcome_levels <- data_example[, length(unique(outcome_viewing))]
  if (outcome_levels == 2) {
    expect_equal(attr(est_spec, "outcome_type"), "binary")
  } else {
    expect_equal(attr(est_spec, "outcome_type"), "continuous")
  }

  # check that nodes of NPSEM match variables names from original dataset
  npsem <- attr(est_spec, "npsem")
  expect_equal(npsem$W, baseline_names)
  expect_equal(npsem$V, segmentation_names)
  expect_equal(npsem$A, "treatment")
  expect_equal(npsem$Y, "outcome_viewing")

  # NOTE: no cost associatd with treatment so this slot should be NULL
  expect_equal(attr(est_spec, "treatment_cost"), NULL)
})
