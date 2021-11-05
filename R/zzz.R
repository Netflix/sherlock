.onAttach <- function(...) {
  packageStartupMessage(paste0(
    "sherlock v", utils::packageDescription("sherlock")$Version,
    ": ", utils::packageDescription("sherlock")$Title
  ))

  # instantiate defaults
  set_sherlock_options(option = "pval_crit", val = 0.05)
  set_sherlock_options(option = "ci_covers", val = 0.95)
  options(sl3.pcontinuous = 0)
}
