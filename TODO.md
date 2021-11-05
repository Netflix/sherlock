# TO-DO

## 16 Aug 2021

- [x] fix rounding bug in `cost_knapsack` for better integer approximations

## 12 Aug 2021

- add documentation in all `R` source files for estimation functionality:
  - [x] `cate.R`
  - [ ] `data.R`
  - [x] `effects.R`
  - [x] `fluctuation.R`
  - [x] `nuisance.R`
  - [x] `costs.R`
  - [x] `segment.R`
  - [x] `confint.R`
  - [x] `setup_data.R`
  - [x] `wrappers.R`
  - [x] `Lrnr_prob_known.R`
- add documentation where necessary in all `R` auxiliary source files:
  - [x] `bound.R`
  - [x] `scale.R`
  - [x] `globals.R`
  - [x] `options.R`
  - [x] `zzz.R`

## 11 Aug 2021

- [x] fix TMLE fluctuation to fit each parameter separately
- [x] add confidence intervals for CATE and effect measure estimates
- [ ] add in checks for diagnostics at smaller sample sizes

## 07 Aug 2021

- [x] reduce to a few high-level functions: `sherlock` to detect segments,
           `watson` to assign an optimal rule, and `mycroft` to compute effects

## 06 Aug 2021

- [x] rename HTE to OTE, since HTE already defined otherwise in ABlaze
- [ ] evaluate OTE conditional on segmentation covariates, ie, COTE = E(OTE|V)

## 04 Aug 2021

-[x] create a wrapper function to bundle `set_est_data` and `est_cate`
-[x] add summarization of EIF across IDs for repeated measures
-[x] add functionality to handle known g in PS estimation
-[x] export global option for `sl3.pcontinuous` to avoid guessing categorical
