## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(coinpi)

## -----------------------------------------------------------------------------
fit <- simulate_coin_pi(
  n_sims = 2000L,
  chunk_size = 500L,
  seed = 123,
  progress = FALSE
)
fit

## ----eval = FALSE-------------------------------------------------------------
# # One million trial states per chunk
# fit_big <- simulate_coin_pi(
#   n_sims = 5e6L,
#   chunk_size = 1e6L,
#   seed = 2026,
#   progress = TRUE
# )
# fit_big$pi_hat

## -----------------------------------------------------------------------------
fit_ratios <- simulate_coin_pi(
  n_sims = 2000L,
  chunk_size = 500L,
  seed = 99,
  progress = FALSE,
  return_ratios = TRUE
)

summary(fit_ratios$ratios)

