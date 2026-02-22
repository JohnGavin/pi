test_that("simulate_coin_pi returns expected structure", {
  fit <- simulate_coin_pi(
    n_sims = 200L,
    chunk_size = 50L,
    seed = 123,
    progress = FALSE
  )

  expect_s3_class(fit, "coin_pi_estimate")
  expect_named(
    fit,
    c("pi_hat", "mean_ratio", "std_error", "n_sims", "chunk_size",
      "n_chunks", "ratios", "seed")
  )
  expect_equal(fit$n_sims, 200L)
  expect_equal(fit$chunk_size, 50L)
  expect_equal(fit$n_chunks, 4L)
  expect_null(fit$ratios)
})

test_that("simulate_coin_pi is reproducible with seed", {
  fit1 <- simulate_coin_pi(
    n_sims = 300L,
    chunk_size = 60L,
    seed = 42,
    progress = FALSE
  )
  fit2 <- simulate_coin_pi(
    n_sims = 300L,
    chunk_size = 60L,
    seed = 42,
    progress = FALSE
  )

  expect_equal(fit1$pi_hat, fit2$pi_hat)
  expect_equal(fit1$mean_ratio, fit2$mean_ratio)
  expect_equal(fit1$std_error, fit2$std_error)
})

test_that("estimate is reasonably close to pi", {
  fit <- simulate_coin_pi(
    n_sims = 1000L,
    chunk_size = 200L,
    seed = 2026,
    progress = FALSE
  )

  expect_lt(abs(fit$pi_hat - base::pi), 0.6)
})

test_that("return_ratios stores all simulated ratios", {
  fit <- simulate_coin_pi(
    n_sims = 120L,
    chunk_size = 30L,
    seed = 11,
    progress = FALSE,
    return_ratios = TRUE
  )

  expect_length(fit$ratios, 120L)
  expect_true(all(fit$ratios > 0.5))
  expect_true(all(fit$ratios <= 1))
})

test_that("input validation catches bad arguments", {
  expect_snapshot_error(simulate_coin_pi(n_sims = 0L, progress = FALSE))
  expect_snapshot_error(simulate_coin_pi(chunk_size = 0L, progress = FALSE))
  expect_snapshot_error(simulate_coin_pi(seed = "bad", progress = FALSE))
  expect_snapshot_error(simulate_coin_pi(progress = NA))
  expect_snapshot_error(simulate_coin_pi(return_ratios = NA))
})
