#' Estimate Pi by Stopping-Time Coin Flips
#'
#' Simulate the coin-flip process from Propp (2026) in chunks, using a
#' vectorized inner loop over active trajectories in each chunk.
#'
#' For each trial, repeatedly flip a fair coin until heads first outnumber
#' tails. Record `H_tau / tau` for that trial. Across many trials,
#' `4 * mean(H_tau / tau)` estimates `pi`.
#'
#' @param n_sims Number of independent trials to run.
#' @param chunk_size Number of trials to simulate per vectorized chunk.
#' @param seed Optional integer seed for reproducibility.
#' @param progress Logical; if `TRUE`, print chunk progress.
#' @param return_ratios Logical; if `TRUE`, include all simulated
#'   `H_tau / tau` ratios in the output.
#'
#' @return A `coin_pi_estimate` object (list) with fields:
#' `pi_hat`, `mean_ratio`, `std_error`, `n_sims`, `chunk_size`, `n_chunks`,
#' and optionally `ratios`.
#'
#' @examples
#' fit <- simulate_coin_pi(n_sims = 500L, chunk_size = 100L, seed = 123)
#' fit
#'
#' @export
simulate_coin_pi <- function(
    n_sims = 1e6L,
    chunk_size = 1e6L,
    seed = NULL,
    progress = interactive(),
    return_ratios = FALSE
) {
  n_sims <- validate_count(n_sims, "n_sims")
  chunk_size <- validate_count(chunk_size, "chunk_size")

  if (!is.null(seed)) {
    seed <- validate_seed(seed)
    set.seed(seed)
  }

  validate_flag(progress, "progress")
  validate_flag(return_ratios, "return_ratios")

  if (chunk_size > n_sims) {
    chunk_size <- n_sims
  }

  n_chunks <- as.integer(ceiling(n_sims / chunk_size))
  n_done <- 0L

  ratio_sum <- 0
  ratio_sq_sum <- 0
  ratios <- if (isTRUE(return_ratios)) numeric(n_sims) else NULL
  ratio_pos <- 1L

  for (chunk_id in seq_len(n_chunks)) {
    n_current <- as.integer(min(chunk_size, n_sims - n_done))
    chunk_ratios <- simulate_chunk_ratios(n_current)

    ratio_sum <- ratio_sum + sum(chunk_ratios)
    ratio_sq_sum <- ratio_sq_sum + sum(chunk_ratios * chunk_ratios)

    if (isTRUE(return_ratios)) {
      end_pos <- ratio_pos + n_current - 1L
      ratios[ratio_pos:end_pos] <- chunk_ratios
      ratio_pos <- end_pos + 1L
    }

    n_done <- n_done + n_current

    if (isTRUE(progress)) {
      cli::cli_alert_info(
        "chunk {chunk_id}/{n_chunks}: processed {format(n_done, big.mark = ',')}"
      )
    }
  }

  mean_ratio <- ratio_sum / n_sims
  var_ratio <- max(ratio_sq_sum / n_sims - mean_ratio^2, 0)

  out <- list(
    pi_hat = 4 * mean_ratio,
    mean_ratio = mean_ratio,
    std_error = 4 * sqrt(var_ratio / n_sims),
    n_sims = n_sims,
    chunk_size = chunk_size,
    n_chunks = n_chunks,
    ratios = ratios,
    seed = seed
  )

  class(out) <- "coin_pi_estimate"
  out
}

#' Print a `coin_pi_estimate`
#'
#' @param x A `coin_pi_estimate` object from [simulate_coin_pi()].
#' @param ... Unused.
#'
#' @return The input object, invisibly.
#' @export
print.coin_pi_estimate <- function(x, ...) {
  cat("<coin_pi_estimate>\n")
  cat("  pi_hat:     ", format(x$pi_hat, digits = 8), "\n", sep = "")
  cat("  std_error:  ", format(x$std_error, digits = 5), "\n", sep = "")
  cat("  n_sims:     ", format(x$n_sims, big.mark = ","), "\n", sep = "")
  cat("  chunk_size: ", format(x$chunk_size, big.mark = ","), "\n", sep = "")
  cat("  n_chunks:   ", x$n_chunks, "\n", sep = "")

  if (!is.null(x$seed)) {
    cat("  seed:       ", x$seed, "\n", sep = "")
  }

  if (!is.null(x$ratios)) {
    cat("  ratios:     stored (length ", length(x$ratios), ")\n", sep = "")
  }

  invisible(x)
}

simulate_chunk_ratios <- function(n) {
  heads <- numeric(n)
  tosses <- numeric(n)
  balance <- numeric(n)
  active <- seq_len(n)

  while (length(active) > 0L) {
    flips <- stats::rbinom(n = length(active), size = 1L, prob = 0.5)

    heads[active] <- heads[active] + flips
    tosses[active] <- tosses[active] + 1
    balance[active] <- balance[active] + (2 * flips - 1)

    active <- active[balance[active] <= 0]
  }

  heads / tosses
}

validate_count <- function(x, arg_name) {
  if (!rlang::is_integerish(x, n = 1, finite = TRUE)) {
    cli::cli_abort(c(
      "x" = "{.arg {arg_name}} must be a single finite integer-like value.",
      "i" = "You provided class {.cls {class(x)}}."
    ))
  }

  x_int <- as.integer(x)

  if (is.na(x_int) || x_int < 1L) {
    cli::cli_abort(c(
      "x" = "{.arg {arg_name}} must be at least 1.",
      "i" = "You provided {.val {x}}."
    ))
  }

  x_int
}

validate_seed <- function(seed) {
  if (!rlang::is_integerish(seed, n = 1, finite = TRUE)) {
    cli::cli_abort(c(
      "x" = "{.arg seed} must be a single finite integer-like value.",
      "i" = "Use {.code seed = 123} for reproducible simulations."
    ))
  }

  as.integer(seed)
}

validate_flag <- function(x, arg_name) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    cli::cli_abort(c(
      "x" = "{.arg {arg_name}} must be either TRUE or FALSE.",
      "i" = "You provided {.val {x}}."
    ))
  }

  invisible(NULL)
}
