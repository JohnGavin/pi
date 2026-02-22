#' coinpi: Chunked, Vectorized Pi Estimation from Coin Flips
#'
#' Implements the stopping-time simulation described in Jim Propp's 2026
#' note: for each trial, flip a fair coin until heads first outnumber tails,
#' record `H_tau / tau`, and use `4 * mean(H_tau / tau)` to estimate `pi`.
#'
#' @keywords internal
"_PACKAGE"
