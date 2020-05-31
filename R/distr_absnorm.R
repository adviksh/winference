#' The Absolute Normal Distribution
#' @name absnorm
#'
#' @description distribution function, quantile function and
#' random generation for the absolute value normal distribution
#' with mean equal to [mean] and standard deviation equal to [sd].
#' @inheritParams stats::rnorm
#'
NULL

#' @rdname absnorm
r_absnorm <- function(n, mean = 0, sd = 1) { abs(stats::rnorm(n, mean, sd)) }

#' @rdname absnorm
p_absnorm <- function(q, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE) {
  p_lo <- stats::pnorm(q = -q, mean = mean, sd = sd, lower.tail = lower.tail)
  p_hi <- stats::pnorm(q = q, mean = mean, sd = sd, lower.tail = lower.tail)

  p <- pmax(p_hi - p_lo, 0)

  if (log.p) return(log(p))

  return(p)
}

#' @rdname absnorm
q_absnorm <- function(p, mean = 0, sd = 1) {
  stats::qnorm(p = 0.5 * (1 + p), mean = mean, sd = sd)
}
