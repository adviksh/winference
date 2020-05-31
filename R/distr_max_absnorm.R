#' The Maximum Absolute Normal Distribution
#' @name max_absnorm
#'
#' @description distribution function, quantile function and
#' random generation for the maximum of [size] absolute value normal random
#' variables with mean equal to [mean] and standard deviation equal to [sd].
#' @inheritParams stats::rnorm
#' @param size number of absolute normal variables maximized over per
#' observation
#'
NULL

#' @rdname max_absnorm
r_max_absnorm <- function(n, size, mean = 0, sd = 1) {
  replicate(n, max(r_absnorm(size, mean, sd)))
}

#' @rdname max_absnorm
p_max_absnorm <- function(size, q, mean = 0, sd = 1, lower.tail = TRUE,
                          log.p = FALSE) {

  log_p1 <- p_absnorm(q = q, mean = mean, sd = sd, lower.tail = lower.tail,
                      log.p = TRUE)

  if (log.p) return(size * log_p1)

  p1 <- exp(log_p1)
  return(p1^size)
}

#' @rdname max_absnorm
q_max_absnorm <- function(size, p, mean = 0, sd = 1) {
  p_tilde <- p^(1/size)
  q_absnorm(p_tilde, mean = mean, sd = sd)
}
