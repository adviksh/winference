#' Confidence Interval for a Proportion
#'
#' @inheritParams ci_docs
#' @param y integer | vector \cr
#' Either the number of successes, or a vector of successes and failures
#' @param n integer \cr
#' If y is the number of successes, the number of trials.
#'
ci_proportion <- function(y, n = length(y), conf_level = 0.95) {

  if (length(n) > 1) rlang::abort("n must be a single integer")
  if (length(y) > 1) y <- sum(y)

  bt <- stats::binom.test(x = y, n = n, conf.level = conf_level)

  as.numeric(bt$conf.int)
}
