#' Confidence Interval Using Asymptotic Normality
#' @export
#' @inheritParams ci_docs
#'
ci_standard <- function(estimates, st_errs, conf_level = 0.95) {

  # Defense -----------------------------------------------------------------
  check_estimates(estimates)
  check_st_errs(st_errs   = st_errs,
                estimates = estimates)
  check_conf_level(conf_level)

  # Function Body -----------------------------------------------------------
  half_alpha <- (1 - conf_level) / 2
  tail_tiles <- c(half_alpha, 1 - half_alpha)

  ordering <- order(estimates, decreasing = TRUE)

  estimates_sorted <- estimates[ordering]
  st_errs_sorted   <- st_errs[ordering]

  estimates_sorted[1] + st_errs_sorted[1] * stats::qnorm(tail_tiles)
}
