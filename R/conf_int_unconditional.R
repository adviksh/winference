#' Unconditional Confidence Interval Using Order Statistics
#' @export
#' @inheritParams ci_docs
#'
ci_unconditional <- function(estimates, st_errs, conf_level = 0.95) {

  # Defense -----------------------------------------------------------------
  check_estimates(estimates)
  check_st_errs(st_errs   = st_errs,
                estimates = estimates)
  check_conf_level(conf_level)

  # Function Body -----------------------------------------------------------
  ordering <- order(estimates, decreasing = TRUE)

  c_alpha <- q_max_absnorm(p    = conf_level,
                           size = length(estimates))
  tail_tiles <- c(-c_alpha, c_alpha)

  estimates_sorted <- estimates[ordering]
  st_errs_sorted   <- st_errs[ordering]

  estimates_sorted[1] + tail_tiles * st_errs_sorted[1]
}
