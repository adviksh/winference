#' Conditional Confidence Interval Using Truncated Normality
#' @export
#' @inheritParams ci_docs
#'
ci_conditional_equal_tailed <- function(estimates, st_errs, conf_level = 0.95) {

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

  ci <- purrr::map_dbl(tail_tiles,
                       mu_alpha_conditional,
                       y        = estimates_sorted[1],
                       se       = st_errs_sorted[1],
                       trunc_lo = conditional_trunc_lo(estimates_sorted))

  if (is.na(ci[1])) ci[1] <- -Inf
  if (is.na(ci[2])) ci[2] <- Inf

  ci
}

mu_alpha_conditional <- function(alpha, y, se, trunc_lo = -Inf, trunc_hi = Inf) {

  ur_result <- tryCatch(
    stats::uniroot(f         = subtract(TruncatedNormal::ptmvnorm,
                                        .minus = 1 - alpha),
                   interval  = c(y +  10 * se, y - 10 * se),
                   q         = y,
                   sigma     = se,
                   lb        = trunc_lo,
                   ub        = trunc_hi,
                   extendInt = "downX"),
    error = function(e) { list(root = NA_real_) })

  ur_result$root
}
