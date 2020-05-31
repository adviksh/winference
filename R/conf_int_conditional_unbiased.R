#' Conditional Confidence Interval Using Truncated Normality
#' @inheritParams ci_docs
#'
ci_conditional_unbiased <- function(estimates, st_errs, k = 1,
                                    conf_level = 0.95) {

  rlang::abort("still in development")

  # Defense -----------------------------------------------------------------
  check_estimates(estimates)
  check_st_errs(st_errs   = st_errs,
                estimates = estimates)
  check_conf_level(conf_level)

  # Function Body -----------------------------------------------------------
  alpha <- 1 - conf_level

  half_alpha <- alpha / 2
  tail_tiles <- c(half_alpha, 1 - half_alpha)

  ordering <- order(estimates, decreasing = TRUE)

  estimates_sorted <- estimates[ordering]
  st_errs_sorted   <- st_errs[ordering]

  # Need: two extreme values of mu that are rejected
  # and one central value that isn't

  # start with mu init
  # get the unbiased confidence interval
  mu_list <- list(reject_lo = NULL,
                  ftr       = NULL,
                  reject_hi = NULL)

  mu_init <- q_truncnorm(p = 0.5,
                         mean     = estimates_sorted[1],
                         sd       = st_errs_sorted[1],
                         trunc_lo = estimates_sorted[2])

  interval_init <- unbiased_interval(mu       = estimates_sorted[1],
                                     sigma    = st_errs_sorted[1],
                                     alpha    = alpha,
                                     trunc_lo = estimates_sorted[2])
  if (estimates_sorted[1] >= min(interval_init) &
      estimates_sorted[1] <= max(interval_init)) {

    mu_list$ftr <- mu_init

    mu_list$reject_lo <- tryCatch(stats::uniroot(subtract(unbiased_test_rejects, 1),
                                                 y        = estimates_sorted[1],
                                                 sigma    = st_errs_sorted[1],
                                                 trunc_lo = estimates_sorted[2],
                                                 alpha    = alpha,
                                                 lower = mu_list$ftr - 1,
                                                 upper = mu_list$ftr,
                                                 extendInt = "downX")$root,
                                  error = function(e) NA)

    mu_list$reject_hi <- tryCatch(stats::uniroot(subtract(unbiased_test_rejects, 1),
                                                 y        = estimates_sorted[1],
                                                 sigma    = st_errs_sorted[1],
                                                 trunc_lo = estimates_sorted[2],
                                                 alpha    = alpha,
                                                 lower = mu_list$ftr,
                                                 upper = mu_list$ftr + 1,
                                                 extendInt = "upX")$root,
                                  error = function(e) NA)

  } else {
    browser()

    # if y is under the interval search right
    # if y is over the interval search left
  }

  # Once we've established the endpoints, find a jump.
  ci_lo <- tryCatch(find_jump(.f = unbiased_test_rejects,
                              .bounds  = c(mu_list$reject_lo,
                                           mu_list$ftr),
                              y        = estimates_sorted[1],
                              sigma    = st_errs_sorted[1],
                              trunc_lo = estimates_sorted[2],
                              alpha    = alpha),
                    error = function(e) -Inf)

  ci_hi <- tryCatch(find_jump(.f = unbiased_test_rejects,
                              .bounds  = c(mu_list$reject_lo,
                                           mu_list$ftr),
                              y        = estimates_sorted[1],
                              sigma    = st_errs_sorted[1],
                              trunc_lo = estimates_sorted[2],
                              alpha    = alpha),
                    error = function(e) Inf)

  c(ci_lo, ci_hi)

}
