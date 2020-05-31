#' Equal-Tailed Hybrid Confidence Interval
#' @export
#' @inheritParams ci_docs
#'
ci_hybrid_equal_tailed <- function(estimates, st_errs, conf_level = 0.95,
                                   beta = (1 - conf_level) / 10) {

  # Defense -----------------------------------------------------------------
  check_estimates(estimates = estimates)
  check_st_errs(st_errs = st_errs, estimates = estimates)
  check_conf_level(conf_level = conf_level)
  check_beta(beta = beta, conf_level = conf_level)


  # Fast Returns ------------------------------------------------------------
  if (beta == 0) {
    ci <- ci_conditional_unbiased(estimates  = estimates,
                                  st_errs    = st_errs,
                                  conf_level = conf_level)
    return(ci)
  }


  # Function Body -----------------------------------------------------------
  # see discussion before prop 6 in Andrews, Kitagawa, McCloskey (2019)
  alpha    <- 1 - conf_level

  ordering <- order(estimates, decreasing = TRUE)

  estimates_sorted <- estimates[ordering]
  st_errs_sorted   <- st_errs[ordering]

  # Unconditional CI ---------------------------------------------------------
  conf_int_unb <- ci_unconditional(estimates  = estimates,
                                   st_errs    = st_errs,
                                   conf_level = 1 - beta)

  hybrid_test <- purrr::partial(hybrid_equal_tailed_test_rejects,
                                y           = estimates_sorted[1],
                                sigma       = st_errs_sorted[1],
                                n_estimates = length(estimates),
                                alpha       = alpha,
                                beta        = beta,
                                trunc_lo    = conditional_trunc_lo(estimates),
                                trunc_hi    = Inf)

  reject_bounds <- purrr::map_lgl(conf_int_unb, hybrid_test)

  # If any of the hypothesis tests failed due to computational limitations
  # Return NA
  if (any(is.na(reject_bounds))) return(NA)

  # If both bounds pass the test, return the interval
  if (sum(reject_bounds) == 0) return(conf_int_unb)

  # If only one bound passes the test, find the jump
  if (sum(reject_bounds) == 1) {

    jump <- find_jump(hybrid_test, .bounds = conf_int_unb)

    ci   <- c(jump, conf_int_unb[reject_bounds == FALSE])

    return(sort(ci))

  }

  # If neither bound passes the test, find an interior value that does pass
  # then find the jump on either side of it

  midpoint_rejected <- TRUE
  while (midpoint_rejected) {

    midpoint <- stats::runif(1, min = conf_int_unb[1], max = conf_int_unb[2])

    midpoint_rejected <- hybrid_test(midpoint)
  }

  jump_lo <- find_jump(hybrid_test, .bounds = c(conf_int_unb[1], midpoint))
  jump_hi <- find_jump(hybrid_test, .bounds = c(midpoint, conf_int_unb[2]))


  # Return ------------------------------------------------------------------
  c(jump_lo, jump_hi)

}
