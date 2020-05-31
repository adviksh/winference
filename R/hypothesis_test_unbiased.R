unbiased_test_rejects <- function(mu, y, sigma, alpha = 0.05,
                                  trunc_lo = -Inf, trunc_hi = Inf) {

  # Immediately reject if observation falls outside the truncation set
  if (is_contained(y, c(trunc_lo, trunc_hi)) == FALSE) return(TRUE)

  y_interval <- unbiased_interval(mu       = mu,
                                  sigma    = sigma,
                                  alpha    = alpha,
                                  trunc_lo = trunc_lo,
                                  trunc_hi = trunc_hi)

  # Reject if y is outside the interval
  is_contained(y, y_interval) == FALSE

}


unbiased_interval <- function(mu, sigma, alpha = 0.05, trunc_lo = -Inf,
                              trunc_hi = Inf) {

  c_lo <- solve_c_lo(mu       = mu,
                     sigma    = sigma,
                     trunc_lo = trunc_lo,
                     trunc_hi = trunc_hi,
                     alpha    = alpha)

  c_hi <- solve_c_hi(c_lo     = c_lo,
                     mu       = mu,
                     sigma    = sigma,
                     trunc_lo = trunc_lo,
                     trunc_hi = trunc_hi,
                     alpha    = alpha)

  c(c_lo, c_hi)

}

solve_c_lo <- function(mu, sigma, alpha = 0.05,
                       trunc_lo = -Inf, trunc_hi = Inf) {

  # Optimization Bounds -----------------------------------------------------
  c_lo_init <- q_truncnorm(p    = alpha / 2,
                           mean = mu,
                           sd   = sigma,
                           trunc_lo = trunc_lo,
                           trunc_hi = trunc_hi)

  c_lo_min <- trunc_lo

  c_lo_max <- q_truncnorm(p    = alpha,
                          mean = mu,
                          sd   = sigma,
                          trunc_lo = trunc_lo,
                          trunc_hi = trunc_hi)

  # Optimization ------------------------------------------------------------
  opt_result <- stats::optim(par   = c_lo_init,
                             fn    = solve_c_lo_loss,
                             mu    = mu,
                             sigma = sigma,
                             alpha = alpha,
                             trunc_lo = trunc_lo,
                             trunc_hi = trunc_hi,
                             method = "Brent",
                             lower  = c_lo_min,
                             upper  = c_lo_max)


  # Return ------------------------------------------------------------------
  opt_result$par

}

solve_c_lo_loss <- function(c_lo, mu, sigma, alpha = 0.05,
                            trunc_lo = -Inf, trunc_hi = Inf) {

  # Get interval length
  c_hi <- solve_c_hi(c_lo     = c_lo,
                     mu       = mu,
                     sigma    = sigma,
                     trunc_lo = trunc_lo,
                     trunc_hi = trunc_hi,
                     alpha    = alpha)
  c_z  <- c(c_lo, c_hi)

  # E[zeta * indicator]
  e_zi <- e_truncnorm_times_indicator(mu    = mu,
                                      sigma = sigma,
                                      c_z   = c_z,
                                      trunc_lo = trunc_lo,
                                      trunc_hi = trunc_hi)

  e_z <- e_truncnorm(mean     = mu,
                     sd       = sigma,
                     trunc_lo = trunc_lo,
                     trunc_hi = trunc_hi)

  # Return
  abs(e_zi - (1 - alpha) * e_z)
}

solve_c_hi <- function(c_lo, mu, sigma, trunc_lo, trunc_hi, alpha) {

  p_c_lo <- p_truncnorm(q        = c_lo,
                        mean     = mu,
                        sd       = sigma,
                        trunc_lo = trunc_lo,
                        trunc_hi = trunc_hi)
  p_c_hi <- p_c_lo + (1 - alpha)

  if (is.nan(p_c_hi)) return(NaN)

  if (p_c_hi > 1) browser()

  # Return
  q_truncnorm(p        = p_c_hi,
              mean     = mu,
              sd       = sigma,
              trunc_lo = trunc_lo,
              trunc_hi = trunc_hi)
}
