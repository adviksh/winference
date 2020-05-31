equal_tailed_test_rejects <- function(mu, y, sigma, alpha = 0.05,
                                      trunc_lo = -Inf, trunc_hi = Inf) {

  # Immediately reject if observation falls outside the truncation set
  if (is_contained(y, c(trunc_lo, trunc_hi)) == FALSE) return(TRUE)

  left_test <- one_sided_test_rejects(mu       = mu,
                                      sigma    = sigma,
                                      y        = y,
                                      side     = "left",
                                      trunc_lo = trunc_lo,
                                      trunc_hi = trunc_hi,
                                      alpha    = alpha / 2)

  right_test <- one_sided_test_rejects(mu       = mu,
                                       sigma    = sigma,
                                       y        = y,
                                       side     = "right",
                                       trunc_lo = trunc_lo,
                                       trunc_hi = trunc_hi,
                                       alpha    = alpha / 2)

  left_test | right_test
}
