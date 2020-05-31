hybrid_equal_tailed_test_rejects <- function(mu, y, sigma, n_estimates,
                                             alpha = 0.05, beta = alpha / 10,
                                             trunc_lo = -Inf, trunc_hi = Inf) {

  # Refine the truncation set
  c_b <- q_max_absnorm(p    = 1 - beta,
                       size = n_estimates)

  trunc_set_proj <- c(mu - c_b * sigma,
                      mu + c_b * sigma)

  trunc_set_adj <- intersect_intervals(trunc_set_proj,
                                       c(trunc_lo, trunc_hi))

  alpha_adj      <- (alpha - beta) / (1 - beta)

  # Immediately reject if observation falls outside the truncation set
  if (is_contained(y, trunc_set_adj) == FALSE) return(TRUE)

  left_test <- one_sided_test_rejects(mu       = mu,
                                      sigma    = sigma,
                                      y        = y,
                                      side     = "left",
                                      trunc_lo = min(trunc_set_adj),
                                      trunc_hi = max(trunc_set_adj),
                                      alpha    = alpha_adj / 2)

  right_test <- one_sided_test_rejects(mu       = mu,
                                       sigma    = sigma,
                                       y        = y,
                                       side     = "right",
                                       trunc_lo = min(trunc_set_adj),
                                       trunc_hi = max(trunc_set_adj),
                                       alpha    = alpha_adj / 2)

  left_test | right_test
}
