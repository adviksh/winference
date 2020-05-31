one_sided_test_rejects <- function(mu, side, y, sigma, alpha = 0.025,
                                   trunc_lo = -Inf, trunc_hi = Inf) {

  # Immediately reject if observation falls outside the truncation set
  if (is_contained(y, c(trunc_lo, trunc_hi)) == FALSE) return(TRUE)

  left_tail <- p_truncnorm(mean = mu,
                           q    = y,
                           sd   = sigma,
                           trunc_lo = trunc_lo,
                           trunc_hi = trunc_hi)
  right_tail <- 1 - left_tail

  # TODO: how to handle cases where ptmvnorm can't handle the integration?
  # Usually happens when mu is far from the y and the permissible set
  # So that we're integrating far in the tail
  # For now, return NA
  switch(side,
         left  = left_tail  <= alpha,
         right = right_tail <= alpha)

}
