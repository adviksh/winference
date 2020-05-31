#' Expected Value of Truncated Normal Times Indicator
#'
#' @param c_z interval for indicator
#' @param mu mean of the normal distribution pre-truncation
#' @param sigma standard deviation of the normal distribution pre-truncation
#' @param trunc_lo lower truncation point
#' @param trunc_hi upper truncation point
#'
#'
#' @description E[z * 1(z in c_z)]
e_truncnorm_times_indicator <- function(c_z, mu, sigma, trunc_lo = -Inf,
                                        trunc_hi = Inf) {
  int <- stats::integrate(Vectorize(e_tti_integrand, "z"),
                          lower = trunc_lo,
                          upper = trunc_hi,
                          c_z   = c_z,
                          mu    = mu,
                          sigma = sigma,
                          trunc_lo = trunc_lo,
                          trunc_hi = trunc_hi)
  int$value
}

e_tti_integrand <- function(z, c_z, mu, side, sigma,
                            trunc_lo = -Inf, trunc_hi = Inf) {
  d <- d_truncnorm(x        = z,
                   mean     = mu,
                   sd       = sigma,
                   trunc_lo = trunc_lo,
                   trunc_hi = trunc_hi)
  z * is_contained(z, c_z) * d
}
