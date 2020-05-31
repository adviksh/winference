#' Expected Value of the Truncated Normal Distribution
#' @name truncnorm
#'
#' @description cumulative distribution, density, quantiles, and expected values
#' for the truncated normal distribution
#' with mean equal to [mean] and standard deviation equal to [sd],
#' truncated below at [trunc_lo] and above at [trunc_hi].
#' @inheritParams stats::rnorm
#' @param trunc_lo lower truncation piont
#' @param trunc_hi upper truncation piont
#'
NULL

#' @rdname truncnorm
p_truncnorm <- function(q, mean = 0, sd = 1, trunc_lo = -Inf, trunc_hi = Inf) {

  if (0 < q - trunc_lo & q - trunc_lo < .Machine$double.eps) return(0)
  if (0 < trunc_hi - q & trunc_hi - q < .Machine$double.eps) return(1)

  tryCatch(TruncatedNormal::ptmvnorm(q     = q,
                                     mu    = mean,
                                     sigma = sd,
                                     lb    = trunc_lo,
                                     ub    = trunc_hi),
           error = function(e) NA_real_)

}

#' @rdname truncnorm
d_truncnorm <- function(x, mean = 0, sd = 1, trunc_lo = -Inf, trunc_hi = Inf) {

  TruncatedNormal::dtmvnorm(x     = x,
                            mu    = mean,
                            sigma = sd,
                            lb    = trunc_lo,
                            ub    = trunc_hi)

}

#' @rdname truncnorm
q_truncnorm <- function(p, mean = 0, sd = 1, trunc_lo = -Inf, trunc_hi = Inf) {

  if (p == 0) return(trunc_lo)
  if (p == 1) return(trunc_hi)

  TruncatedNormal::qtnorm(p  = p,
                          mu = mean,
                          sd = sd,
                          lb = trunc_lo,
                          ub = trunc_hi)

  # ur_result <- uniroot(subtract(p_truncnorm, p),
  #                      lower    = TruncatedNormal::qtnorm(p  = p / 10,
  #                                                         mu = mean,
  #                                                         sd = sd,
  #                                                         lb = trunc_lo,
  #                                                         ub = trunc_hi),
  #                      upper    = TruncatedNormal::qtnorm(p  = 1 - (p / 10),
  #                                                         mu = mean,
  #                                                         sd = sd,
  #                                                         lb = trunc_lo,
  #                                                         ub = trunc_hi),
  #                      extendInt = "yes",
  #                      mean     = mean,
  #                      sd       = sd,
  #                      trunc_lo = trunc_lo,
  #                      trunc_hi = trunc_hi)
  #
  # ur_result$root

}

#' @rdname truncnorm
e_truncnorm <- function(mean = 0, sd = 1, trunc_lo = -Inf, trunc_hi = Inf) {
  alpha <- (trunc_lo - mean) / sd
  beta  <- (trunc_hi - mean) / sd
  Z     <- stats::pnorm(beta) - stats::pnorm(alpha)

  mean + (stats::dnorm(alpha) - stats::dnorm(beta)) * sd / Z
}

#' @rdname truncnorm
r_truncnorm <- function(n = 1, mean = 0, sd = 1, trunc_lo = -Inf, trunc_hi = Inf) {
  TruncatedNormal::rtnorm(n  = n,
                          mu = mean,
                          sd = sd,
                          lb = trunc_lo,
                          ub = trunc_hi)
}
