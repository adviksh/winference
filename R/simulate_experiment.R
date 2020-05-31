#' Simulate Experiment(s)
#' @export
#'
#' @name simulate_experiments
#'
#' @description simulate experiment(s) with many treatment arms, where
#' true treatment effects in each arm are given by [true_means],
#' the standard errors of the estimated treatment effects is given by
#' [st_errs], and the sample size in each treatment arm is given by [n_obs].
#'
#' @return An experiment is a named list. simulate_experiment returns
#' a single list, while simulate_experiments returns a list of sublists where
#' each sublist is an experiment. An experiment has entries: \cr
#' \describe{
#'   \item{true_means}{The true mean outcome for each treatment arm.}
#'   \item{estimates}{The sample mean for each treatment arm.}
#'   \item{st_errs}{The true standard error of the sample means.}
#'   \item{st_errs_hat}{The estimated standard error of the sample means.}
#' }
#'
#' @param n_experiments integer \cr
#' Number of experiments to simulate
#'
#' @param true_means double vector \cr
#' Vector of true mean outcome for each treatment arm.
#'
#' @param st_errs double vector \cr
#' Standard error of the sample mean in each treatment arm.
#'
#' @param n_obs integer vector \cr
#' Number of observations in each treatment arm (default: 100).
#'
simulate_experiments <- function(n_experiments, true_means, st_errs,
                                 n_obs = 100) {

  replicate(n_experiments,
            simulate_experiment(true_means = true_means,
                                st_errs    = st_errs,
                                n_obs = n_obs),
            simplify = FALSE)

}

#' @rdname simulate_experiments
simulate_experiment <- function(true_means, st_errs, n_obs = 100) {

  # Defense -----------------------------------------------------------------
  if (length(st_errs) == 1) st_errs <- rep(st_errs, length(true_means))

  if (length(n_obs) == 1) n_obs <- rep(n_obs, length(true_means))

  if (length(true_means) != length(st_errs)) {
    rlang::abort("true_means and st_errs must be the same length")
  }

  if (length(true_means) != length(n_obs)) {
    rlang::abort("true_means and n_obs must be the same length")
  }


  # Function Body -----------------------------------------------------------
  n_tmt <- length(true_means)

  samp_means <- stats::rnorm(n_tmt, true_means, st_errs)

  # implied variance of unit-level observations
  obs_var  <- st_errs^2 * n_obs

  # estimated sample variance in each treatment arm
  samp_var <- stats::rchisq(n_tmt, n_obs - 1) * (obs_var / (n_obs - 1))

  list(true_means  = true_means,
       estimates   = samp_means,
       st_errs     = st_errs,
       st_errs_hat = sqrt(samp_var / n_obs))
}
