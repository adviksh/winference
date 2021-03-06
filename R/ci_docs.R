#' @title Confidence Interval Documentation
#' @name ci_docs
#'
#' @param estimates numeric vector \cr
#' A vector of estimates. Will return a confidence interval for the maximum
#' estimate.
#'
#' @param st_errs numeric vector \cr
#' A vector of estimated standard errors for each estimate.
#'
#' @param conf_level double in (0,1) \cr
#' The desired confidence level of the resulting interval. Note that
#' conditional and hybrid confidence intervals don't have nominal
#' coverage in the conventional sense.
#'
#' @param beta double in [0, 1 - conf_level] \cr
#' For hybrid intervals, degree of mixing between conditional and unconditional
#' intervals. At 0, hybrid intervals are identical to conditional intervals.
#'
NULL
