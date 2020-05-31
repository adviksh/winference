#' Confidence Interval for Winning Estimate
#' @export
#'
#' @description A utility function, intended for programming and testing
#' package code. For analysis, try using other functions, like
#' ci_conditional_unbiased() or ci_hybrid_unbiased().
#'
#'
#' @param interval_type character \cr
#' One of 'standard', 'conditional_unbiased', 'unconditional', or
#' 'hybrid_unbiased'.
#'
#' @param ... \cr
#' Additional arguments for the desired interval type.
#'
ci_winner <- function(interval_type, ...) {
  switch(interval_type,
         standard                 = ci_standard(...),
         conditional_equal_tailed = ci_conditional_equal_tailed(...),
         unconditional            = ci_unconditional(...),
         hybrid_equal_tailed      = ci_hybrid_equal_tailed(...),
         rlang::abort("unrecognized interval_type"))
}
