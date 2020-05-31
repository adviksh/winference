#' Check 'estimates' Argument
#' @inheritParams ci_docs
#'
check_estimates <- function(estimates) {
  assertthat::assert_that(assertthat::noNA(estimates))
  assertthat::assert_that(all(is.numeric(estimates)))

  return(NULL)
}

#' Check 'st_errs' Argument
#' @inheritParams ci_docs
#'
check_st_errs <- function(st_errs, estimates) {
  assertthat::assert_that(is.numeric(st_errs))
  assertthat::assert_that(assertthat::noNA(st_errs))
  assertthat::assert_that(all(st_errs > 0))
  assertthat::assert_that(length(st_errs) == length(estimates))

  return(NULL)
}

#' Check 'conf_level' Argument
#' @inheritParams ci_docs
#'
check_conf_level <- function(conf_level) {
  # Length
  assertthat::assert_that(length(conf_level) == 1)

  # Type
  assertthat::assert_that(rlang::is_scalar_double(conf_level))

  # Properties
  assertthat::assert_that(conf_level > 0)
  assertthat::assert_that(conf_level < 1)

  return(NULL)
}

#' Check 'beta' Argument
#' @inheritParams ci_docs
#'
check_beta <- function(beta, conf_level) {
  # Length
  assertthat::assert_that(length(beta) == 1)

  # Type
  assertthat::assert_that(rlang::is_scalar_double(beta))

  # Properties
  assertthat::assert_that(beta >= 0)
  assertthat::assert_that(beta < (1 - conf_level))

  return(NULL)
}
