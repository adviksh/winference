#' Lower Truncation Point
#' @inheritParams ci_docs
#'
conditional_trunc_lo <- function(estimates) {
  max(estimates[-which.max(estimates)])
}

