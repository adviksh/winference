#' Truncation Points
#' @inheritParams ci_docs
#'
conditional_trunc_points <- function(estimates, k) {

  if (k > length(estimates)) {
    msg <- paste0("k (", k , ") cannot be larger than the length of estimates ",
                  "(", length(estimates), ").")
    rlang::abort(message = msg)
  }

  est_sort <- c(-Inf, sort(estimates), Inf)

  rnk <- length(estimates) - k + 2

  c(est_sort[rnk - 1], est_sort[rnk + 1])
}
