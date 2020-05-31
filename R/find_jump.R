#' Find The Jump in a Step Function
#'
#' @return the input at which the function steps
#'
#' @param .f step function
#' @param .bounds endpoints within which the function steps
#' @param .tol the desired accuracy
#' @param ... additional arguments to .f
#'
find_jump <- function(.f, .bounds, .tol = 1e-7, ...) {

  # Binary search for changepoint within bounds

  .bounds_in  <- .bounds
  .bounds_out <- c(.f(.bounds[1], ...),
                   .f(.bounds[2], ...))

  if (.bounds_out[1] == .bounds_out[2]) {
    rlang::abort(".f must return different values at each end of .bounds")
  }

  while(diff(.bounds_in) > .tol) {

    .mid_in        <- mean(.bounds_in)
    .mid_out       <- .f(.mid_in, ...)
    .which_replace <- match(.mid_out, .bounds_out)

    .bounds_in[.which_replace]  <- .mid_in
    .bounds_out[.which_replace] <- .mid_out

  }

  mean(.bounds_in)

}
