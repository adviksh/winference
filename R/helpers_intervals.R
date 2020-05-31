#' Return the Intersection of Two Intervals
#'
#' @param int_1 double(2) \cr
#' An interval to intersect with [int_2]
#'
#' @param int_2 double(2) \cr
#' An interval to intersect with [int_1]
#'
intersect_intervals <- function(int_1, int_2) {

  if (are_disjoint_intervals(int_1, int_2)) return(NULL)
  if (is_contained(this = int_1, in_this = int_2)) return(int_1)
  if (is_contained(this = int_2, in_this = int_1)) return(int_2)

  if (min(int_1) < min(int_2)) return(c(min(int_2), max(int_1)))
  if (min(int_2) < min(int_1)) return(c(min(int_1), max(int_2)))

  rlang::abort("intersect_intervals: unanticipated condition")
}

#' Check if Two Intervals are Disjoint
#'
#' @inheritParams intersect_intervals
are_disjoint_intervals <- function(int_1, int_2) {
  max(int_1) < min(int_2) | max(int_2) < min(int_1)
}

#' Check if a Point/Interval is Contained in an Interval
#'
#' @param this double(2) \cr
#' A point or interval that may fall inside the interval [in_this]
#'
#' @param in_this double(2) \cr
#' An interval that may contain the point/interval [this]
#'
is_contained <- function(this, in_this) {
  min(this) >= min(in_this) &
  max(this) <= max(in_this)
}
