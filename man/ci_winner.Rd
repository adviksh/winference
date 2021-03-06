% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/conf_int_winner.R
\name{ci_winner}
\alias{ci_winner}
\title{Confidence Interval for Winning Estimate}
\usage{
ci_winner(interval_type, ...)
}
\arguments{
\item{interval_type}{character \cr
One of 'standard', 'conditional_unbiased', 'unconditional', or
'hybrid_unbiased'.}

\item{...}{\cr
Additional arguments for the desired interval type.}
}
\description{
A utility function, intended for programming and testing
package code. For analysis, try using other functions, like
ci_conditional_unbiased() or ci_hybrid_unbiased().
}
