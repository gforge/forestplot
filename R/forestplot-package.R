#' Package description
#'
#' The forest plot function, \code{\link{forestplot}}, is a more general
#' version of the original \pkg{rmeta}-packages \code{forestplot}
#' implementation. The aim is at using forest plots for more than
#' just meta-analyses.
#'
#' The forestplot:
#' \enumerate{
#'   \item Allows for multiple confidence intervals per row
#'   \item Custom fonts for each text element
#'   \item Custom confidence intervals
#'   \item Text mixed with expressions
#'   \item Legends both on top/left of the plot and within the graph
#'   \item Custom line height including auto-adapt height
#'   \item Graph width that auto-adapts
#'   \item Flexible arguments
#'   \item and more
#' }
#'
#' @section Additional functions:
#'
#' The \code{\link{getTicks}} tries to format ticks for plots in a nicer way.
#' The major use is for exponential form where ticks are generated using the
#' \eqn{2^n} since a doubling is a concept easy to grasp for less mathematical-savvy
#' readers.
#'
#' @name forestplot-package
#' @docType package
NULL
