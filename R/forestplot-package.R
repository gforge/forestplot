#' Package description
#'
#' The forest plot function, [`forestplot()`], is a more general
#' version of the original \pkg{rmeta}-packages \code{forestplot}
#' implementation. The aim is at using forest plots for more than
#' just meta-analyses.
#'
#' The forestplot:
#' 1. Allows for multiple confidence intervals per row
#' 1. Custom fonts for each text element
#' 1. Custom confidence intervals
#' 1. Text mixed with expressions
#' 1. Legends both on top/left of the plot and within the graph
#' 1. Custom line height including auto-adapt height
#' 1. Graph width that auto-adapts
#' 1. Flexible arguments
#' 1. and more
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
