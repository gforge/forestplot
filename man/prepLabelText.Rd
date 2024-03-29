% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/private_prepLabelText.R
\name{prepLabelText}
\alias{prepLabelText}
\alias{[.forestplot_labeltext}
\title{Prepares label text}
\usage{
prepLabelText(labeltext, nr)

\method{[}{forestplot_labeltext}(x, i, j, ...)
}
\arguments{
\item{labeltext}{The label text input, either \code{expression}, \code{list}
\code{vector} or \code{matrix}}

\item{nr}{The number of rows}

\item{x}{A \code{forestplot_labeltext} object}

\item{i}{The row}

\item{j}{The column}

\item{...}{Passed on to the \code{fn.ci_norm} and
\code{fn.ci_sum} arguments}
}
\value{
Returns a \code{forestplot_labeltext} object with attributes:
\itemize{
\item no_cols
\item no_rows
\item widthcolumn
\item label_type
}
}
\description{
Prepares an object that contains the number of columns and rows
}
\section{Functions}{
\itemize{
\item \code{[}: Pick the value that corresponds to the row and column.
Returns \code{expression}, \code{call}, or \code{text}.

}}
\keyword{internal}
