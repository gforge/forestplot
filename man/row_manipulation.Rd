% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fp_insert_row.R
\name{fp_insert_row}
\alias{fp_insert_row}
\alias{fp_add_header}
\alias{fp_append_row}
\title{Insert/append rows into forestplot}
\usage{
fp_insert_row(
  x,
  ...,
  mean = NULL,
  lower = NULL,
  upper = NULL,
  position = 1,
  is.summary = FALSE,
  boxsize = NA
)

fp_add_header(x, ..., position = 1, is.summary = TRUE)

fp_append_row(x, ..., position = "last", is.summary = FALSE)
}
\arguments{
\item{x}{The forestplot object}

\item{...}{Either named arguments that correspond to the original column
names or unnamed arguments that will map in appearing order.}

\item{mean}{Either a mean or all the values if three columns (mean, lower, upper)}

\item{lower}{A vector or matrix with the lower confidence interval}

\item{upper}{A vector or matrix with the upper confidence interval}

\item{position}{The row position to input at. Either a row number or "last".}

\item{is.summary}{Whether the row is a summary.}

\item{boxsize}{The box size for the drawn estimate line}
}
\value{
The foresplot object with the added rows
}
\description{
These functions are used for inserting or appending
a row into a forestplot object. Can be used for inputting multiple
rows. Just make sure that all elements are of equal length.
}
\examples{
base_data <- tibble::tibble(mean  = c(0.578, 0.165, 0.246, 0.700, 0.348, 0.139, 1.017),
                            lower = c(0.372, 0.018, 0.072, 0.333, 0.083, 0.016, 0.365),
                            upper = c(0.898, 1.517, 0.833, 1.474, 1.455, 1.209, 2.831),
                            study = c("Auckland", "Block", "Doran", "Gamsu",
                                      "Morrison", "Papageorgiou", "Tauesch"),
                            deaths_steroid = c("36", "1", "4", "14", "3", "1", "8"),
                            deaths_placebo = c("60", "5", "11", "20", "7", "7", "10"),
                            OR = c("0.58", "0.16", "0.25", "0.70", "0.35", "0.14", "1.02"))

base_data |>
  forestplot(labeltext = c(study, deaths_steroid, deaths_placebo, OR),
             clip = c(0.1, 2.5),
             xlog = TRUE) |>
  fp_add_header(study = c("", "Study"),
                deaths_steroid = c("Deaths", "(steroid)"),
                deaths_placebo = c("Deaths", "(placebo)"),
                OR = c("", "OR")) |>
  fp_append_row(mean  = 0.531,
                lower = 0.386,
                upper = 0.731,
                study = "Summary",
                OR = "0.53",
                is.summary = TRUE)
}
\seealso{
Other graph modifiers: 
\code{\link{fp_add_lines}()},
\code{\link{fp_decorate_graph}()},
\code{\link{fp_set_style}()},
\code{\link{fp_set_zebra_style}()}

Other forestplot functions: 
\code{\link{forestplot}()},
\code{\link{fpColors}()},
\code{\link{fpDrawNormalCI}()},
\code{\link{fpLegend}()},
\code{\link{fpShapesGp}()},
\code{\link{fp_add_lines}()},
\code{\link{fp_decorate_graph}()},
\code{\link{fp_set_style}()},
\code{\link{fp_set_zebra_style}()}
}
\concept{forestplot functions}
\concept{graph modifiers}
