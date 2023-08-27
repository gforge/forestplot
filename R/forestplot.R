#' @title Draws a forest plot
#'
#' @description This function generates a forest plot with extended capabilities compared to
#' the default `forestplot()` function in the `rmeta` package. It overcomes some limitations
#' of the original function, including the addition of expressions, use of multiple confidence
#' bands per label, autosizing to viewport, and uses modern tidyverse syntax. Refer to `vignette("forestplot")`
#' for comprehensive details.
#'
#' @details This version of `forestplot()` enhances the standard function in the following ways:
#' * **Adding Expressions:** Allows the use of expressions, such as `expression(beta)`.
#' * **Multiple Bands:** Enables multiple confidence bands for the same label.
#' * **Autosize:** Adapts to the viewport (graph) size.
#' * **Tidyverse syntax:** Utilizes convenient dplyr/tidyverse syntax for more flexible data manipulation.
#'
#' @section Multiple bands:
#' Multiple bands (or lines) per variable can be useful for comparing different outcomes.
#' For instance, you may want to compare heart disease-specific survival to overall survival
#' rates for smokers. It can be insightful to overlay two bands for this purpose. Another application
#' could be displaying crude and adjusted estimates as separate bands.
#'
#' @section Horizontal lines:
#'
#' The `hrzl_lines` argument can be set as `TRUE` or a `list` with `grid::gpar` elements.
#'
#' * `TRUE`: A line will be added based upon the \code{is.summary} rows. If the first line is a summary it
#' * `grid::gpar`: The same as above but the lines will be formatted according to the [`grid::gpar`] element
#' * `list`: The list must either be numbered, i.e. \code{list("2" = gpar(lty = 1))}, or have the same length
#'           as the \code{NROW(mean) + 1}. If the list is numbered the numbers should not exceed the \code{NROW(mean) + 1}.
#'           The no. \emph{1 row designates the top}, i.e. the line above the first row, all other correspond  to
#'           \emph{the row below}. Each element in the list needs to be \code{TRUE}, \code{NULL}, or
#'           \code{\link[grid]{gpar}} element. The \code{TRUE} defaults to a standard line, the \code{NULL}
#'           skips a line, while \code{\link[grid]{gpar}} corresponds to the fully customized line. Apart from
#'           allowing standard \code{\link[grid]{gpar}} line descriptions, \code{lty}, \code{lwd}, \code{col}, and more
#'           you can also specify \code{gpar(columns = c(1:3, 5))} if you for instance want the line to skip a column.
#'
#' @section Known Issues:
#'
#' * The x-axis does not completely adhere to the margin.
#' * Autosizing boxes may not always yield the best visual result; manual adjustment is recommended where possible.
#'
#'
#' @section API Changes from `rmeta` package's `forestplot`:
#'
#' * **xlog:** Outputs the axis in log() format, but the input data should be in antilog/exp format.
#' * **col:** The corresponding function in this package is `fpColors`.
#'
#' @param labeltext A list, matrix, vector or expression with the names of each
#'  row or the name of the column if using the *dplyr* select syntax - defaults to "labeltext".
#'  Note that when using `group_by` a separate labeltext is not allowed.
#'  The list should be wrapped in m x n number to resemble a matrix:
#'  \code{list(list("rowname 1 col 1", "rowname 2 col 1"), list("r1c2", expression(beta))}.
#'  You can also provide a matrix although this cannot have expressions by design:
#'  \code{matrix(c("rowname 1 col 1", "rowname 2 col 1", "r1c2", "beta"), ncol = 2)}.
#'  Use \code{NA}:s for blank spaces and if you provide a full column with \code{NA} then
#'  that column is a empty column that adds some space. \emph{Note:} If you do not
#'  provide the mean/lower/upper arguments the function expects the label text
#'  to be a matrix containing the labeltext in the rownames and then columns for
#'  mean, lower, and upper.
#' @param mean The name of the column if using the *dplyr* select syntax - defaults to "mean",
#'  else it should be a vector or a matrix with the averages. You can also provide a 2D/3D
#'  matrix that is automatically converted to the lower/upper parameters. The values
#'  should be in exponentiated form if they follow this interpretation, e.g. use
#'  exp(mean) if you have the output from a logistic regression
#' @param lower The lower bound of the confidence interval for the forestplot, needs
#'   to be the same format as the mean.
#' @param upper The upper bound of the confidence interval for the forestplot, needs
#'   to be the same format as the mean.
#' @param align Vector giving alignment (l,r,c) for the table columns
#' @param is.summary A vector indicating by \code{TRUE}/\code{FALSE} if
#'   the value is a summary value which means that it will have a different
#'   font-style
#' @param graph.pos The position of the graph element within the table of text. The
#'   position can be \code{1-(ncol(labeltext) + 1)}. You can also choose set the position
#'   to \code{"left"} or \code{"right"}.
#' @param hrzl_lines Add horizontal lines to graph. Can either be \code{TRUE} or a \code{list}
#'   of \code{\link[grid]{gpar}}. See line section below for details.
#' @param clip Lower and upper limits for clipping confidence intervals to arrows
#' @param xlab x-axis label
#' @param zero x-axis coordinate for zero line. If you provide a vector of length 2 it
#'   will print a rectangle instead of just a line. If you provide NA the line is suppressed.
#' @param graphwidth Width of confidence interval graph, see \code{\link[grid]{unit}} for
#'   details on how to utilize mm etc. The default is \code{auto}, that is it uses up whatever
#'   space that is left after adjusting for text size and legend
#' @param colgap Sets the gap between columns, defaults to 6 mm but for relative widths.
#'   Note that the value should be in \code{\link[grid]{unit}(,"npc")}.
#' @param lineheight Height of the graph. By default this is \code{auto} and adjusts to the
#'   space that is left after adjusting for x-axis size and legend. Sometimes
#'   it might be desirable to set the line height to a certain height, for
#'   instance if you have several forestplots you may want to standardize their
#'   line height, then you set this variable to a certain height, note this should
#'   be provided as a \code{\link[grid]{unit}} object. A good option
#'   is to set the line height to \code{unit(2, "cm")}. A third option
#'   is to set line height to "lines" and then you get 50% more than what the
#'   text height is as your line height
#' @param line.margin Set the margin between rows, provided in numeric or \code{\link[grid]{unit}} form.
#'   When having multiple confidence lines per row setting the correct
#'   margin in order to visually separate rows
#' @param col Set the colors for all the elements. See \code{\link{fpColors}} for
#'   details
#' @param txt_gp Set the fonts etc for all text elements. See \code{\link{fpTxtGp}}
#'   for details
#' @param xlog If TRUE, x-axis tick marks are to follow a logarithmic scale, e.g. for
#'   logistic regression (OR), survival estimates (HR), Poisson regression etc.
#'   \emph{Note:} This is an intentional break with the original \code{forestplot}
#'   function as I've found that exponentiated ticks/clips/zero effect are more
#'   difficult to for non-statisticians and there are sometimes issues with rounding
#'   the tick marks properly.
#' @param xticks Optional user-specified x-axis tick marks. Specify NULL to use
#'   the defaults, numeric(0) to omit the x-axis. By adding a labels-attribute,
#'   \code{attr(my_ticks, "labels") <- ...} you can dictate the outputted text
#'   at each tick. If you specify a boolean vector then ticks indicated with
#'   FALSE wont be printed. Note that the labels have to be the same length
#'   as the main variable.
#' @param xticks.digits The number of digits to allow in the x-axis if this
#'   is created by default
#' @param grid If you want a discrete gray dashed grid at the level of the
#'   ticks you can set this parameter to \code{TRUE}. If you set the parameter
#'   to a vector of values lines will be drawn at the corresponding positions.
#'   If you want to specify the \code{\link[grid]{gpar}} of the lines then either
#'   directly pass a \code{\link[grid]{gpar}} object or set the gp attribute e.g.
#'   \code{attr(line_vector, "gp") <- \link[grid]{gpar}(lty = 2, col = "red")}
#' @param lwd.xaxis lwd for the xaxis, see \code{\link[grid]{gpar}}
#' @param lwd.zero  lwd for the vertical line that gives the no-effect line, see \code{\link[grid]{gpar}}
#' @param lwd.ci lwd for the confidence bands, see \code{\link[grid]{gpar}}
#' @param lty.ci lty for the confidence bands, see \code{\link[grid]{gpar}}
#' @param ci.vertices Set this to TRUE if you want the ends of the confidence
#'  intervals to be shaped as a T. This is set default to TRUE if you have
#'  any other line type than 1 since there is a risk of a dash occurring
#'  at the very end, i.e. showing incorrectly narrow confidence interval.
#' @param ci.vertices.height The height hoft the vertices. Defaults to npc units
#'  corresponding to 10% of the row height.
#'  \emph{Note that the arrows correspond to the vertices heights.}
#' @param boxsize Override the default box size based on precision
#' @param mar A numerical vector of the form \code{c(bottom, left, top, right)} of
#'   the type \code{\link[grid]{unit}}
#' @param title The title of the plot if any
#' @param legend Legend corresponding to the number of bars
#' @param legend_args The legend arguments as returned by the \code{\link{fpLegend}} function.
#' @param new_page If you want the plot to appear on a new blank page then set this to \code{TRUE}, by
#'  default it is \code{TRUE}. If you want to change this behavior for all plots then
#'  set the \code{options(forestplot_new_page = FALSE)}
#' @param fn.ci_norm You can specify exactly how the line with the box is
#'  drawn for the normal (i.e. non-summary) confidence interval by changing this
#'  parameter to your own function or some of the alternatives provided in the package.
#'  It defaults to the box function \code{\link{fpDrawNormalCI}}
#' @param fn.ci_sum Same as previous argument but for the summary outputs
#'  and it defaults to \code{\link{fpDrawSummaryCI}}.
#' @param fn.legend What type of function should be used for drawing the
#'  legends, this can be a list if you want different functions. It defaults to
#'  a box if you have anything else than a single function or the number of columns
#'  in the \code{mean} argument
#' @param shapes_gp Sets graphical parameters (squares and lines widths, styles, etc.)
#'  of all shapes drawn (squares, lines, diamonds, etc.). This overrides \code{col},
#'  \code{lwd.xaxis}, \code{lwd.zero}, \code{lwd.ci} and \code{lty.ci}.
#' @param ... Passed on to the \code{fn.ci_norm} and
#'  \code{fn.ci_sum} arguments
#' @return `gforge_forestplot` object
#'
#' @import grid
#' @importFrom grDevices dev.cur
#'
#' @author Max Gordon, Thomas Lumley
#'
#' @example inst/examples/forestplot_example.R
#' @family forestplot functions
#' @seealso `vignette("forestplot")`
#' @rdname forestplot
#' @export forestplot
#' @aliases forestplot forestplot.default
forestplot <- function(...) {
  UseMethod("forestplot")
}
