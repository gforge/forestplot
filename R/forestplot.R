#' Draws a forest plot
#'
#' The \emph{forestplot} is based on the \pkg{rmeta}-package`s
#' \code{\link[rmeta]{forestplot}} function. This
#' function resolves some limitations of the original
#' functions such as:
#' \itemize{
#'   \item{Adding expressions: }{Allows use of expressions, e.g. \code{expression(beta)}}
#'   \item{Multiple bands: }{Using multiple confidence bands for the same label}
#'   \item{Autosize: }{Adapts to viewport (graph) size}
#' }
#'
#' See \code{vignette("forestplot")} for details.
#'
#' @section Multiple bands:
#'
#' Using multiple bands, i.e. multiple lines, per variable can be interesting when
#' you want to compare different outcomes. E.g. if you want to compare survival
#' specific to heart disease to overall survival for smoking it may be useful to
#' have two bands on top of eachother. Another useful implementation is to show
#' crude and adjusted estimates as separate bands.
#'
#' @section Horizontal lines:
#'
#' The argument \code{hrzl_lines} can be either \code{TRUE} or a \code{list} with \code{\link[grid]{gpar}}
#' elements:
#'
#' \itemize{
#'  \item{\code{TRUE}}{A line will be added based upon the \code{is.summary} rows. If the first line is a summary it}
#'  \item{\code{\link[grid]{gpar}}}{The same as above but the lines will be formatted according to the
#'                                  \code{\link[grid]{gpar}} element}
#'  \item{\code{list}}{The list must either be numbered, i.e. \code{list("2" = gpar(lty=1))}, or have the same length
#'                     as the \code{NROW(mean) + 1}. If the list is numbered the numbers should not exceed the \code{NROW(mean) + 1}.
#'                     The no. \emph{1 row designates the top}, i.e. the line above the first row, all other correspond  to
#'                     \emph{the row below}. Each element in the list needs to be \code{TRUE}, \code{NULL}, or
#'                     \code{\link[grid]{gpar}} element. The \code{TRUE} defaults to a standard line, the \code{NULL}
#'                     skips a line, while \code{\link[grid]{gpar}} corresponds to the fully customized line. Apart from
#'                     allowing standard \code{\link[grid]{gpar}} line descriptions, \code{lty}, \code{lwd}, \code{col}, and more
#'                     you can also specify \code{gpar(columns = c(1:3, 5))} if you for instance want the line to skip a column.}
#' }
#'
#' @section Known issues:
#'
#' The x-axis does not entirely respect the margin. Autosizing boxes is not
#' always the best option, try to set these manually as much as possible.
#'
#' @section API-changes from \pkg{rmeta}-package`s \code{\link[rmeta]{forestplot}}:
#' \itemize{
#'   \item{xlog: }{The xlog outputs the axis in log() format but the input data should be in antilog/exp format}
#'   \item{col: }{The corresponding function is \code{\link{fpColors}} for this package}
#' }
#'
#' @param labeltext A list, matrix, vector or expression with the names of each
#'  row. The list should be wrapped in m x n number to resemble a matrix:
#'  \code{list(list("rowname 1 col 1", "rowname 2 col 1"), list("r1c2", expression(beta))}.
#'  You can also provide a matrix although this cannot have expressions by design:
#'  \code{matrix(c("rowname 1 col 1", "rowname 2 col 1", "r1c2", "beta"), ncol=2)}.
#'  Use \code{NA}:s for blank spaces and if you provide a full column with \code{NA} then
#'  that column is a empty column that adds some space. \emph{Note:} If you do not
#'  provide the mean/lower/upper arguments the function expects the label text
#'  to be a matrix containing the labeltext in the rownames and then columns for
#'  mean, lower, and upper.
#' @param mean A vector or a matrix with the averages. You can also provide a 2D/3D
#'  matrix that is automatically converted to the lower/upper parameters. The values
#'  should be in exponentiated form if they follow this interpretation, e.g. use
#'  exp(mean) if you have the output from a logistic regression
#' @param lower The lower bound of the confidence interval for the forestplot, needs
#'   to be the same format as the mean, i.e. matrix/vector of equal columns & length
#' @param upper The upper bound of the confidence interval for the forestplot, needs
#'   to be the same format as the mean, i.e. matrix/vector of equal columns \& length
#' @param align Vector giving alignment (l,r,c) for the table columns
#' @param is.summary A vector indicating by \code{TRUE}/\code{FALSE} if
#'   the value is a summary value which means that it will have a different
#'   font-style
#' @param graph.pos The position of the graph element within the table of text. The
#'   position can be \code{1-(ncol(labeltext) + 1)}. You can also choose set the positin
#'   to \code{"left"} or \code{"right"}.
#' @param hrzl_lines Add horizontal lines to graph. Can either be \code{TRUE} or a \code{list}
#'   of \code{\link[grid]{gpar}}. See line section below for details.
#' @param clip Lower and upper limits for clipping confidence intervals to arrows
#' @param xlab x-axis label
#' @param zero x-axis coordinate for zero line. If you provide a vector of length 2 it
#'   will print a rectangle instead of just a line.
#' @param graphwidth Width of confidence interval graph, see \code{\link[grid]{unit}} for
#'   details on how to utilize mm etc. The default is \code{auto}, that is it uses up whatever
#'   space that is left after adjusting for text size and legend
#' @param colgap Sets the gap between columns, defaults to 6 mm but for relative widths.
#'   Note that the value should be in \code{\link[grid]{unit}(,"npc")}.
#' @param lineheight Height of the graph. By default this is \code{auto} and adjustes to the
#'   space that is left after adjusting for x-axis size and legend. Sometimes
#'   it might be desireable to set the line height to a certain height, for
#'   instance if you have several forestplots you may want to standardize their
#'   line height, then you set this variable to a certain height, note this should
#'   be provided as a \code{\link[grid]{unit}} object. A good option
#'   is to set the line height to \code{unit(2, "cm")}. A third option
#'   is to set line height to "lines" and then you get 50 \% more than what the
#'   text height is as your line height
#' @param line.margin Set the margin between rows, provided in numeric or \code{\link[grid]{unit}} formar.
#'   When having multiple confidence lines per row setting the correct
#'   margin in order to visually separate rows
#' @param col Set the colors for all the elements. See \code{\link{fpColors}} for
#'   details
#' @param txt_gp Set the fonts etc for all text elements. See \code{\link{fpTxtGp}}
#'   for details
#' @param xlog If TRUE, x-axis tick marks are to follow a logarithmic scale, e.g. for
#'   logistic regressoin (OR), survival estimates (HR), poisson regression etc.
#'   \emph{Note:} This is an intentional break with the original \code{\link[rmeta]{forestplot}}
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
#'   \code{attr(line_vector, "gp") <- \link[grid]{gpar}(lty=2, col = "red")}
#' @param lwd.xaxis lwd for the xaxis, see \code{\link[grid]{gpar}}
#' @param lwd.zero  lwd for the vertical line that gives the no-effect line, see \code{\link[grid]{gpar}}
#' @param lwd.ci lwd for the confidence bands, see \code{\link[grid]{gpar}}
#' @param lty.ci lty for the confidence bands, see \code{\link[grid]{gpar}}
#' @param ci.vertices Set this to TRUE if you want the ends of the confidence
#'  intervals to be shaped as a T. This is set default to TRUE if you have
#'  any other line type than 1 since there is a risk of a dash occurring
#'  at the very end, i.e. showing incorrectly narrow confidence interval.
#' @param ci.vertices.height The height hoft the vertices. Defaults to npc units
#'  corresponding to 10\% of the row height.
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
#' @param ... Passed on to the \code{fn.ci_norm} and
#'  \code{fn.ci_sum} arguments
#'
#' @import grid
#'
#' @author Max Gordon, Thomas Lumley
#'
#' @example inst/examples/forestplot_example.R
#' @family forestplot functions
#' @rdname forestplot
#' @export forestplot
#' @aliases forestplot forestplot.default
forestplot <- function(...)
{
  UseMethod("forestplot")
}

#' @return \code{NULL}
#'
#' @rdname forestplot
#' @method forestplot default
#' @export
#' @importFrom checkmate assert_class assert_vector
forestplot.default <- function (labeltext,
                                mean, lower, upper,
                                align,
                                is.summary         = FALSE,
                                graph.pos          = "right",
                                hrzl_lines,
                                clip               = c(-Inf, Inf),
                                xlab               = "",
                                zero               = ifelse(xlog, 1, 0),
                                graphwidth         = "auto",
                                colgap,
                                lineheight         = "auto",
                                line.margin,
                                col                = fpColors(),
                                txt_gp             = fpTxtGp(),
                                xlog               = FALSE,
                                xticks,
                                xticks.digits      = 2,
                                grid               = FALSE,
                                lwd.xaxis,
                                lwd.zero,
                                lwd.ci,
                                lty.ci = 1,
                                ci.vertices,
                                ci.vertices.height = .1,
                                boxsize,
                                mar                = unit(rep(5, times=4), "mm"),
                                title,
                                legend,
                                legend_args        = fpLegend(),
                                new_page           = getOption("forestplot_new_page", TRUE),
                                fn.ci_norm         = fpDrawNormalCI,
                                fn.ci_sum          = fpDrawSummaryCI,
                                fn.legend,
                                ...)
{
  if (missing(colgap)){
    colgap <- convertUnit(unit(6, "mm"), "npc", valueOnly=TRUE)
    if (colgap < .1)
      colgap <- unit(.05, "npc")
    else
      colgap <- unit(colgap, "npc")
  }else if(!grid::is.unit(colgap)){
    colgap <- as.numeric(colgap)
    if (is.na(colgap))
      stop("Invalid colgap argument")
  }
  colgap <- convertUnit(colgap, "mm")

  assert_class(txt_gp, "fpTxtGp")
  assert_class(col, "fpColors")

  if (missing(lower) &&
      missing(upper) &&
      missing(mean)){
    if(missing(labeltext))
      stop("You need to provide the labeltext or",
           " the mean/lower/upper arguments")

    assert_matrix(labeltext, ncols=3)

    mean <- labeltext
    labeltext <- rownames(mean)
  }

  assert_vector(zero, max.len = 2)

  if (missing(labeltext))
    labeltext <- rownames(mean)

  if (is.null(labeltext))
    stop("You must provide labeltext either in the direct form as an argument",
         " or as rownames for the mean argument.")
  # Assume that lower and upper are contained within
  # the mean variable
  if (missing(lower) &&
      missing(upper)){
    if (NCOL(mean) != 3)
      stop("If you do not provide lower/upper arguments your mean needs to have 3 columns")

    # If the mean can in this case be eithe 2D-matrix
    # that generates a regular forest plot or
    # it can be a 3D-array where the 3:rd level
    # constitutes the different bands
    all <- prFpConvertMultidimArray(mean)
    mean <- all$mean
    lower <- all$lower
    upper <- all$upper
  }

  if (NCOL(mean) != NCOL(lower) ||
      NCOL(lower) != NCOL(upper) ||
      NCOL(mean) == 0)
    stop('Mean, lower and upper contain invalid number of columns',
         " Mean columns:", ncol(mean),
         " Lower bound columns:", ncol(lower),
         " Upper bound columns:", ncol(upper))

  if (NCOL(mean) != length(col$box)){
    col$box <- rep(col$box, length.out = NCOL(mean))
    col$line <- rep(col$lines, length.out = NCOL(mean))
  }

  # Prepare the legend marker
  if (!missing(legend)){
    fn.legend <- prFpPrepareLegendMarker(fn.legend = fn.legend,
                                         col_no = NCOL(mean),
                                         fn.ci_norm = fn.ci_norm)
  }

  if (!is.unit(lineheight) && !lineheight %in% c("auto", "lines"))
    stop("The argument lineheight must either be of type unit or set to 'auto',",
         " you have provided a '", class(lineheight), "' class")

  if (!missing(legend)){
    if (length(legend) != ncol(mean))
      stop("If you want a legend you need to provide the same number of",
           " legend descriptors as you have boxes per line, currently you have ",
           ncol(mean), " boxes and ",
           length(legend), " legends.")
    if (is.list(legend_args$pos)){
      legend_args$pos <- prFpGetLegendBoxPosition(legend_args$pos)
    }else if (!legend_args$pos %in% c("top", "right")){
      stop("The legend is either a list positioning it inside the main plot or at the 'top' or 'right' side,",
           " the position '", legend_args$pos, "' is not valid.")
    }

    if (inherits(legend_args$gp, "gpar")){
      # Remove default border if no color
      # unless there is a line width or type specified
      if (!"col" %in% names(legend_args$gp)){
        if (any(c("lwd", "lwd") %in% names(legend_args$gp))){
          legend_args$gp[["col"]] = "black"
        }else{
          legend_args$gp[["col"]] = NA
        }
      }
    }
  }

  # Fix if data.frames were provided in the arguments
  if (is.data.frame(mean))
    mean <- as.matrix(mean)
  if (is.data.frame(lower))
    lower<- as.matrix(lower)
  if (is.data.frame(upper))
    upper <- as.matrix(upper)

  # Save the original values since the function due to it's inheritance
  # from the original forestplot needs some changing to the parameters
  if (xlog){
    if (any(mean < 0, na.rm = TRUE) ||
        any(lower < 0, na.rm = TRUE) ||
        any(upper < 0, na.rm = TRUE) ||
        zero <= 0 ||
        (!missing(clip) && any(clip <= 0, na.rm = TRUE)) ||
        (!missing(grid) && any(grid <= 0, na.rm = TRUE))){
      stop("All argument values (mean, lower, upper, zero, grid and clip)",
           " should be provided as exponentials when using the log scale.",
           " This is an intentional break with the original forestplot function in order",
           " to simplify other arguments such as ticks, clips, and more.")
    }

    # Change all the values along the log scale
    org_mean <- log(mean)
    org_lower <- log(lower)
    org_upper <- log(upper)
  }else{
    org_mean <- mean
    org_lower <- lower
    org_upper <- upper
  }

  # For border calculations etc it's
  # convenient to have the matrix as a
  # vector
  if (NCOL(mean) > 1){
    mean <- as.vector(mean)
    lower <- as.vector(lower)
    upper <- as.vector(upper)
  }

  nr <- NROW(org_mean)

  # Get the number of columns (nc) and number of rows (nr)
  # if any columns are to be spacers the widthcolumn variable
  if (is.expression(labeltext)){
    widthcolumn <- c(TRUE)
    # Can't figure out multiple levels of expressions
    nc <- 1
    label_type = "expression"
    label_nr <- length(labeltext)
  } else if (is.list(labeltext)){
    if (all(sapply(labeltext, function(x){
      length(x) == 1 &&
        !is.list(x)
    }))){
      labeltext <-
        list(labeltext)
    }
    if (!prFpValidateLabelList(labeltext))
      stop("Invalid labellist, it has to be formed as a matrix m x n elements")

    # Can't figure out multiple levels of expressions
    nc <- length(labeltext)

    widthcolumn = c()
    # Should mark the columns that don't contain
    # epressions, text or numbers as widthcolumns
    for(col.no in seq(along=labeltext)){
      empty_row <- TRUE
      for (row.no in seq(along=labeltext[[col.no]])){
        if (is.expression(labeltext[[col.no]][[row.no]]) ||
            !is.na(labeltext[[col.no]][[row.no]])){
          empty_row <- FALSE
          break
        }
      }
      widthcolumn <- append(widthcolumn, empty_row)
    }

    label_type = "list"
    label_nr <- length(labeltext[[1]])
  } else if (is.vector(labeltext)){
    widthcolumn <- c(FALSE)
    nc = 1

    labeltext <- matrix(labeltext, ncol=1)
    label_type = "matrix"
    label_nr <- NROW(labeltext)
  } else {
    # Original code for matrixes
    widthcolumn <- !apply(is.na(labeltext), 1, any)
    nc <- NCOL(labeltext)
    label_type = "matrix"
    label_nr <- NROW(labeltext)
  }

  if (nr != label_nr){
    stop("You have provided ", nr, " rows in your",
         " mean arguement while the labels have ", label_nr, " rows")
  }

  if (is.character(graph.pos)){
    graph.pos <-
      switch(graph.pos,
             right = nc + 1,
             last = nc + 1,
             left = 1,
             first = 1,
             stop("The graph.pos argument has an invalid text argument.",
                  " The only values accepted are 'left'/'right' or 'first'/'last'.",
                  " You have provided the value '", graph.pos, "'"))
  }else if(is.numeric(graph.pos)){
    if (!graph.pos %in% 1:(NCOL(labeltext) + 1))
      stop("The graph position must be between 1 and ", (NCOL(labeltext) + 1), ".",
           " You have provided the value '", graph.pos, "'.")
  }else{
    stop("The graph pos must either be a string consisting of 'left'/'right' (alt. 'first'/'last')",
         ", or an integer value between 1 and ", (NCOL(labeltext) + 1))
  }

  # Prepare the summary and align variables
  if (missing(align)){
    if (graph.pos == 1)
      align <- rep("l", nc)
    else if (graph.pos == nc + 1)
      align <- c("l", rep("r", nc - 1))
    else
      align <- c("l",
                 rep("c", nc - 1))
  } else {
    align <- rep(align, length.out = nc)
  }

  is.summary <- rep(is.summary, length = nr)

  if (is.matrix(mean)) {
    missing_rows <- apply(mean, 2, function(row) all(is.na(row)))
  }else{
    missing_rows <- sapply(mean, is.na)
  }

  fn.ci_norm <-
    prFpGetConfintFnList(fn = fn.ci_norm,
                         no_rows = NROW(mean),
                         no_cols = NCOL(mean),
                         missing_rows = missing_rows,
                         is.summary = is.summary,
                         summary = FALSE)
  fn.ci_sum <-
    prFpGetConfintFnList(fn = fn.ci_sum,
                         no_rows = NROW(mean),
                         no_cols = NCOL(mean),
                         missing_rows = missing_rows,
                         is.summary = is.summary,
                         summary = TRUE)

  lty.ci <- prPopulateList(lty.ci,
                           no_rows = NROW(mean),
                           no_cols = NCOL(mean))


  hrzl_lines <- prFpGetLines(hrzl_lines = hrzl_lines,
                             is.summary = is.summary,
                             total_columns = nc + 1,
                             col = col)

  labels <- prFpGetLabels(label_type = label_type,
                          labeltext = labeltext, align = align,
                          nc = nc, nr = nr,
                          is.summary = is.summary,
                          txt_gp = txt_gp,
                          col = col)

  # There is always at least one column so grab the widest one
  # and have that as the base for the column widths
  colwidths <- unit.c(prFpFindWidestGrob(labels[[1]]))

  # If multiple row label columns, add the other column widths
  if (nc > 1) {
    for (i in 2:nc){
      colwidths <- unit.c(colwidths,
                          colgap,
                          prFpFindWidestGrob(labels[[i]]))
    }
  }


  axisList <- prFpGetGraphTicksAndClips(xticks = xticks,
                                        xticks.digits = xticks.digits,
                                        grid = grid,
                                        xlog = xlog,
                                        xlab = xlab,
                                        lwd.xaxis = lwd.xaxis,
                                        txt_gp = txt_gp,
                                        col = col,
                                        clip = clip,
                                        zero = zero,
                                        x_range=prFpXrange(upper = upper,
                                                           lower = lower,
                                                           clip = clip,
                                                           zero = zero,
                                                           xticks = xticks,
                                                           xlog = xlog),
                                        mean = org_mean,
                                        graph.pos = graph.pos)
  clip <- axisList$clip

  ###################
  # Create the plot #
  ###################
  if (new_page) grid.newpage()

  # Adjust for the margins and the x-axis + label
  marList <- list()

  # This breaks without separate variables
  marList$bottom <- convertY(mar[1], "npc")
  marList$left <- convertX(mar[2], "npc")
  marList$top <- convertY(mar[3], "npc")
  marList$right <- convertX(mar[4], "npc")

  prPushMarginViewport(bottom = marList$bottom,
                       left = marList$left,
                       top = marList$top,
                       right = marList$right,
                       name="forestplot_margins")

  if (!missing(title)){
    prGridPlotTitle(title=title, gp = txt_gp$title)
  }

  # Initiate the legend
  if (!missing(legend)){
    lGrobs <- prFpGetLegendGrobs(legend = legend,
                                 txt_gp = txt_gp,
                                 title = legend_args$title)
    legend_colgap <- colgap
    if (convertUnit(legend_colgap, unitTo = "mm", valueOnly = TRUE) >
        convertUnit(attr(lGrobs, "max_height"), unitTo = "mm", valueOnly = TRUE)){
      legend_colgap <- attr(lGrobs, "max_height")
    }

    legend_horizontal_height <-
      sum(legend_args$padding,
          attr(lGrobs, "max_height"),
          legend_args$padding)
    if (!is.null(attr(lGrobs, "title"))){
      legend_horizontal_height <-
        sum(attr(lGrobs, "titleHeight"),
            attr(lGrobs, "line_height_and_spacing")[2],
            legend_horizontal_height)
    }
    legend_vertical_width <-
      sum(unit.c(legend_args$padding,
                 attr(lGrobs, "max_height"),
                 legend_colgap,
                 attr(lGrobs, "max_width"),
                 legend_args$padding))



    # Prepare the viewports if the legend is not
    # positioned inside the forestplot, i.e. on the top or right side
    if ((!is.list(legend_args$pos) && legend_args$pos == "top") ||
        ("align" %in% names(legend_args$pos) && legend_args$pos[["align"]] == "horizontal")){
      legend_layout <- grid.layout(nrow=3, ncol=1,
                                   heights=unit.c(legend_horizontal_height,
                                                  legend_colgap+legend_colgap,
                                                  unit(1, "npc")-
                                                    legend_horizontal_height-
                                                    legend_colgap-legend_colgap))

      legend_pos <- list(row = 1,
                         col = 1)
      main_pos <- list(row = 3,
                       col = 1)
    }else{
      legend_layout <- grid.layout(nrow=1, ncol=3,
                                   widths = unit.c(unit(1, "npc") -
                                                     legend_colgap -
                                                     legend_vertical_width,
                                                   legend_colgap,
                                                   legend_vertical_width))
      legend_pos <- list(row = 1,
                         col = 3)
      main_pos <- list(row = 1,
                       col = 1)
    }
  }

  # If the legend should be positioned within the plot then wait
  # until after the plot has been drawn
  if (!missing(legend) > 0 &&
      !is.list(legend_args$pos)){
    pushViewport(prFpGetLayoutVP(lineheight=lineheight,
                                 labels = labels,
                                 nr=nr,
                                 legend_layout=legend_layout))
    vp <- viewport(layout.pos.row = legend_pos$row,
                   layout.pos.col = legend_pos$col,
                   name = "legend")
    pushViewport(vp)

    # Draw the legend
    prFpDrawLegend(lGrobs = lGrobs,
                   col = col,
                   colgap = convertUnit(legend_colgap, unitTo="mm"),
                   pos = legend_args$pos,
                   gp = legend_args$gp,
                   r = legend_args$r,
                   padding = legend_args$padding,
                   fn.legend = fn.legend,
                   ...)
    upViewport()

    # Reset to the main plot
    vp <- viewport(layout.pos.row = main_pos$row,
                   layout.pos.col = main_pos$col,
                   name="main")
    pushViewport(vp)
  }else{
    pushViewport(prFpGetLayoutVP(lineheight=lineheight,
                                 labels = labels, nr=nr))
  }

  ###########################################
  # Normalize the widths to cover the whole #
  # width of the graph space.               #
  ###########################################
  if(!is.unit(graphwidth) &&
     graphwidth=="auto"){
    # If graph width is not provided as a unit the autosize it to the
    # rest of the space available
    npc_colwidths <- convertUnit(unit.c(colwidths, colgap), "npc", valueOnly=TRUE)
    graphwidth <- unit(1 - sum(npc_colwidths), "npc")
  }else if(!is.unit(graphwidth)){
    stop("You have to provide graph width either as a unit() object or as 'auto'.",
         " Auto sizes the graph to maximally use the available space.",
         " If you want to have exact mm width then use graphwidth = unit(34, 'mm').")
  }

  # Add the base grapwh width to the total column width
  # default is 2 inches
  if (graph.pos == 1){
    colwidths <- unit.c(graphwidth, colgap, colwidths)
  }else if (graph.pos == nc + 1){
    colwidths <- unit.c(colwidths, colgap, graphwidth)
  }else{
    spl_position <- ((graph.pos-1)*2 - 1)
    colwidths <- unit.c(colwidths[1:spl_position],
                        colgap,
                        graphwidth,
                        colwidths[(spl_position + 1):length(colwidths)])
  }

  # Add space for the axis and the label
  axis_height <- unit(0, "npc")
  if (is.grob(axisList$axisGrob))
    axis_height <- axis_height  + grobHeight(axisList$axisGrob)
  if (is.grob(axisList$labGrob)){
    gp_lab_cex <- prGetTextGrobCex(axisList$labGrob)

    # The lab grob y actually includes the axis (note negative)
    axis_height <-  axis_height +
      unit(gp_lab_cex+.5, "line")
  }

  axis_layout <- grid.layout(nrow=2,
                             ncol=1,
                             heights=unit.c(unit(1, "npc") - axis_height,
                                            axis_height))
  pushViewport(viewport(layout=axis_layout,
                        name="axis_margin"))
  pushViewport(viewport(layout.pos.row=1, layout.pos.col=1))

  # The base viewport, set the increase.line_height paremeter if it seems a little
  # crowded between the lines that might happen when having multiple comparisons
  main_grid_layout <- grid.layout(nrow   = nr,
                                  ncol   = length(colwidths),
                                  widths = colwidths,
                                  heights = unit(rep(1/nr, nr), "npc"),
                                  respect = TRUE)
  pushViewport(viewport(layout = main_grid_layout,
                        name="BaseGrid"))

  # Create the fourth argument 4 the fpDrawNormalCI() function
  if (!missing(boxsize)){
    # If matrix is provided this will convert it
    # to a vector but it doesn't matter in this case
    info <- rep(boxsize, length = length(mean))
  }else{
    # Get width of the lines
    cwidth <- (upper - lower)
    # Set cwidth to min value if the value is invalid
    # this can be the case for reference points
    cwidth[cwidth <= 0 | is.na(cwidth)] <- min(cwidth[cwidth > 0])
    textHeight <- convertUnit(grobHeight(textGrob("A", gp=do.call(gpar, txt_gp$label))),
                              unitTo="npc",
                              valueOnly=TRUE)
    info <- 1/cwidth*0.75
    info <- info/max(info[!is.summary], na.rm = TRUE)
    # Adjust the dots as it gets ridiculous with small text and huge dots
    if (any(textHeight*(nr+.5) * 1.5 < info))
      info <- textHeight*(nr+.5) * 1.5 * info/max(info, na.rm=TRUE) + textHeight*(nr+.5)*1.5/4

    # Set summary to maximum size
    info[is.summary] <- 1/NCOL(org_mean)
  }

  prFpPrintLabels(labels = labels,
                  nc = nc,
                  nr = nr,
                  graph.pos = graph.pos)

  prFpDrawLines(hrzl_lines = hrzl_lines, nr = nr, colwidths = colwidths,
                graph.pos = graph.pos)


  prFpPrintXaxis(axisList=axisList,
                 col=col, lwd.zero=lwd.zero)

  # Output the different confidence intervals
  for (i in 1:nr) {
    if (is.matrix(org_mean)){
      low_values <- org_lower[i,]
      mean_values <- org_mean[i,]
      up_values <- org_upper[i,]
      info_values <- matrix(info, ncol=length(low_values))[i, ]
    }else{
      low_values <- org_lower[i]
      mean_values <- org_mean[i]
      up_values <- org_upper[i]
      info_values <- info[i]
    }

    # The line and box colors may vary
    clr.line <- rep(col$line, length.out=length(low_values))
    clr.marker <- rep(col$box, length.out=length(low_values))
    clr.summary <- rep(col$summary, length.out=length(low_values))

    line_vp <- viewport(layout.pos.row = i,
                        layout.pos.col = graph.pos * 2 - 1,
                        xscale = axisList$x_range,
                        name = sprintf("Line_%d_%d", i, graph.pos*2-1))
    pushViewport(line_vp)

    # Draw multiple confidence intervals
    if (length(low_values) > 1){
      b_height <- max(info_values)
      if (is.unit(b_height))
        b_height <- convertUnit(b_height, unitTo="npc", valueOnly=TRUE)

      if (missing(line.margin)){
        line.margin <- .1 + .2/(length(low_values) - 1)
      }else if (is.unit(line.margin)){
        line.margin <- convertUnit(line.margin, unitTo = "npc", valueOnly = TRUE)
      }
      y.offset_base <- b_height/2 + line.margin
      y.offset_increase <- (1 - line.margin*2 - b_height)/(length(low_values)-1)

      for(j in length(low_values):1){
        # Start from the bottom and plot up
        # the one on top should always be
        # above the one below
        current_y.offset <- y.offset_base + (length(low_values)-j)*y.offset_increase
        if (is.na(mean_values[j]))
          next;

        if (is.summary[i]){
          call_list <-
            list(fn.ci_sum[[i]][[j]],
                 lower_limit=low_values[j],
                 estimate=mean_values[j],
                 upper_limit=up_values[j],
                 size=info_values[j],
                 y.offset = current_y.offset,
                 col = clr.summary[j])
        }else{
          call_list <-
            list(fn.ci_norm[[i]][[j]],
                 lower_limit=low_values[j],
                 estimate=mean_values[j],
                 upper_limit=up_values[j],
                 size=info_values[j],
                 y.offset = current_y.offset,
                 clr.line = clr.line[j],
                 clr.marker = clr.marker[j],
                 lty = lty.ci[[i]][[j]],
                 vertices.height = ci.vertices.height)

          if (!missing(ci.vertices))
            call_list$vertices = ci.vertices;

          if (!missing(lwd.ci))
            call_list$lwd <- lwd.ci
        }


        # Add additional arguments that are passed on
        # from the original parameters
        if (length(list(...)) > 0){
          ll <- list(...)
          for (name in names(ll)){
            call_list[[name]] <- ll[[name]]
          }
        }

        # Do the actual drawing of the object
        eval(as.call(call_list))
      }
    }else{
      if (is.summary[i]){
        call_list <-
          list(fn.ci_sum[[i]],
               lower_limit=low_values,
               estimate=mean_values,
               upper_limit=up_values,
               size=info_values,
               col=clr.summary)
      }else{
        call_list <-
          list(fn.ci_norm[[i]],
               lower_limit=low_values,
               estimate=mean_values,
               upper_limit=up_values,
               size=info_values,
               clr.line = clr.line,
               clr.marker = clr.marker,
               lty = lty.ci[[i]],
               vertices.height = ci.vertices.height)

        if (!missing(ci.vertices))
          call_list$vertices = ci.vertices;

        if (!missing(lwd.ci))
          call_list$lwd <- lwd.ci
      }

      # Add additional arguments that are passed on
      # from the original parameters
      if (length(list(...)) > 0){
        ll <- list(...)
        for (name in names(ll)){
          call_list[[name]] <- ll[[name]]
        }
      }

      # Do the actual drawing of the object
      if (!is.na(mean_values))
        eval(as.call(call_list))
    }

    upViewport()
  }

  # Output the legend if it is inside the main plot
  if (!missing(legend) &&
      is.list(legend_args$pos)){
    plot_vp <- viewport(layout.pos.row = 1:nr,
                        layout.pos.col = 2 * graph.pos - 1,
                        name = "main_plot_area")
    pushViewport(plot_vp)

    if ("align" %in% names(legend_args$pos) && legend_args$pos[["align"]] == "horizontal"){
      # Calculated with padding above
      height <- legend_horizontal_height
      # Calculate the horizontal width by iterating througha all elements
      # as each element may have a different width
      width <- 0
      for (i in 1:length(lGrobs)){
        if (width > 0){
          width <- width + convertUnit(legend_colgap, unitTo="npc", valueOnly=TRUE)
        }
        width <- width + convertUnit(attr(lGrobs, "max_height") + legend_colgap + attr(lGrobs[[i]], "width"), unitTo="npc", valueOnly=TRUE)
      }
      # Add the padding
      width <- unit(width + convertUnit(legend_args$padding, unitTo="npc", valueOnly=TRUE)*2, "npc")
    }else{
      legend_height <- attr(lGrobs, "line_height_and_spacing")[rep(1:2, length.out=length(legend)*2-1)]
      if (!is.null(attr(lGrobs, "title"))){
        legend_height <- unit.c(attr(lGrobs, "titleHeight"),
                                attr(lGrobs, "line_height_and_spacing")[2], legend_height)
      }

      height <- sum(legend_args$padding, legend_height, legend_args$padding)
      width <- legend_vertical_width
    }
    pushViewport(viewport(x=legend_args$pos[["x"]],
                          y=legend_args$pos[["y"]],
                          width=width,
                          height=height,
                          just=legend_args$pos[["just"]]))
    # Draw the legend
    prFpDrawLegend(lGrobs = lGrobs,
                   col = col,
                   colgap = legend_colgap,
                   pos = legend_args$pos,
                   gp = legend_args$gp,
                   r = legend_args$r,
                   padding = legend_args$padding,
                   fn.legend = fn.legend,
                   ...)
    upViewport(2)
  }

  # Go back to the original viewport
  seekViewport("forestplot_margins")
  upViewport(2)
}
