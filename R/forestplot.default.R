#' @rdname forestplot
#' @method forestplot default
#' @export
#' @importFrom checkmate assert_class assert_vector assert_matrix check_matrix check_array assert check_integer
forestplot.default <- function(labeltext,
                               mean, lower, upper,
                               align = NULL,
                               is.summary = FALSE,
                               graph.pos = "right",
                               hrzl_lines = NULL,
                               clip = c(-Inf, Inf),
                               xlab = NULL,
                               zero = ifelse(xlog, 1, 0),
                               graphwidth = "auto",
                               colgap = NULL,
                               lineheight = "auto",
                               line.margin = NULL,
                               col = fpColors(),
                               txt_gp = fpTxtGp(),
                               xlog = FALSE,
                               xticks = NULL,
                               xticks.digits = 2,
                               grid = FALSE,
                               lwd.xaxis = NULL,
                               lwd.zero = 1,
                               lwd.ci = NULL,
                               lty.ci = 1,
                               ci.vertices = NULL,
                               ci.vertices.height = .1,
                               boxsize = NULL,
                               mar = unit(rep(5, times = 4), "mm"),
                               title = NULL,
                               legend = NULL,
                               legend_args = fpLegend(),
                               new_page = getOption("forestplot_new_page", TRUE),
                               fn.ci_norm = fpDrawNormalCI,
                               fn.ci_sum = fpDrawSummaryCI,
                               fn.legend = NULL,
                               shapes_gp = fpShapesGp(),
                               ...) {
  if (is.null(colgap)) {
    colgap <- convertUnit(unit(6, "mm"), "npc", valueOnly = TRUE)
    if (colgap < .1) {
      colgap <- unit(.05, "npc")
    } else {
      colgap <- unit(colgap, "npc")
    }
  } else if (!grid::is.unit(colgap)) {
    colgap <- as.numeric(colgap)
    if (is.na(colgap)) {
      stop("Invalid colgap argument")
    }
  }
  colgap <- convertUnit(colgap, "mm")

  assert_class(txt_gp, "fpTxtGp")
  assert_class(col, "fpColors")
  assert_vector(zero, max.len = 2)

  coreData <- buildEstimateArray(labeltext, lower, upper, mean)
  rm(labeltext)
  if (!missing(mean)) {
    rm(lower, upper, mean)
  }

  if (dim(coreData$estimates)[3] != length(col$box)) {
    col$box <- rep(col$box, length.out = dim(coreData$estimates)[3])
    col$line <- rep(col$lines, length.out = dim(coreData$estimates)[3])
  }

  # Prepare the legend marker
  if (!is.null(legend)) {
    fn.legend <- prFpPrepareLegendMarker(
      fn.legend = fn.legend,
      col_no = dim(coreData$estimates)[3],
      row_no = nrow(coreData$estimates),
      fn.ci_norm = fn.ci_norm
    )
  }

  if (!is.unit(lineheight) && !lineheight %in% c("auto", "lines")) {
    stop(
      "The argument lineheight must either be of type unit or set to 'auto',",
      " you have provided a '", class(lineheight), "' class"
    )
  }

  if (!is.null(legend)) {
    if (length(legend) != dim(coreData$estimates)[3]) {
      stop(
        "If you want a legend you need to provide the same number of",
        " legend descriptors as you have boxes per line, currently you have ",
        dim(coreData$estimates)[3], " boxes and ",
        length(legend), " legends."
      )
    }
    if (is.list(legend_args$pos)) {
      legend_args$pos <- prFpGetLegendBoxPosition(legend_args$pos)
    } else if (!legend_args$pos %in% c("top", "right")) {
      stop(
        "The legend is either a list positioning it inside the main plot or at the 'top' or 'right' side,",
        " the position '", legend_args$pos, "' is not valid."
      )
    }

    if (inherits(legend_args$gp, "gpar")) {
      # Remove default border if no color
      # unless there is a line width or type specified
      if (!"col" %in% names(legend_args$gp)) {
        if (any(c("lwd", "lwd") %in% names(legend_args$gp))) {
          legend_args$gp[["col"]] <- "black"
        } else {
          legend_args$gp[["col"]] <- NA
        }
      }
    }
  }

  # Instantiate a new page - forced if no device exists
  if (new_page || dev.cur() == 1) grid.newpage()

  # Save the original values since the function due to it's inheritance
  # from the original forestplot needs some changing to the parameters
  if (xlog) {
    if (any(coreData$estimates < 0, na.rm = TRUE) ||
        (!is.null(clip) && any(Filter(Negate(is.infinite), clip) <= 0, na.rm = TRUE)) ||
        (!is.null(grid) && !isFALSE(grid) && any(grid <= 0, na.rm = TRUE))) {
      stop("All argument values (mean, lower, upper, zero, grid and clip)",
           " should be provided in exponential form when using the log scale.",
           " This is an intentional break with the original forestplot function in order",
           " to simplify other arguments such as ticks, clips, and more.")
    }

    # Change all the values along the log scale
    coreData$estimates <- log(coreData$estimates)
    clip[clip < 0] <- 0
    clip <- log(clip)
    zero <- log(zero)
  }

  # Prep basics
  labels <- prepLabelText(labeltext = coreData$labeltext,
                          nr = nrow(coreData$estimates))
  graph.pos <- prepGraphPositions(graph.pos, nc = attr(labels, "no_cols"))
  align <- prepAlign(align, graph.pos = graph.pos, nc = attr(labels, "no_cols"))

  is.summary <- rep(is.summary, length.out = nrow(coreData$estimates))
  missing_rows <- apply(coreData$estimates, 2, \(row) all(is.na(row)))

  fn.ci_norm <- prFpGetConfintFnList(fn = fn.ci_norm,
                                     no_rows = nrow(coreData$estimates),
                                     no_depth = dim(coreData$estimates)[3],
                                     missing_rows = missing_rows,
                                     is.summary = is.summary,
                                     summary = FALSE)
  fn.ci_sum <- prFpGetConfintFnList(fn = fn.ci_sum,
                                    no_rows = nrow(coreData$estimates),
                                    no_depth = dim(coreData$estimates)[3],
                                    missing_rows = missing_rows,
                                    is.summary = is.summary,
                                    summary = TRUE)
  lty.ci <- prPopulateList(lty.ci,
                           no_rows = nrow(coreData$estimates),
                           no_depth = dim(coreData$estimates)[3])

  list(labels = labels,
       estimates = coreData$estimates,
       mar = mar,
       align = align,
       title = title,
       legend = legend,
       legend_args = legend_args,
       txt_gp = txt_gp,
       colgap = colgap,
       lineheight = lineheight,
       col = col,
       graphwidth = graphwidth,
       graph.pos = graph.pos,
       boxsize = boxsize,
       is.summary = is.summary,
       shapes_gp = shapes_gp,
       hrzl_lines = hrzl_lines,
       line.margin = line.margin,
       fn.legend = fn.legend,
       fn.ci_sum = fn.ci_sum,
       fn.ci_norm = fn.ci_norm,
       lty.ci = lty.ci,
       ci.vertices.height = ci.vertices.height,
       ci.vertices = ci.vertices,
       lwd.zero = lwd.zero,
       lwd.ci = lwd.ci,
       xticks = xticks,
       xticks.digits = xticks.digits,
       xlab = xlab,
       xlog = xlog,
       clip = clip,
       zero = zero,
       lwd.xaxis = lwd.xaxis,
       extra_arguments = list(...)) |>
    structure(class = "gforge_forestplot")
}

#' @rdname forestplot
#' @param x The `gforge_forestplot` object to be printed
#' @export
print.gforge_forestplot <- function(x, ...) {
  extra_arguments <- list(...)
  for (n in names(extra_arguments)) {
    stopifnot(n != "")
    x[[n]] <- extra_arguments[[n]]
  }
  drawForestplotObject(x)
}

#' @rdname forestplot
#' @param y Ignored
#' @export
plot.gforge_forestplot <- function(x, y, ...) {
  print(x, ...)
}
