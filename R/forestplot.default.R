#' @rdname forestplot
#' @method forestplot default
#' @export
#' @importFrom checkmate assert_class assert_vector assert_matrix check_matrix check_array assert check_integer
forestplot.default <- function(labeltext,
                               mean, lower, upper,
                               align,
                               is.summary = FALSE,
                               graph.pos = "right",
                               hrzl_lines,
                               clip = c(-Inf, Inf),
                               xlab = "",
                               zero = ifelse(xlog, 1, 0),
                               graphwidth = "auto",
                               colgap,
                               lineheight = "auto",
                               line.margin,
                               col = fpColors(),
                               txt_gp = fpTxtGp(),
                               xlog = FALSE,
                               xticks,
                               xticks.digits = 2,
                               grid = FALSE,
                               lwd.xaxis,
                               lwd.zero,
                               lwd.ci,
                               lty.ci = 1,
                               ci.vertices,
                               ci.vertices.height = .1,
                               boxsize,
                               mar = unit(rep(5, times = 4), "mm"),
                               title,
                               legend,
                               legend_args = fpLegend(),
                               new_page = getOption("forestplot_new_page", TRUE),
                               fn.ci_norm = fpDrawNormalCI,
                               fn.ci_sum = fpDrawSummaryCI,
                               fn.legend,
                               shapes_gp = fpShapesGp(),
                               ...) {
  if (missing(colgap)) {
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

  if (missing(lower) &&
    missing(upper) &&
    missing(mean)) {
    if (missing(labeltext)) {
      stop(
        "You need to provide the labeltext or",
        " the mean/lower/upper arguments"
      )
    }

    mean <- labeltext
    labeltext <- rownames(mean)
  }

  if (missing(lower) &&
    missing(upper)) {
    assert(
      check_matrix(mean, ncols = 3),
      check_array(mean, d = 3),
      check_integer(dim(mean)[2], lower = 3, upper = 3)
    )
  }

  assert_vector(zero, max.len = 2)

  if (missing(labeltext)) {
    labeltext <- rownames(mean)
  }

  if (is.null(labeltext)) {
    stop(
      "You must provide labeltext either in the direct form as an argument",
      " or as rownames for the mean argument."
    )
  }
  # Assume that lower and upper are contained within
  # the mean variable
  if (missing(lower) &&
    missing(upper)) {
    if (NCOL(mean) != 3) {
      stop("If you do not provide lower/upper arguments your mean needs to have 3 columns")
    }

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
    NCOL(mean) == 0) {
    stop(
      "Mean, lower and upper contain invalid number of columns",
      " Mean columns:", ncol(mean),
      " Lower bound columns:", ncol(lower),
      " Upper bound columns:", ncol(upper)
    )
  }

  if (NCOL(mean) != length(col$box)) {
    col$box <- rep(col$box, length.out = NCOL(mean))
    col$line <- rep(col$lines, length.out = NCOL(mean))
  }

  # Prepare the legend marker
  if (!missing(legend)) {
    fn.legend <- prFpPrepareLegendMarker(
      fn.legend = fn.legend,
      col_no = NCOL(mean),
      row_no = NROW(mean),
      fn.ci_norm = fn.ci_norm
    )
  }

  if (!is.unit(lineheight) && !lineheight %in% c("auto", "lines")) {
    stop(
      "The argument lineheight must either be of type unit or set to 'auto',",
      " you have provided a '", class(lineheight), "' class"
    )
  }

  if (!missing(legend)) {
    if (length(legend) != ncol(mean)) {
      stop(
        "If you want a legend you need to provide the same number of",
        " legend descriptors as you have boxes per line, currently you have ",
        ncol(mean), " boxes and ",
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

  # Fix if data.frames were provided in the arguments
  if (is.data.frame(mean)) {
    mean <- as.matrix(mean)
  }
  if (is.data.frame(lower)) {
    lower <- as.matrix(lower)
  }
  if (is.data.frame(upper)) {
    upper <- as.matrix(upper)
  }

  # Instantiate a new page - forced if no device exists
  if (new_page || dev.cur() == 1) grid.newpage()

  # Save the original values since the function due to it's inheritance
  # from the original forestplot needs some changing to the parameters
  if (xlog) {
    if (any(mean < 0, na.rm = TRUE) ||
      any(lower < 0, na.rm = TRUE) ||
      any(upper < 0, na.rm = TRUE) ||
      (!is.na(zero) && zero <= 0) ||
      (!missing(clip) && any(clip <= 0, na.rm = TRUE)) ||
      (!missing(grid) && any(grid <= 0, na.rm = TRUE))) {
      stop(
        "All argument values (mean, lower, upper, zero, grid and clip)",
        " should be provided in exponential form when using the log scale.",
        " This is an intentional break with the original forestplot function in order",
        " to simplify other arguments such as ticks, clips, and more."
      )
    }

    # Change all the values along the log scale
    org_mean <- log(mean)
    org_lower <- log(lower)
    org_upper <- log(upper)
  } else {
    org_mean <- mean
    org_lower <- lower
    org_upper <- upper
  }

  # For border calculations etc it's
  # convenient to have the matrix as a
  # vector
  if (NCOL(mean) > 1) {
    mean <- as.vector(mean)
    lower <- as.vector(lower)
    upper <- as.vector(upper)
  }

  nr <- NROW(org_mean)

  # Get the number of columns (nc) and number of rows (nr)
  # if any columns are to be spacers the widthcolumn variable
  if (is.expression(labeltext)) {
    widthcolumn <- c(TRUE)
    # Can't figure out multiple levels of expressions
    nc <- 1
    label_type <- "expression"
    label_nr <- length(labeltext)
  } else if (is.list(labeltext)) {
    if (all(sapply(labeltext, function(x) {
      length(x) == 1 &&
        !is.list(x)
    }))) {
      labeltext <-
        list(labeltext)
    }
    if (!prFpValidateLabelList(labeltext)) {
      stop("Invalid labellist, it has to be formed as a matrix m x n elements")
    }

    # Can't figure out multiple levels of expressions
    nc <- length(labeltext)

    widthcolumn <- c()
    # Should mark the columns that don't contain
    # epressions, text or numbers as widthcolumns
    for (col.no in seq(along = labeltext)) {
      empty_row <- TRUE
      for (row.no in seq(along = labeltext[[col.no]])) {
        if (is.expression(labeltext[[col.no]][[row.no]]) ||
          !is.na(labeltext[[col.no]][[row.no]])) {
          empty_row <- FALSE
          break
        }
      }
      widthcolumn <- append(widthcolumn, empty_row)
    }

    label_type <- "list"
    label_nr <- length(labeltext[[1]])
  } else if (is.vector(labeltext)) {
    widthcolumn <- c(FALSE)
    nc <- 1

    labeltext <- matrix(labeltext, ncol = 1)
    label_type <- "matrix"
    label_nr <- NROW(labeltext)
  } else {
    # Original code for matrixes
    widthcolumn <- !apply(is.na(labeltext), 1, any)
    nc <- NCOL(labeltext)
    label_type <- "matrix"
    label_nr <- NROW(labeltext)
  }

  if (nr != label_nr) {
    stop(
      "You have provided ", nr, " rows in your",
      " mean arguement while the labels have ", label_nr, " rows"
    )
  }

  if (is.character(graph.pos)) {
    graph.pos <-
      switch(graph.pos,
        right = nc + 1,
        last = nc + 1,
        left = 1,
        first = 1,
        stop(
          "The graph.pos argument has an invalid text argument.",
          " The only values accepted are 'left'/'right' or 'first'/'last'.",
          " You have provided the value '", graph.pos, "'"
        )
      )
  } else if (is.numeric(graph.pos)) {
    if (!graph.pos %in% 1:(nc + 1)) {
      stop(
        "The graph position must be between 1 and ", (nc + 1), ".",
        " You have provided the value '", graph.pos, "'."
      )
    }
  } else {
    stop(
      "The graph pos must either be a string consisting of 'left'/'right' (alt. 'first'/'last')",
      ", or an integer value between 1 and ", (nc + 1)
    )
  }

  # Prepare the summary and align variables
  if (missing(align)) {
    if (graph.pos == 1) {
      align <- rep("l", nc)
    } else if (graph.pos == nc + 1) {
      align <- c("l", rep("r", nc - 1))
    } else {
      align <- c(
        "l",
        rep("c", nc - 1)
      )
    }
  } else {
    align <- rep(align, length.out = nc)
  }

  is.summary <- rep(is.summary, length = nr)

  if (is.matrix(mean)) {
    missing_rows <- apply(mean, 2, function(row) all(is.na(row)))
  } else {
    missing_rows <- sapply(mean, is.na)
  }

  fn.ci_norm <- prFpGetConfintFnList(
    fn = fn.ci_norm,
    no_rows = NROW(org_mean),
    no_cols = NCOL(org_mean),
    missing_rows = missing_rows,
    is.summary = is.summary,
    summary = FALSE
  )
  fn.ci_sum <- prFpGetConfintFnList(
    fn = fn.ci_sum,
    no_rows = NROW(org_mean),
    no_cols = NCOL(org_mean),
    missing_rows = missing_rows,
    is.summary = is.summary,
    summary = TRUE
  )

  lty.ci <- prPopulateList(lty.ci,
    no_rows = NROW(org_mean),
    no_cols = NCOL(org_mean)
  )


  hrzl_lines <- prFpGetLines(
    hrzl_lines = hrzl_lines,
    is.summary = is.summary,
    total_columns = nc + 1,
    col = col,
    shapes_gp = shapes_gp
  )

  labels <- prFpGetLabels(
    label_type = label_type,
    labeltext = labeltext,
    align = align,
    nc = nc,
    nr = nr,
    is.summary = is.summary,
    txt_gp = txt_gp,
    col = col
  )

  # There is always at least one column so grab the widest one
  # and have that as the base for the column widths
  colwidths <- unit.c(prFpFindWidestGrob(labels[[1]]))

  # If multiple row label columns, add the other column widths
  if (nc > 1) {
    for (i in 2:nc) {
      colwidths <- unit.c(
        colwidths,
        colgap,
        prFpFindWidestGrob(labels[[i]])
      )
    }
  }

  axisList <- prFpGetGraphTicksAndClips(
    xticks = xticks,
    xticks.digits = xticks.digits,
    grid = grid,
    xlog = xlog,
    xlab = xlab,
    lwd.xaxis = lwd.xaxis,
    txt_gp = txt_gp,
    col = col,
    clip = clip,
    zero = zero,
    x_range = prFpXrange(
      upper = upper,
      lower = lower,
      clip = clip,
      zero = zero,
      xticks = xticks,
      xlog = xlog
    ),
    mean = org_mean,
    graph.pos = graph.pos,
    shapes_gp = shapes_gp
  )

  handleMissing <- function(x, default = NA) {
    if (missing(x)) {
      return(default)
    }
    x
  }

  structure(list(
    labels = labels,
    mean = mean,
    upper = upper,
    lower = lower,
    mar = mar,
    title = handleMissing(title),
    legend = handleMissing(legend),
    legend_args = legend_args,
    txt_gp = txt_gp,
    colgap = colgap,
    lineheight = lineheight,
    nc = nc,
    nr = nr,
    col = col,
    graphwidth = graphwidth,
    colwidths = colwidths,
    graph.pos = graph.pos,
    axisList = axisList,
    boxsize = handleMissing(boxsize),
    is.summary = is.summary,
    org_mean = org_mean,
    hrzl_lines = hrzl_lines,
    shapes_gp = shapes_gp,
    org_lower = org_lower,
    org_upper = org_upper,
    line.margin = handleMissing(line.margin),
    fn.legend = handleMissing(fn.legend),
    fn.ci_sum = fn.ci_sum,
    fn.ci_norm = fn.ci_norm,
    lty.ci = lty.ci,
    ci.vertices.height = ci.vertices.height,
    ci.vertices = handleMissing(ci.vertices),
    lwd.zero = handleMissing(lwd.zero, default = 1),
    lwd.ci = handleMissing(lwd.ci),
    extra_arguments = list(...)
  ),
  class = "gforge_forestplot"
  )
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
