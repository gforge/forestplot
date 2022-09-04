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

  # Prep basics
  labels <- prepLabelText(labeltext = labeltext,
                          nr = NROW(org_mean))
  graph.pos <- prepGraphPositions(graph.pos, nc = attr(labels, "no_cols"))
  align <- prepAlign(align, graph.pos = graph.pos, nc = attr(labels, "no_cols"))
  is.summary <- rep(is.summary, length.out = attr(labels, "no_rows"))

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

  handleMissing <- function(x, default = NA) {
    if (missing(x)) {
      return(default)
    }
    x
  }

  list(labels = labels,
       mean = mean,
       upper = upper,
       lower = lower,
       mar = mar,
       align = align,
       title = handleMissing(title),
       legend = handleMissing(legend),
       legend_args = legend_args,
       txt_gp = txt_gp,
       colgap = colgap,
       lineheight = lineheight,
       col = col,
       graphwidth = graphwidth,
       graph.pos = graph.pos,
       boxsize = handleMissing(boxsize),
       is.summary = is.summary,
       org_mean = org_mean,
       shapes_gp = shapes_gp,
       hrzl_lines = handleMissing(hrzl_lines, default = NULL),
       org_lower = org_lower,
       org_upper = org_upper,
       line.margin = handleMissing(line.margin),
       fn.legend = fn.legend,
       fn.ci_sum = fn.ci_sum,
       fn.ci_norm = fn.ci_norm,
       lty.ci = lty.ci,
       ci.vertices.height = ci.vertices.height,
       ci.vertices = handleMissing(ci.vertices),
       lwd.zero = handleMissing(lwd.zero, default = 1),
       lwd.ci = handleMissing(lwd.ci),
       xticks = xticks,
       xticks.digits = xticks.digits,
       xlog = xlog,
       clip = clip,
       zero = zero,
       lwd.xaxis = handleMissing(lwd.xaxis),
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
