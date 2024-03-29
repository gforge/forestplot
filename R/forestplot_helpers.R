#' Draw standard confidence intervals
#'
#' A function that is used to draw the different
#' confidence intervals for the non-summary lines.
#' Use the \code{fpDrawNormalCI} function as a
#' template if you want to make your own funky line + marker.
#'
#' @param lower_limit The lower limit of the confidence line.
#'  A native numeric variable that can actually be
#'  outside the boundaries. If you want to see if it
#'  is outside then convert it to 'npc' and see if the
#'  value ends up more than 1 or less than 0. Here's how
#'  you do the conversion:
#'  \code{convertX(unit(upper_limit, "native"), "npc", valueOnly = TRUE)}
#'  and the \code{\link[grid]{convertX}} together with \code{\link[grid]{unit}}
#'  is needed to get the right values while you need to provide the valueOnly
#'  as you cannot compare a unit object.
#' @param estimate The estimate indicating the placement
#'  of the actual box. Note, this can also be outside bounds
#'  and is provided in a numeric format the same way as the
#'  \code{lower_limit}.
#' @param upper_limit The upper limit of the confidence line. See
#'  lower_limit for details.
#' @param size The actual size of the box/diamond/marker.
#'  This provided in the 'snpc' format to generate a perfect
#'  marker. Although you can provide it alternative units as well,
#'  this is useful for the legends to work nicely.
#' @param y.offset If you have multiple lines they need an offset in
#'  the y-direction.
#' @param clr.line The color of the line.
#' @param clr.marker The color of the estimate marker
#' @param lwd Line width, see \code{\link[grid]{gpar}}
#' @param lty Line type, see \code{\link[grid]{gpar}}
#' @param vertices Set this to TRUE if you want the ends of the confidence
#'  intervals to be shaped as a T. This is set default to TRUE if you have
#'  any other line type than 1 since there is a risk of a dash occurring
#'  at the very end, i.e. showing incorrectly narrow confidence interval.
#' @param vertices.height The height hoft the vertices. Defaults to npc units
#'  corresponding to 10% of the row height.
#' @param ... Allows additional parameters for sibling functions
#' @return \code{void} The function outputs the line using grid compatible
#'  functions and does not return anything.
#' @param shapes_gp A set of graphical parameters of class \code{\link{fpShapesGp}}
#' @param shape_coordinates A vector of length 2 the label (first item of the vector)
#' and the band (second item of the vector) of the confidence interval.
#' This is used together with shapes_gp to retrieve graphical parameters for that item.
#'
#' @author Max Gordon, Thomas Lumley
#' @example inst/examples/forestplot_alt_ci_example.R
#' @rdname fpDrawCI
#' @export
#' @family forestplot functions
fpDrawNormalCI <- function(lower_limit,
                           estimate,
                           upper_limit,
                           size,
                           y.offset = 0.5,
                           clr.line, clr.marker,
                           lwd,
                           lty = 1,
                           vertices,
                           vertices.height = .1,
                           shapes_gp = fpShapesGp(),
                           shape_coordinates = structure(c(1, 1), max.coords = c(1, 1)),
                           ...) {
  if (is.na(lower_limit) ||
    is.na(estimate) ||
    is.na(upper_limit)) {
    return()
  }

  # Funciton for drawing the confidence line
  prFpDrawLine(
    lower_limit = lower_limit,
    upper_limit = upper_limit,
    line_gp = prGetShapeGp(shapes_gp, shape_coordinates, "lines", default = prDefaultGp(col = clr.line, lwd = lwd, lty = lty)),
    vertices_gp = prGetShapeGp(shapes_gp, shape_coordinates, "vertices", nodefault = TRUE),
    y.offset = y.offset,
    vertices = vertices,
    vertices.height = vertices.height
  )

  # If the box is outside the plot the it shouldn't be plotted
  box <- convertX(unit(estimate, "native"), "npc", valueOnly = TRUE)
  if (is.na(box)) {
    stop("The estimate '", estimate, "' does not convert into a valid numeric value using grid::convertX")
  }
  skipbox <- box < 0 || box > 1

  # Lastly draw the box if it is still there
  if (!skipbox) {
    # Convert size into 'snpc'
    if (!is.unit(size)) {
      size <- unit(size, "snpc")
    }

    # Draw the actual box
    gp <- prGetShapeGp(
      shapes_gp = shapes_gp,
      coords = shape_coordinates,
      object = "box",
      default = gpar(fill = clr.marker, col = clr.marker)
    )
    grid.rect(
      x = unit(estimate, "native"),
      y = y.offset,
      width = size,
      height = size,
      gp = gp
    )
  }
}

#' Construct default parameters from arguments that may include missing arguments
#'
#' @param col Line color (or missing)
#' @param lwd Line width (or missing)
#' @param lty Line type (or missing)
#'
#' @return a \code{\link[grid]{gpar}} object
#'  containing these three attributes
prDefaultGp <- function(col, lwd, lty) {
  ret <- list()
  if (!missing(col)) {
    ret$col <- col
  }
  if (!missing(lwd)) {
    ret$lwd <- lwd
  }
  if (!missing(lty)) {
    ret$lty <- lty
  }
  return(do.call(grid::gpar, ret))
}

#' Draws a straight line
#'
#' If the line is outside the boundaries the line
#' is clipped with an arrow at the limit indicating
#' that it continues. If the lower limit is not below
#' the upper limit the line is not drawn.
#'
#' @inheritParams fpDrawNormalCI
#' @param clr.line Legacy color of line (please, use line_gp)
#' @param lwd Legacy width of line (please, use line_gp)
#' @param lty Legacy type of line (please, use line_gp)
#' @param line_gp A \code{\link[grid]{gpar}} for drawing the horizontal line
#' @param vertices_gp A \code{\link[grid]{gpar}} for drawing the vertices.
#'  unspecified attributes in vertices_gp default to line_gp.
#' @keywords internal
#' @importFrom grid gpar
#' @return \code{void}
prFpDrawLine <- function(lower_limit, upper_limit, clr.line, lwd, lty, y.offset,
                         vertices, vertices.height = .1, line_gp, vertices_gp) {

  # clr.line, lwd and lty are obsolete but are maintained for backward compatibility
  line_gp0 <- prDefaultGp(col = clr.line, lwd = lwd, lty = lty)

  if (missing(line_gp) & missing(vertices_gp)) {
    line_gp <- line_gp0
    vertices_gp <- line_gp0
  } else {
    line_gp <- prMergeGp(line_gp0, line_gp)
  }

  # Draw the lines if the lower limit is
  # actually below the upper limit
  if (lower_limit >= upper_limit) {
    return()
  }

  if (any(vertices.height < 0)) {
    stop("The vertices height cannot be negative")
  }

  if (inherits(vertices.height, "unit")) {
    vertices.height <- convertY(vertices.height,
      unitTo = "npc",
      valueOnly = TRUE
    )
  }

  if (!inherits(y.offset, "unit")) {
    y.offset <- unit(y.offset, "npc")
  }

  # If the limit is outside the 0-1 range in npc-units
  # then that part is outside the box and it should
  # be clipped (this function adds an arrow to the end
  # of the line)
  clipupper <-
    convertX(unit(upper_limit, "native"),
      "npc",
      valueOnly = TRUE
    ) > 1
  cliplower <-
    convertX(unit(lower_limit, "native"),
      "npc",
      valueOnly = TRUE
    ) < 0

  gp_list <- line_gp
  if (is.null(gp_list$lty)) {
    gp_list$lty <- 1
  }

  grid_line_args <- list(
    y = y.offset,
    gp = do.call(gpar, gp_list)
  )
  verticals <- "none"
  if (clipupper || cliplower) {
    # A version where arrows are added to the part outside
    # the limits of the graph
    ends <- "both"
    lims <- unit(c(0, 1), c("npc", "npc"))
    if (!clipupper) {
      ends <- "first"
      verticals <- "right"
      lims <- unit(c(0, upper_limit), c("npc", "native"))
    }
    if (!cliplower) {
      ends <- "last"
      verticals <- "left"
      lims <- unit(c(lower_limit, 1), c("native", "npc"))
    }
    grid_line_args$x <- lims
  } else {
    verticals <- "both"
    grid_line_args$x <- unit(c(lower_limit, upper_limit), "native")
  }

  do.call(grid.lines, grid_line_args)

  # The arrows should not have dashed line type
  # and it seems that the simples solution is just to do
  # an arrow of my own through the line-call
  if (clipupper || cliplower) {
    # Make arrow the same height the intended vertices
    # Old code: unit(0.05, "inches")
    radians <- 30 * pi / 180
    vertices.height_mm <- convertY(unit(vertices.height, "npc"),
      "mm",
      valueOnly = TRUE
    )
    arrow_length <- max(abs(vertices.height_mm)) / tan(radians)
    y_mm <- convertY(grid_line_args$y[1], "mm", valueOnly = TRUE)
    arrow_args <-
      list(
        y =
          unit(
            c(
              y_mm + vertices.height_mm,
              y_mm,
              y_mm - vertices.height_mm
            ),
            "mm"
          ) |>
            convertY("npc")
      )
    gp_list$lty <- 1
    arrow_args$gp <- do.call(gpar, gp_list)

    if (clipupper) {
      x <- max(grid_line_args$x)
      x <- unit.c(
        x - unit(arrow_length, "mm"),
        x,
        x - unit(arrow_length, "mm")
      ) |>
        convertX("npc")
      arrow_args$x <- x
      do.call(grid.lines, arrow_args)
    }

    if (cliplower) {
      x <- min(grid_line_args$x)
      x <- unit.c(
        x + unit(arrow_length, "mm"),
        x,
        x + unit(arrow_length, "mm")
      ) |>
        convertX("npc")
      arrow_args$x <- x
      do.call(grid.lines, arrow_args)
    }
  }

  if (missing(vertices)) {
    if (gp_list$lty != 1) {
      vertices <- TRUE
    } else {
      vertices <- FALSE
    }
  }

  if (vertices && verticals != "none") {
    if (length(vertices.height) == 1) {
      vertices.height <- c(vertices.height, -vertices.height)
    } else {
      vertices.height <- range(vertices.height)
    }
    y <- convertY(grid_line_args$y[1], "npc", valueOnly = TRUE)
    y_spread <- y + vertices.height
    gp_list$lty <- 1
    gp_vertices <- prMergeGp(gp_list, vertices_gp)
    if (verticals != "right") {
      grid.lines(
        x = rep(grid_line_args$x[1], 2),
        y = y_spread,
        gp = gp_vertices
      )
    }
    if (verticals != "left") {
      grid.lines(
        x = rep(grid_line_args$x[2], 2),
        y = y_spread,
        gp = gp_vertices
      )
    }
  }
}


#' @rdname fpDrawCI
#' @export
fpDrawDiamondCI <- function(lower_limit,
                            estimate,
                            upper_limit,
                            size,
                            y.offset = 0.5,
                            clr.line, clr.marker,
                            lwd,
                            lty = 1,
                            vertices,
                            vertices.height = .1,
                            shapes_gp = fpShapesGp(),
                            shape_coordinates = structure(c(1, 1), max.coords = c(1, 1)),
                            ...) {
  if (is.na(lower_limit) ||
    is.na(estimate) ||
    is.na(upper_limit)) {
    return()
  }

  # Funciton for drawing the confidence line
  prFpDrawLine(
    lower_limit = lower_limit,
    upper_limit = upper_limit,
    line_gp = prGetShapeGp(shapes_gp, shape_coordinates, "lines", default = prDefaultGp(col = clr.line, lwd = lwd, lty = lty)),
    vertices_gp = prGetShapeGp(shapes_gp, shape_coordinates, "vertices", nodefault = TRUE),
    y.offset = y.offset,
    vertices = vertices,
    vertices.height = vertices.height
  )

  # If the box is outside the plot the it shouldn't be plotted
  box <- convertX(unit(estimate, "native"), "npc", valueOnly = TRUE)
  if (box >= 0 &&
    box <= 1) {

    # Convert size if needed
    default.size.unit <- "snpc"
    if (is.unit(size)) {
      size <- convertUnit(size, unitTo = "mm", valueOnly = TRUE)
      default.size.unit <- "mm"
    }

    grid.polygon(
      x = unit(estimate, "native") +
        unit(c(-size / 2, 0, +size / 2, 0), default.size.unit),
      y = unit(y.offset, "npc") +
        unit(c(0, size / 2, 0, -size / 2), default.size.unit),
      gp = prGetShapeGp(shapes_gp, shape_coordinates, "box", default = gpar(fill = clr.marker, col = clr.marker))
    )
  }
}

#' @rdname fpDrawCI
#' @export
fpDrawCircleCI <- function(lower_limit,
                           estimate,
                           upper_limit,
                           size,
                           y.offset = 0.5,
                           clr.line, clr.marker,
                           lwd,
                           lty = 1,
                           vertices,
                           vertices.height = .1,
                           shapes_gp = fpShapesGp(),
                           shape_coordinates = structure(c(1, 1), max.coords = c(1, 1)),
                           ...) {
  if (is.na(lower_limit) ||
    is.na(estimate) ||
    is.na(upper_limit)) {
    return()
  }

  # Funciton for drawing the confidence line
  prFpDrawLine(
    lower_limit = lower_limit,
    upper_limit = upper_limit,
    line_gp = prGetShapeGp(shapes_gp, shape_coordinates, "lines", default = prDefaultGp(col = clr.line, lwd = lwd, lty = lty)),
    vertices_gp = prGetShapeGp(shapes_gp, shape_coordinates, "vertices", nodefault = TRUE),
    y.offset = y.offset,
    vertices = vertices,
    vertices.height = vertices.height
  )

  # If the box is outside the plot the it shouldn't be plotted
  box <- convertX(unit(estimate, "native"), "npc", valueOnly = TRUE)

  if (box >= 0 &&
    box <= 1) {
    # Convert size into 'mm' and switch to radius
    if (is.unit(size)) {
      size <- convertUnit(size, unitTo = "mm", valueOnly = TRUE)
      size <- unit(size / 2, "mm")
    } else {
      size <- unit(size / 2, "snpc")
    }

    grid.circle(
      x = unit(estimate, "native"),
      y = unit(y.offset, "npc"),
      r = size,
      gp = prGetShapeGp(shapes_gp, shape_coordinates, "box", default = gpar(fill = clr.marker, col = clr.marker))
    )
  }
}

#' @rdname fpDrawCI
#' @param pch Type of point see \code{\link[grid]{grid.points}} for details
#' @export
fpDrawPointCI <- function(lower_limit,
                          estimate,
                          upper_limit,
                          size,
                          y.offset = 0.5,
                          clr.line, clr.marker,
                          lwd,
                          lty = 1,
                          vertices,
                          vertices.height = .1,
                          pch = 1,
                          shapes_gp = fpShapesGp(),
                          shape_coordinates = structure(c(1, 1), max.coords = c(1, 1)),
                          ...) {
  if (is.na(lower_limit) ||
    is.na(estimate) ||
    is.na(upper_limit)) {
    return()
  }

  # Funciton for drawing the confidence line
  prFpDrawLine(
    lower_limit = lower_limit,
    upper_limit = upper_limit,
    line_gp = prGetShapeGp(shapes_gp, shape_coordinates, "lines", default = prDefaultGp(col = clr.line, lwd = lwd, lty = lty)),
    vertices_gp = prGetShapeGp(shapes_gp, shape_coordinates, "vertices", nodefault = TRUE),
    y.offset = y.offset,
    vertices = vertices,
    vertices.height = vertices.height
  )

  # If the box is outside the plot the it shouldn't be plotted
  box <- convertX(unit(estimate, "native"), "npc", valueOnly = TRUE)

  if (box >= 0 &&
    box <= 1) {
    # Convert size into 'snpc' if not given
    if (!is.unit(size)) {
      size <- unit(size, "snpc")
    }

    grid.points(
      x = unit(estimate, "native"),
      y = unit(y.offset, "npc"),
      size = size,
      pch = pch,
      gp = prGetShapeGp(shapes_gp, shape_coordinates, "box", default = gpar(fill = clr.marker, col = clr.marker))
    )
  }
}

#' @rdname fpDrawCI
#' @param col The color of the summary object
#' @export
fpDrawSummaryCI <- function(lower_limit, estimate, upper_limit,
                            size, col, y.offset = 0.5,
                            shapes_gp = fpShapesGp(),
                            shape_coordinates = structure(c(1, 1), max.coords = c(1, 1)),
                            ...) {
  if (is.na(lower_limit) ||
    is.na(estimate) ||
    is.na(upper_limit)) {
    return()
  }

  # Convert size into 'npc' value only if
  # it is provided as a unit() object
  size <- ifelse(is.unit(size),
    convertUnit(size, unitTo = "npc", valueOnly = TRUE),
    size
  ) * .9
  grid.polygon(
    x = unit(c(lower_limit, estimate, upper_limit, estimate), "native"),
    y = unit(y.offset +
      c(0, 0.5 * size, 0, -0.5 * size), "npc"),
    gp = prGetShapeGp(shapes_gp, shape_coordinates, "summary", default = gpar(fill = col, col = col))
  )
}

#' @rdname fpDrawCI
#' @export
fpDrawBarCI <- function(lower_limit, estimate, upper_limit, size, col, y.offset = 0.5,
                        shapes_gp = fpShapesGp(),
                        shape_coordinates = structure(c(1, 1), max.coords = c(1, 1)),
                        ...) {
  size <- ifelse(is.unit(size), convertUnit(size, unitTo = "npc", valueOnly = TRUE), size) * 0.9
  grid.polygon(
    x = unit(c(lower_limit, upper_limit, upper_limit, lower_limit), "native"),
    y = unit(y.offset + 0.5 * c(1, 1, -1, -1) * size, "npc"),
    gp = prGetShapeGp(shapes_gp, shape_coordinates, "summary", default = gpar(fill = col, col = col))
  )
}

#' A function for the color elements used in forestplot()
#'
#' This function encapsulates all the colors that are used in the
#' \code{\link{forestplot}} function. As there are plenty of color
#' options this function gathers them all in one place.
#'
#' Further customization of non-text elements can be performed with
#' \code{\link{fpShapesGp}} passed as \code{shapes_gp} parameter to
#' \code{\link{forestplot}}. The \code{fpColors} function is kept for
#' backwards compatibility.
#'
#' If you have several values per row in a forestplot you can set
#' a color to a vector where the first value represents the first
#' line/box, second the second line/box etc. The vectors are only
#' valid for the \code{box} & \code{lines} options.
#'
#' This function is a copy of the \code{\link[rmeta]{meta.colors}}
#' function in the \pkg{rmeta} package.
#'
#' @param all.elements A color for all the elements. If set to NULL then
#'  it's set to the par("fg") color
#' @param box The color of the box indicating the estimate
#' @param lines The color of the confidence lines
#' @param summary The color of the summary
#' @param zero The color of the zero line
#' @param text The color of the text
#' @param axes The color of the x-axis at the bottom
#' @param hrz_lines The color of the horizontal lines
#' @param vrtcl_lines The color of the vertical lines
#' @return A list with key elements
#'
#' @author Max Gordon, Thomas Lumley
#' @importFrom grDevices colorRampPalette
#' @importFrom graphics par
#'
#' @example inst/examples/fpColors_example.R
#' @export
#' @family forestplot functions
#'
fpColors <- function(all.elements,
                     box = "black",
                     lines = "gray",
                     summary = "black",
                     zero = "lightgray",
                     text = "black",
                     axes = "black",
                     hrz_lines = "black",
                     vrtcl_lines = "lightgray") {
  if (missing(all.elements)) {
    # Make sure the color lengths match
    # if nott then add a slightly lighter/darker shade
    if (length(box) > length(lines)) {
      nl <- length(lines)
      for (n in (nl + 1):length(box)) {
        lines <- append(
          lines,
          colorRampPalette(c(box[n], par("bg")))(10)[2]
        )
      }
    } else if (length(box) < length(lines)) {
      nl <- length(box)
      for (n in (nl + 1):length(lines)) {
        box <- append(
          box,
          colorRampPalette(c(lines[n], par("fg")))(10)[2]
        )
      }
    }

    if (length(summary) < length(box)) {
      summary <- rep(summary, length.out = length(box))
    }
    ret <- list(
      box = box,
      lines = lines,
      summary = summary,
      zero = zero,
      text = text,
      axes = axes,
      hrz_lines = hrz_lines,
      vrtcl_lines = vrtcl_lines
    )
  } else {
    if (is.null(all.elements)) {
      all.elements <- par("fg")
    }

    ret <- list(
      box = all.elements,
      lines = all.elements,
      summary = all.elements,
      zero = all.elements,
      text = all.elements,
      axes = all.elements,
      hrz_lines = all.elements,
      vrtcl_lines = all.elements
    )
  }


  ret |>
    structure(class = c("fpColors", class(ret)))
}

#' A function for graphical parameters of the shapes used in `forestplot()`
#'
#' This function encapsulates all the non-text elements that are used in the
#' [`forestplot()`] function. As there are plenty of shapes
#' options this function gathers them all in one place.
#'
#' This function obsoletes [`fpColors()`].
#'
#' If some, but not all parameters of a shape (e.g. box) are specified in gpar()
#' such as setting lwd but not line color, the unspecified parameters default
#' to the ones specified in `default`, then, default to legacy parameters
#' of `forestplot` such as `col`.
#'
#' Parameters `box`, `lines`, `vertices`, `summary` may be set as list
#' containing several gpars. The length of the list must either be equal to the number of bands
#' per label or to the number of bands multiplied by the number of labels, allowing specification
#' of different styles for different parts of the forest plot.
#'
#' The parameter `grid` can either be a single gpar or a list of gpars with as many
#' elements as there are lines in the grid (as set by the `xticks` or `grid`
#' arguments of forestplot)
#'
#' Parameters `zero`, `axes`, `hrz_lines` must either be NULL or gpar
#' but cannot be lists of gpars.
#'
#' @param default A fallback [grid::gpar] for all unspecified attributes.
#'  If set to NULL then it defaults to legacy parameters, including
#'  the `col`, `lwd.xaxis`, `lwd.ci` and `lty.ci`
#'  parameter of `fpColors`.
#' @param box The graphical parameters (`gpar`, `character`) of the box, circle
#'  or point indicating the point estimate, i.e. the middle
#'  of the confidence interval (may be a list of gpars). If provided
#'  a string a `gpar` will be generated with `col`, and `fill` for
#'  those arguments.
#' @param lines The graphical parameters (`gpar`, `character`) of the confidence lines
#'  (may be a list of gpars). If provided a string a `gpar` will be generated
#'   with `col` as the only arguments.
#' @param vertices The graphical parameters (`gpar`, `character`) of the vertices
#'  (may be a list of gpars).
#'  If `ci.vertices` is set to TRUE in `forestplot`
#'  `vertices` inherits from `lines` all its parameters but lty that is set
#'  to "solid" by default.
#' @param summary The graphical parameters (`gpar`, `character`) of the summary
#'  (may be a list of gpars). If provided a string a `gpar` will be generated with
#'  `col`, and `fill` for those arguments.
#' @param zero The graphical parameters (`gpar`) of the zero line
#'  (may not be a list of gpars). If provided a string a `gpar` will be generated
#'   with `col` as the only arguments.
#' @param axes The graphical parameters (`gpar`) of the x-axis at the bottom
#'  (may not be a list of gpars).
#' @param hrz_lines The graphical parameters (`gpar`) of the horizontal lines
#'  (may not be a list of gpars). If provided a string a `gpar` will be generated
#'  with `col` as the only arguments.
#' @param vrtcl_lines The graphical parameters (`gpar`) of the vertical lines
#'  (may not be a list of gpars). If provided a string a `gpar` will be generated
#'  with `col` as the only arguments.
#' @param grid The graphical parameters (`gpar`) of the grid (vertical lines)
#'  (may be a list of gpars). If provided a string a `gpar` will be generated
#'   with `col` as the only arguments.
#'
#' @return list A list with the elements:
#' * default: the gpar for default attributes
#' * box: the gpar or list of gpars of the box/marker
#' * lines: the gpar or list of gpars of the lines
#' * vertices: the gpar or list of gpars of the vertices
#' * summary: the gpar or list of gpars of the summary
#' * zero: the gpar of the zero vertical line
#' * axes: the gpar of the x-axis
#' * hrz_lines: the gpar of the horizontal lines
#' * grid: the gpar or list of gpars of the grid lines
#'
#' @author Andre GILLIBERT
#' @importFrom grid gpar
#' @importFrom graphics par
#'
#' @example inst/examples/fpShapesGp_example.R
#' @export
#' @family forestplot functions
fpShapesGp <- function(default = NULL,
                       box = NULL,
                       lines = NULL,
                       vertices = NULL,
                       summary = NULL,
                       zero = NULL,
                       axes = NULL,
                       hrz_lines = NULL,
                       vrtcl_lines = NULL,
                       grid = NULL) {
  ret <- list(
    default = default,
    box = box,
    lines = lines,
    vertices = vertices,
    summary = summary,
    zero = zero,
    axes = axes,
    hrz_lines = hrz_lines,
    vrtcl_lines = vrtcl_lines,
    grid = grid
  )

  for (clr_grp in c("box", "summary", "lines", "zero", "hrz_lines", "grid", "vertices")) {
    gpar_generator <- \(clr) gpar(col = clr)
    if (clr_grp %in% c("box", "summary")) {
      gpar_generator <- \(clr) gpar(col = clr, fill = clr)
    }

    if (is.character(ret[[clr_grp]])) {
      ret[[clr_grp]] <- sapply(ret[[clr_grp]],
                               FUN = gpar_generator,
                               USE.NAMES = TRUE,
                               simplify = FALSE)

      if (length(ret[[clr_grp]]) == 1) {
        ret[[clr_grp]] <- ret[[clr_grp]][[1]]
      }
    }
  }

  # check that objects have the correct type
  for (nm in names(ret)) {
    obj <- ret[[nm]]
    if (!is.null(obj) & !inherits(obj, "gpar")) {
      if (nm %in% c("default", "zero", "axes", "hrz_lines")) {
        stop("`", nm, "` must either be NULL or a gpar")
      }

      if (!is.list(obj)) {
        stop("`", nm, "` must either be NULL, a gpar or a list of gpars")
      }

      if (!all(sapply(obj, function(o) inherits(o, "gpar")))) {
        stop("`", nm, "` is not a proper list of gpars")
      }
    }
  }

  ret |>
    structure(class = c("fpShapesGp", class(ret)))
}

#' A function to extract graphical parameters from a fpShapesGp object
#'
#' @param shapes_gp An object of class \code{\link{fpShapesGp}} specifying all graphical parameters
#' @param coords A numeric vector of length 2, specifying the label number (first item of the vector)
#'  and the confidence band number within this label ; that can be >= 2 if there are multiple confidence
#'  bands per label. Can be NULL for objects that are used only once (e.g. axes).
#'  Vector \code{coords} must have an R attribute \code{max.coords} as numeric vector of length 2
#'  specifying the total number of labels
#'  and number of confidence bands by label for the forest plot.
#'  The first coordinate specify the label number and the second coordinate (for multi-band forest plots)
#'  specifies the band number within the label.
#' @param object One of \code{"box"}, \code{"lines"}, \code{"vertices"}, \code{"summary"}, \code{"zero"},
#'  \code{"axes"}, \code{"hrz_lines"} or \code{"grid"}, refering to the object for which the
#'  graphical parameters are requested.
#' @param default Default attributes to rely on when neither found in \code{shapes_gp$object}
#'  nor in \code{shapes_gp$default}
#' @param nodefault Logical. If TRUE, do not search attribute in shapes_gp$default
#'
#' @return An object of class \code{\link[grid]{gpar}}
#'
#' @author Andre GILLIBERT
#' @export
prGetShapeGp <- function(shapes_gp, coords, object, default = grid::gpar(), nodefault = FALSE) {
  # validation of parameters
  if (!inherits(shapes_gp, "fpShapesGp")) {
    stop("`shapes_gp` must be of class fpShapesGp")
  }

  if (!is.character(object) || !is.vector(object) || length(object) != 1) {
    stop("`object` must be a character vector of length")
  }
  if (!(object %in% c("box", "lines", "vertices", "summary", "zero", "axes", "hrz_lines", "vrtcl_lines", "grid"))) {
    stop("`object` must be one of: \"box\", \"lines\", \"vertices\", \"summary\", \"zero\", \"axes\", \"hrz_lines\", \"vrtcl_lines\", \"grid\"")
  }

  undefined_coords <- is.null(coords)

  if (!(is.null(coords) || is.numeric(coords) & length(coords) == 2)) {
    stop(paste0("`coords` must be NULL or a numeric vector of length 2 rather than a ", class(coords)))
  }
  if (!is.null(coords)) {
    if (is.null(attr(coords, "max.coords"))) {
      stop("`coords` must have a `max.coords` attribute")
    }
    max.coords <- attr(coords, "max.coords")
    if (!is.numeric(max.coords) | length(max.coords) != 2) {
      stop("`max.coords` attribute must be a numeric vector of length 2")
    }
    if (!all(coords <= max.coords & coords >= 1)) {
      stop(paste0("`coords` (", paste(coords, collapse = ", "), ")  out of range of `max.coords` (", paste(max.coords, collapse = ", "), ")"))
    }
  } else {
    max.coords <- c(1, 1)
    coords <- c(1, 1)
  }

  if (nodefault) {
    gp <- default
  } else {
    gp <- prMergeGp(default, shapes_gp[["default"]])
  }
  if (!is.null(shapes_gp[[object]])) {
    # override.
    shps <- shapes_gp[[object]]
    if (inherits(shps, "gpar")) {
      gp <- prMergeGp(gp, shps)
    } else if (!is.list(shps)) {
      stop(paste0("`shapes_gp$", object, "` must be a gpar or a list of gpars"))
    } else if (length(shps) == max.coords[2]) { # list of gpars...
      shp <- shps[[coords[2]]]
      gp <- prMergeGp(gp, shp)
    } else if (length(shps) == max.coords[1] * max.coords[2]) {
      shp <- shps[[(coords[1] - 1) * max.coords[2] + coords[2]]]
      gp <- prMergeGp(gp, shp)
    } else {
      if (undefined_coords) {
        stop(paste0("shape_gp$", object, " should be a gpar"))
      } else if (object == "grid") {
        stop(paste0("length of shape_gp$grid (", length(shps), ") must either be equal to 1 or to the number of grid elements specified in `xticks` or `grid` parameter of forestplot"))
      } else {
        stop(paste0(
          "length of shapes_gp$", object, " (", length(shps), ") should either be equal to number",
          " of bands per label (", max.coords[2], "),\nor to the product of the number of labels and",
          " number of bands per label (", max.coords[1], "*", max.coords[2], " = ", max.coords[1] * max.coords[2], ")"
        ))
      }
    }
  }
  return(gp)
}

#' A function to merge two sets of graphical parameters
#'
#' @param weak A \code{\link[grid]{gpar}}
#' @param strong Another \code{\link[grid]{gpar}}, with parameters taking precedence
#' over \code{weak}
#' @return A \code{\link[grid]{gpar}} merging attributes of both \code{weak} and \code{strong}
#
prMergeGp <- function(weak = gpar(), strong = gpar()) {
  ret <- weak
  for (nm in names(strong)) {
    ret[[nm]] <- strong[[nm]]
  }
  return(ret)
}


#' A function for the legend used in forestplot()
#'
#' This function encapsulates all the legend options that are used in the
#' \code{\link{forestplot}} function. This is in order to limit the crowding
#' among the arguments for the \code{\link{forestplot}} call.
#'
#' @param pos The position of the legend, either at the "top" or the "right" unless
#'  positioned inside the plot. If you want the legend to be positioned inside the plot
#'  then you have to provide a list with the same x & y qualities as \code{\link[graphics]{legend}}.
#'  For instance if you want the legend to be positioned at the top right corner then
#'  use \code{pos = list("topright")} - this is equivalent to \code{pos = list(x = 1, y = 1)}.
#'  If you want to have a distance from the edge of the graph then add a inset to the list,
#'  e.g. \code{pos = list("topright", "inset" = .1)} - the inset should be either a \code{\link[grid]{unit}}
#'  element or a value between 0 and 1. The default is to have the boxes aligned vertical, if
#'  you want them to be in a line then you can specify the "align" option, e.g.
#'  \code{pos = list("topright", "inset" = .1, "align" = "horizontal")}
#' @param gp The \code{\link[grid]{gpar}} options for the legend. If you want
#'  the background color to be light grey then use \code{gp = gpar(fill = "lightgrey")}.
#'  If you want a border then set the col argument: \code{gp = gpar(fill = "lightgrey", col = "black")}.
#'  You can also use the lwd and lty argument as usual, \code{gp = gpar(lwd = 2, lty = 1)}, will result
#'  in a black border box of line type 1 and line width 2.
#' @param r The box can have rounded edges, check out \code{\link[grid]{grid.roundrect}}. The
#'  r option should be a \code{\link[grid]{unit}} object. This is by default \code{unit(0, "snpc")}
#'  but you can choose any value that you want. The \code{"snpc"} unit is the preferred option.
#' @param padding The padding for the legend box, only used if box is drawn. This is
#'  the distance from the border to the text/boxes of the legend.
#' @param title The title of the legend if any
#' @return \code{list} Returns a list with all the elements
#' @export
#' @family forestplot functions
fpLegend <- function(pos = "top",
                     gp = NULL,
                     r = unit(0, "snpc"),
                     padding = unit(ifelse(!is.null(gp), 3, 0), "mm"),
                     title = NULL) {
  return(list(
    pos = pos,
    gp = gp,
    r = r,
    padding = padding,
    title = title
  ))
}

#' Get font settings for forestplot
#'
#' This function generates all the \code{\link[grid]{gpar}()}
#' elements for the different text elements within the graph.
#' Elements not specified inherit their default settings from the
#' \code{label} argument.
#'
#' @section List arguments for \code{label}/\code{summary}:
#'
#' You can provide a \code{list} of elements for the \code{label}
#' and \code{summary} in order to specify separate elements. If you
#' provide a \code{list} in one dimension the \code{gpar} elements are assumed
#' to follow the columns. If you provide a \code{list} of 2 dimensions the
#' structure assumes is \code{list[[row]][[column]]} and the number of elements
#' should correspond to the number of labels for the \code{label} argument, i.e.
#' without the rows marked as summary elements. The same goes for \code{summary}
#' arguments.
#'
#' @param label The text labels (see details below)
#' @param summary The summary labels (see details below)
#' @param xlab The xlab text
#' @param title The plot title
#' @param ticks The ticks associated with the xlab
#' @param legend The legend text
#' @param legend.title The legend title
#' @param cex The font size
#' @return A list of the \code{fpTxtGp} class
#' @examples
#' fpTxtGp(label = gpar(fontfamily = "HersheySerif"))
#' @export
fpTxtGp <- function(label,
                    summary,
                    xlab,
                    title,
                    ticks,
                    legend,
                    legend.title,
                    cex = 1) {
  prGparMergeMultiLevel <- function(ret, element) {
    name <- deparse(substitute(element))
    if (!inherits(element, "gpar")) {
      if (inherits(element, "list") &&
        (inherits(element[[1]], "gpar") ||
          (inherits(element[[1]], "list") &&
            inherits(element[[1]][[1]], "gpar")))) {
        if (inherits(element[[1]], "gpar")) {
          ret <- lapply(element, function(x, l1) prGparMerge(l1, x), l1 = ret)
          attr(ret, "txt_dim") <- 1
          default_element <- ret[[1]]
        } else {
          el_len <- sapply(element, length, USE.NAMES = FALSE)
          if (any(el_len != el_len[1])) {
            stop(
              "It seems that you haven't provided a square list",
              " for '", name, "'",
              ", ie all rows have the same number of elements.",
              " Currently the list lengths are:",
              " '", paste(el_len, collapse = "', '"), "'"
            )
          }
          ret <- lapply(element, function(l) {
            lapply(l, function(x, l1) {
              prGparMerge(l1, x)
            }, l1 = ret)
          })
          attr(ret, "txt_dim") <- 2
          default_element <- ret[[1]][[1]]
        }
      } else {
        stop("You can only provide arguments from gpar() or a 1-2 dimensional list of gpars to the function")
      }
    } else {
      ret <- prGparMerge(
        ret,
        element
      )
      attr(ret, "txt_dim") <- 0
      default_element <- ret
    }
    attr(ret, "ref") <- default_element
    return(ret)
  }

  ret <- list()
  ret$label <- list(
    fontface = "plain",
    cex = cex
  )
  attr(ret$label, "ref") <- ret$label
  attr(ret$label, "txt_dim") <- 0

  if (!missing(label)) {
    ret$label <- prGparMergeMultiLevel(
      ret$label,
      label
    )
  }

  ret$summary <-
    prGparMerge(
      attr(ret$label, "ref"),
      list(
        fontface = "bold",
        cex = attr(ret$label, "ref")$cex * 1.1
      )
    )
  attr(ret$summary, "ref") <- ret$summary
  attr(ret$summary, "txt_dim") <- 0

  if (!missing(summary)) {
    ret$summary <- prGparMergeMultiLevel(
      ret$summary,
      summary
    )
  }

  ret$title <-
    prGparMerge(
      attr(ret$label, "ref"),
      list(
        fontface = "bold",
        cex = attr(ret$label, "ref")$cex * 1.2,
        just = "center"
      )
    )

  if (!missing(title)) {
    if (!inherits(title, "gpar")) {
      stop("You can only provide arguments from gpar() to the function")
    }
    ret$title <- prGparMerge(
      ret$title,
      title
    )
  }

  ret$xlab <-
    prGparMerge(
      attr(ret$label, "ref"),
      list(cex = attr(ret$label, "ref")$cex * 0.6)
    )

  if (!missing(xlab)) {
    if (!inherits(xlab, "gpar")) {
      stop("You can only provide arguments from gpar() to the function")
    }
    ret$xlab <- prGparMerge(
      ret$xlab,
      xlab
    )
  }

  ret$ticks <-
    prGparMerge(
      attr(ret$label, "ref"),
      list(cex = attr(ret$label, "ref")$cex * 0.5)
    )

  if (!missing(ticks)) {
    if (!inherits(ticks, "gpar")) {
      stop("You can only provide arguments from gpar() to the function")
    }
    ret$ticks <- prGparMerge(
      ret$ticks,
      ticks
    )
  }

  ret$legend <-
    prGparMerge(
      attr(ret$label, "ref"),
      list(cex = attr(ret$label, "ref")$cex * 0.8)
    )
  attr(ret$legend, "ref") <- ret$legend
  attr(ret$legend, "txt_dim") <- 0

  if (!missing(legend)) {
    if (!inherits(legend, "gpar")) {
      stop("You can only provide arguments from gpar() to the function")
    }

    ret$legend <- prGparMergeMultiLevel(
      ret$legend,
      legend
    )
  }

  ret$legend.title <-
    prGparMerge(
      attr(ret$label, "ref"),
      list(
        fontface = "bold",
        cex = attr(ret$label, "ref")$cex * 1.1
      )
    )

  if (!missing(legend.title)) {
    if (!inherits(legend.title, "gpar")) {
      stop("You can only provide arguments from gpar() to the function")
    }
    ret$legend.title <- prGparMerge(
      ret$legend.title,
      legend.title
    )
  }

  return(structure(ret,
    class = c("fpTxtGp", class(ret))
  ))
}
