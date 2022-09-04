#' Draw the forestplot legend
#'
#' Takes the grobs and outputs the legend
#' inside the current viewport.
#'
#' @param lGrobs A list with all the grobs, see \code{\link{prFpGetLegendGrobs}}
#' @param col The colors of the legends.
#' @param fn.legend The function for drawing the marker
#' @param ... Passed to the legend \code{fn.legend}
#' @return \code{void}
#'
#' @inheritParams forestplot
#' @inheritParams fpLegend
#'
#' @noRd
prFpDrawLegend <- function(lGrobs,
                           col,
                           fn.legend,
                           ...) {
  if (!inherits(lGrobs, "forestplot_legend")) {
    stop("The lGrobs object should be created by the internal Gmisc:::buildLegend and be of class 'forestplot_legend'.")
  }

  # Draw the rounded rectangle at first
  # if there is a gpar specified.
  decorateWithRoundRect <- length(attr(lGrobs, "gp")) > 0
  if (decorateWithRoundRect) {
    grid.roundrect(gp = attr(lGrobs, "gp"), r = r)
    viewport(
      width = unit(1, "npc") - padding - padding,
      height = unit(1, "npc") - padding - padding
    ) |>
      pushViewport()
  }

  pos <- attr(lGrobs, "pos")
  if ((!inherits(pos, "forestplot_legend_position") &&
       !is.list(pos) &&
       pos == "top") ||
      (!inherits(pos, "forestplot_legend_position") &&
       is.list(pos) &&
       "align" %in% names(pos) &&
       pos[["align"]] == "horizontal")) {
    orientation <- "horizontal"
  } else {
    orientation <- "vertical"
  }

  boxSize <- attr(lGrobs, "max_height")

  drawBox <- function(vp, i, col, lGrobs) {
    pushViewport(vp)

    shape_coordinates <- c(1, i)
    attr(shape_coordinates, "max.coords") <- c(1, length(lGrobs))

    call_list <-
      list(fn.legend[[i]],
           lower_limit = 0,
           estimate = .5,
           upper_limit = 1,
           size = attr(lGrobs, "max_height"),
           y.offset = .5,
           clr.marker = col$box[i],
           clr.line = col$lines[i],
           shapes_gp = attr(lGrobs, "shapes_gp"),
           shape_coordinates = shape_coordinates,
           lwd = 1,
           ... = ...
      )

    # Do the actual drawing of the object
    eval(as.call(call_list))

    upViewport()
  }

  colgap <- attr(lGrobs, "colgap")
  if (orientation == "horizontal") {
    # Output the horizontal boxes and texts
    widths <- NULL
    for (n in 1:length(lGrobs)) {
      if (length(widths) == 0) {
        widths <- unit.c(boxSize, colgap, attr(lGrobs[[n]], "width"))
      } else {
        widths <- unit.c(widths, colgap, boxSize, colgap, attr(lGrobs[[n]], "width"))
      }
    }
    heights <- attr(lGrobs, "max_height")
    # Add title height if any
    if (!is.null(attr(lGrobs, "title"))) {
      heights <- unit.c(
        attr(lGrobs, "titleHeight"),
        attr(lGrobs, "line_height_and_spacing")[2],
        heights
      )
    }

    l_layout <- grid.layout(
      nrow = length(heights),
      heights = heights,
      ncol = length(widths),
      widths = widths
    )
    lvp <- viewport(
      layout = l_layout,
      name = "legend_details"
    )
    pushViewport(lvp)
    row <- 1
    # Output title
    if (!is.null(attr(lGrobs, "title"))) {
      vp <- viewport(layout.pos.row = 1)
      pushViewport(vp)
      pushViewport(viewport(width = attr(lGrobs, "titleWidth")))
      grid.draw(attr(lGrobs, "title"))
      upViewport(2)
      row <- 3
    }
    for (i in 1:length(lGrobs)) {
      offset <- 4 * (i - 1)
      vp <- viewport(
        layout.pos.row = row,
        layout.pos.col = 1 + offset,
        xscale = c(0, 1)
      )
      drawBox(vp, i, col, lGrobs)
      vp <- viewport(
        layout.pos.row = row,
        layout.pos.col = 3 + offset
      )
      pushViewport(vp)
      grid.draw(lGrobs[[i]])
      upViewport()
    }
    upViewport()
  } else {
    # Output the vertical boxes and texts
    widths <- unit.c(boxSize, colgap, attr(lGrobs, "max_width"))

    # Remove bottom line
    heights <- attr(lGrobs, "line_height_and_spacing")[rep(1:2, length.out = length(lGrobs) * 2 - 1)]
    # heights <- unit(convertUnit(heights, unitTo = "npc", valueOnly = TRUE)/sum(convertUnit(heights, unitTo = "npc", valueOnly = TRUE), "npc")
    # Add title height if any
    if (!is.null(attr(lGrobs, "title"))) {
      heights <- unit.c(
        attr(lGrobs, "titleHeight"),
        attr(lGrobs, "line_height_and_spacing")[2],
        heights
      )
    }

    l_layout <- grid.layout(
      ncol = length(widths),
      nrow = length(heights),
      widths = widths,
      heights = heights
    )

    lvp <- viewport(
      layout = l_layout, just = "left", x = 0,
      name = "legend"
    )
    pushViewport(lvp)
    row_start <- 1
    # Output title
    if (!is.null(attr(lGrobs, "title"))) {
      vp <- viewport(layout.pos.row = 1)
      pushViewport(vp)
      grid.draw(attr(lGrobs, "title"))
      upViewport()
      row_start <- 3
    }

    for (i in 1:length(lGrobs)) {
      vp <- viewport(
        layout.pos.row = row_start + (i - 1) * 2,
        layout.pos.col = 1,
        xscale = c(0, 1)
      )
      drawBox(vp, i, col, lGrobs)

      vp <- viewport(
        layout.pos.row = row_start + (i - 1) * 2,
        layout.pos.col = 3
      )
      pushViewport(vp)
      grid.draw(lGrobs[[i]])
      upViewport()
    }
    upViewport()
  }

  if (decorateWithRoundRect) {
    upViewport()
  }
}
