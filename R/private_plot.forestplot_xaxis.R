#' Plots the x-axis for forestplot
#'
#' A helper function to the \code{\link{forestplot}}
#' function.
#'
#' @param x The list from \code{\link{prFpGetGraphTicksAndClips}}
#' @param ... Unused
#' @return void
#'
#' @inheritParams forestplot
#' @noRd
plot.forestplot_xaxis <- function(x, ...) {
  # Now plot the axis inkluding the horizontal bar
  pushViewport(x$axis_vp)

  # Plot the vertical "zero" axis
  gp_list <- list(col = x$col$zero)
  if (!is.null(x$lwd.zero)) {
    gp_list$lwd <- x$lwd.zero
  }
  zero_gp <- prGetShapeGp(x$shapes_gp, NULL, "zero", default = do.call(gpar, gp_list))

  if (length(x$zero) > 1 || !is.na(x$zero)) {
    if (length(x$zero) == 1) {
      grid.lines(
        x = unit(x$zero, "native"),
        y = 0:1,
        gp = zero_gp
      )
    } else if (length(x$zero) == 2) {
      gp_list$fill <- gp_list$col
      grid.polygon(
        x = unit(
          c(
            x$zero,
            rev(x$zero)
          ),
          "native"
        ),
        y = c(0, 0, 1, 1),
        gp = zero_gp
      )
    }
  }

  if (is.grob(x$gridList)) {
    grid.draw(x$gridList)
  }

  lab_y <- unit(0, "mm")
  lab_grob_height <- unit(-2, "mm")
  # Omit the axis if specified as 0
  if (is.grob(x$axisGrob)) {
    # Plot the actual x-axis
    grid.draw(x$axisGrob)
    lab_grob_height <- grobHeight(x$axisGrob)
    lab_y <- lab_y - lab_grob_height
  }

  if (is.grob(x$labGrob)) {
    # Add some padding between text and ticks proportional to the ticks height
    padding <-
      unit(
        convertY(lab_grob_height, "lines", valueOnly = TRUE) * 0.1,
        "lines"
      )

    # The text is strangely messy
    # and needs its own viewport
    pushViewport(viewport(
      height = grobHeight(x$labGrob),
      y = lab_y - padding, just = "top"
    ))
    grid.draw(x$labGrob)
    upViewport()
  }
  upViewport()
}

