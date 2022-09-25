#' Plots the x-axis for forestplot
#'
#' A helper function to the \code{\link{forestplot}}
#' function.
#'
#' @param axisList The list from \code{\link{prFpGetGraphTicksAndClips}}
#' @return void
#'
#' @inheritParams forestplot
#' @noRd
plot.forestplot_xaxis <- function(axisList) {
  # Now plot the axis inkluding the horizontal bar
  pushViewport(axisList$axis_vp)

  # Plot the vertical "zero" axis
  gp_list <- list(col = axisList$col$zero)
  if (!is.null(axisList$lwd.zero)) {
    gp_list$lwd <- axisList$lwd.zero
  }
  zero_gp <- prGetShapeGp(axisList$shapes_gp, NULL, "zero", default = do.call(gpar, gp_list))

  if (length(axisList$zero) > 1 || !is.na(axisList$zero)) {
    if (length(axisList$zero) == 1) {
      grid.lines(
        x = unit(axisList$zero, "native"),
        y = 0:1,
        gp = zero_gp
      )
    } else if (length(axisList$zero) == 2) {
      gp_list$fill <- gp_list$col
      grid.polygon(
        x = unit(
          c(
            axisList$zero,
            rev(axisList$zero)
          ),
          "native"
        ),
        y = c(0, 0, 1, 1),
        gp = zero_gp
      )
    }
  }

  if (is.grob(axisList$gridList)) {
    grid.draw(axisList$gridList)
  }

  lab_y <- unit(0, "mm")
  lab_grob_height <- unit(-2, "mm")
  # Omit the axis if specified as 0
  if (is.grob(axisList$axisGrob)) {
    # Plot the actual x-axis
    grid.draw(axisList$axisGrob)
    lab_grob_height <- grobHeight(axisList$axisGrob)
    lab_y <- lab_y - lab_grob_height
  }

  if (is.grob(axisList$labGrob)) {
    # Add some padding between text and ticks proportional to the ticks height
    padding <-
      unit(
        convertY(lab_grob_height, "lines", valueOnly = TRUE) * 0.1,
        "lines"
      )

    # The text is strangely messy
    # and needs its own viewport
    pushViewport(viewport(
      height = grobHeight(axisList$labGrob),
      y = lab_y - padding, just = "top"
    ))
    grid.draw(axisList$labGrob)
    upViewport()
  }
  upViewport()
}

