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
  bottom_y <- lab_y
  # Omit the axis if specified as 0
  if (is.grob(x$axisGrob)) {
    # Plot the actual x-axis
    grid.draw(x$axisGrob)
    lab_grob_height <- grobHeight(x$axisGrob)
    lab_y <- lab_y - lab_grob_height
    bottom_y <- lab_y
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

    bottom_y <- lab_y - padding - grobHeight(x$labGrob)
  }

  if (is.list(x$graph_favors)) {
    if (identical(x$graph_favors$position, "inside")) {
      favors_height <- unit(if (isTRUE(x$graph_favors$arrows)) 2 else 1.2, "line")
      pushViewport(viewport(
        height = favors_height,
        y = unit(0, "npc") + unit(0.1, "line"),
        just = "bottom",
        name = "favors_inside"
      ))
      plotGraphFavors(x$graph_favors, txt_gp = x$txt_gp$ticks, col = x$col$axes)
      upViewport()
    } else {
      favors_height <- unit(if (isTRUE(x$graph_favors$arrows)) 2.5 else 1.5, "line")
      favors_padding <- unit(0.2, "line")
      pushViewport(viewport(
        height = favors_height,
        y = bottom_y - favors_padding,
        just = "top",
        name = "favors_outside"
      ))
      plotGraphFavors(x$graph_favors, txt_gp = x$txt_gp$ticks, col = x$col$axes)
      upViewport()
    }
  }
  upViewport()
}

plotGraphFavors <- function(favors, txt_gp, col) {
  gp_list <- txt_gp
  gp_list$col <- col
  if (!is.null(favors$txt_gp)) {
    for (n in names(favors$txt_gp)) {
      gp_list[[n]] <- favors$txt_gp[[n]]
    }
  }

  txt_gp <- do.call(gpar, gp_list)

  left_head_x <- unit(0.02, "npc")
  right_head_x <- unit(0.98, "npc")

  cex <- txt_gp$cex
  if (is.null(cex) || !is.numeric(cex) || is.na(cex[1]) || cex[1] <= 0) {
    cex <- 1
  } else {
    cex <- cex[1]
  }

  arrow_y <- if (identical(favors$position, "outside")) {
    outside_arrow_y <- 0.72 - 0.18 * (cex - 0.6)
    unit(min(max(outside_arrow_y, 0.55), 0.8), "npc")
  } else {
    unit(0.2, "npc")
  }

  label_x_nudge <- favors$label_x_nudge
  if (is.null(label_x_nudge)) {
    base_x_nudge_mm <- 3
    x_slope_mm <- 1
    computed_x_nudge_mm <- base_x_nudge_mm + x_slope_mm * (cex - 0.5)
    label_x_nudge <- unit(max(1.5, computed_x_nudge_mm), "mm")
  }
  label_y_nudge <- favors$label_y_nudge
  if (is.null(label_y_nudge)) {
    base_y_nudge_line <- 0.3
    y_slope_line <- -0.2
    computed_y_nudge_line <- base_y_nudge_line + y_slope_line * (cex - 0.5)
    label_y_nudge <- unit(min(max(0.08, computed_y_nudge_line), 0.45), "line")
  }
  label_y <- if (isTRUE(favors$arrows)) {
    arrow_y + label_y_nudge
  } else {
    unit(0.35, "npc") + label_y_nudge
  }

  grid.text(
    favors$low,
    x = left_head_x + label_x_nudge,
    y = label_y,
    hjust = 0,
    vjust = 0,
    gp = txt_gp
  )
  grid.text(
    favors$high,
    x = right_head_x - label_x_nudge,
    y = label_y,
    hjust = 1,
    vjust = 0,
    gp = txt_gp
  )

  if (isTRUE(favors$arrows)) {
    arrow_gp <- list(col = txt_gp$col)
    if (!is.null(txt_gp$lwd)) {
      arrow_gp$lwd <- txt_gp$lwd
    }
    if (!is.null(favors$arrow_gp)) {
      for (n in names(favors$arrow_gp)) {
        arrow_gp[[n]] <- favors$arrow_gp[[n]]
      }
    }

    segments_gp <- do.call(gpar, arrow_gp)
    arrow_spec <- arrow(type = "open", ends = "last", length = unit(2.5, "mm"))

    grid.segments(
      x0 = unit(0.48, "npc"),
      y0 = arrow_y,
      x1 = left_head_x,
      y1 = arrow_y,
      gp = segments_gp,
      arrow = arrow_spec
    )
    grid.segments(
      x0 = unit(0.52, "npc"),
      y0 = arrow_y,
      x1 = right_head_x,
      y1 = arrow_y,
      gp = segments_gp,
      arrow = arrow_spec
    )
  }
}
