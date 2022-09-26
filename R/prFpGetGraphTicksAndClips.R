
#' A helper function to forestplot
#'
#' Gets the x-label and zero-bar details
#'
#' @param x_range The range that the values from the different confidence
#'  interval span
#' @param estimates The estimates as a 3D array
#' @return \code{list} Returns a list with axis_vp, axisGrob, labGrob, zero and clip
#'
#'
#' @inheritParams forestplot
#' @noRd
prFpGetGraphTicksAndClips <- function(xticks,
                                      xticks.digits,
                                      grid,
                                      xlog,
                                      xlab,
                                      lwd.xaxis,
                                      lwd.zero,
                                      col,
                                      txt_gp,
                                      clip,
                                      zero,
                                      x_range,
                                      estimates,
                                      graph.pos,
                                      shapes_gp = fpShapesGp()) {
  layoutRowSpan <- getActiveRowSpan(estimates)

  if (xlog) {
    if (is.null(xticks)) {
      ticks <- getTicks(exp(x_range),
                        clip = clip,
                        exp = xlog,
                        digits = xticks.digits)

      # Add the endpoint ticks to the tick list if
      # it's not already there
      if (is.infinite(clip[1]) == FALSE &&
          min(ticks, na.rm = TRUE) < clip[1]) {
        ticks <- unique(c(exp(clip[1]), ticks))
      }

      if (is.infinite(clip[2]) == FALSE &&
          max(ticks, na.rm = TRUE) > clip[2]) {
        ticks <- unique(c(ticks, exp(clip[2])))
      }

      # Update the range so that it includes the ticks
      if (min(x_range) > log(min(ticks))) {
        x_range[which.min(x_range)] <- log(min(ticks))
      }
      if (max(x_range) < log(max(ticks))) {
        x_range[which.max(x_range)] <- log(max(ticks))
      }
    } else {
      ticks <- xticks
    }

    axis_vp <- viewport(layout.pos.col = graph.pos * 2 - 1,
                        layout.pos.row = layoutRowSpan,
                        xscale = x_range,
                        name = "axis")



    # Draw the x-axis if there are any ticks
    if (length(ticks)) {

      # Decide on the number of digits, if below zero then there should
      # be by default one more digit
      ticklabels <- ifelse(ticks < 1 | abs(floor(ticks * 10) - ticks * 10) > 0,
                           format(ticks, digits = 2, nsmall = 2),
                           format(ticks, digits = 1, nsmall = 1))
      ticks <- log(ticks)
    } else {
      ticks <- NULL
      ticklabels <- FALSE
    }
  } else {
    if (is.null(xticks)) {
      ticks <- getTicks(x_range,
                        clip = clip,
                        exp = xlog,
                        digits = xticks.digits
      )

      # Add the endpoint ticks to the tick list if
      # it's not already there
      if (is.infinite(clip[1]) == FALSE &&
          min(ticks, na.rm = TRUE) < clip[1]) {
        ticks <- unique(c(clip[1], ticks))
      }

      if (is.infinite(clip[2]) == FALSE &&
          max(ticks, na.rm = TRUE) > clip[2]) {
        ticks <- unique(c(ticks, clip[2]))
      }

      ticklabels <- TRUE

      # Update the range so that it includes the ticks
      if (min(x_range) > min(ticks)) {
        x_range[which.min(x_range)] <- min(ticks)
      }
      if (max(x_range) < max(ticks)) {
        x_range[which.max(x_range)] <- max(ticks)
      }
    } else {
      ticks <- xticks
      ticklabels <- TRUE
    }

    axis_vp <- viewport(layout.pos.col = 2 * graph.pos - 1,
                        layout.pos.row = layoutRowSpan,
                        xscale = x_range,
                        name = "axis")
  }

  # Clean
  if (any(ticks < .Machine$double.eps &
          ticks > -.Machine$double.eps)) {
    ticks[ticks < .Machine$double.eps &
            ticks > -.Machine$double.eps] <- 0
  }


  # Prepare grid gpar option
  grid_gp <- gpar(lty = 2, col = "#DDDDDD")
  if (inherits(grid, "gpar")) {
    grid_gp <- grid
    grid <- TRUE
  } else if (inherits(attr(grid, "gp"), "gpar")) {
    grid_gp <- attr(grid, "gp")
  }

  if (length(ticks) != 1 || ticks != 0) {
    gp_list <- txt_gp$ticks
    gp_list$col <- col$axes
    if (!is.null(lwd.xaxis)) {
      gp_list$lwd <- lwd.xaxis
    }
    gp_axis <- prGetShapeGp(shapes_gp, NULL, "axes", default = do.call(grid::gpar, gp_list))

    if (!is.null(xticks) &&
        !is.null(attr(xticks, "labels"))) {
      labattr <- attr(xticks, "labels")
      if (length(labattr) != length(ticks)) {
        stop(
          "You want to specify the tick labels but you have provided",
          " '", length(labattr), "' labels while there are",
          " '", length(labattr), "' ticks after processing xticks.",
          " They should be identical."
        )
      }
      if (length(ticklabels) == 1) {
        ticklabels <- ticks
      }
      if (all(is.logical(labattr))) {
        ticklabels[!labattr] <- ""
      } else {
        ticklabels <- labattr
      }
    }
    dg <- xaxisGrob(at = ticks,
                    label = ticklabels,
                    gp = gp_axis)
    if (length(grid) == 1) {
      if (is.logical(grid) &&
          grid == TRUE) {
        grid <- ticks
      }
    }
  } else {
    dg <- FALSE
  }

  gridList <- NULL
  if (any(grid != FALSE)) {
    # Actually identical to the ticks viewport
    grid_vp <- viewport(
      layout.pos.col = 2 * graph.pos - 1,
      layout.pos.row = layoutRowSpan,
      xscale = x_range,
      name = "grid_vp"
    )
    gridList <- gTree()
    for (ipos in 1:length(grid)) {
      xpos <- grid[ipos]
      if (inherits(xpos, "unit")) {
        xpos <- convertX(xpos, unitTo = "native", valueOnly = TRUE)
      }
      coords <- structure(c(ipos, 1), max.coords = c(length(grid), 1))
      grid_gpx <- prGetShapeGp(shapes_gp, coords, "grid", default = grid_gp)
      if ((length(zero) == 1 && is.na(zero)) || !xpos %in% zero) {
        lg <- linesGrob(
          x = unit(rep(ifelse(xlog, log(xpos), xpos), 2), units = "native"),
          y = unit(c(0, 1), units = "npc"),
          gp = grid_gpx,
          vp = grid_vp
        )
        gridList <- addGrob(gridList, lg)
      }
    }
  }

  if (!is.null(xlab) && nchar(xlab) > 0) {
    gp_list <- txt_gp$xlab
    gp_list$col <- col$axes
    # Write the label for the x-axis
    labGrob <- textGrob(xlab,
                        gp = do.call(gpar, gp_list)
    )
  } else {
    labGrob <- FALSE
  }

  list(axis_vp = axis_vp,
       axisGrob = dg,
       gridList = gridList,
       labGrob = labGrob,
       zero = zero,
       clip = clip,
       x_range = x_range,
       col = col,
       shapes_gp = shapes_gp,
       lwd.zero = lwd.zero) |>
    structure(class = "forestplot_xaxis")
}

#' Retrieve rows with actual data, i.e. not headers
#'
#' Active rows are all excluding the top ones with NA in the mean value
#'
#' @inheritParams prFpGetGraphTicksAndClips
#' @return vector with all active rows (i.e. `from:to`)
#'
#' @noRd
getActiveRowSpan <- function(estimates) {
  mean <- estimates[,1,,drop = FALSE]
  to <- nrow(estimates)
  for (from in 1:to) {
    if (!all(is.na(mean[from,,]))) {
      return(from:to)
    }
  }
  stop("Could not identify rows with actual data")
}
