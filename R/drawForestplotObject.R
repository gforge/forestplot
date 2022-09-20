#' @noRd
drawForestplotObject <- function(obj) {
  ##################
  # Build the plot #
  ##################
  hrzl_lines <- prFpGetLines(hrzl_lines = obj$hrzl_lines,
                             is.summary = obj$is.summary,
                             total_columns = ncol(obj$labels) + 1,
                             col = obj$col,
                             shapes_gp = obj$shapes_gp)

  labels <- prGetLabelsList(labels = obj$labels,
                            align = obj$align,
                            is.summary = obj$is.summary,
                            txt_gp = obj$txt_gp,
                            col = obj$col)
  obj$labels <- NULL


  xRange <- prFpXrange(upper = obj$upper,
                        lower = obj$lower,
                        clip = obj$clip,
                        zero = obj$zero,
                        xticks = obj$xticks,
                        xlog = obj$xlog)

  axisList <- prFpGetGraphTicksAndClips(xticks = obj$xticks,
                                        xticks.digits = obj$xticks.digits,
                                        grid = obj$grid,
                                        xlog = obj$xlog,
                                        xlab = obj$xlab,
                                        lwd.xaxis = obj$lwd.xaxis,
                                        txt_gp = obj$txt_gp,
                                        col = obj$col,
                                        clip = obj$clip,
                                        zero = obj$zero,
                                        x_range = xRange,
                                        mean = obj$org_mean,
                                        graph.pos = obj$graph.pos,
                                        shapes_gp = obj$shapes_gp)

  marList <- prepGridMargins(mar = obj$mar)
  prPushMarginViewport(bottom = marList$bottom,
                       left = marList$left,
                       top = marList$top,
                       right = marList$right,
                       name = "forestplot_margins")

  if (!all(is.na(obj$title))) {
    prGridPlotTitle(title = obj$title, gp = obj$txt_gp$title)
  }

  legend <- buildLegend(obj$legend,
                        obj$txt_gp,
                        obj$legend_args,
                        obj$colgap,
                        shapes_gp = obj$shapes_gp,
                        lineheight = obj$lineheight,
                        fn.legend = obj$fn.legend)

  plot(legend, margin = TRUE, col = obj$col)

  colwidths <- getColWidths(labels = labels,
                            graphwidth = obj$graphwidth,
                            colgap = obj$colgap,
                            graph.pos = obj$graph.pos)


  # Add space for the axis and the label
  axis_height <- unit(0, "npc")
  if (is.grob(axisList$axisGrob)) {
    axis_height <- axis_height + grobHeight(axisList$axisGrob)
  }

  if (is.grob(axisList$labGrob)) {
    gp_lab_cex <- prGetTextGrobCex(axisList$labGrob)

    # The lab grob y actually includes the axis (note negative)
    axis_height <- axis_height +
      unit(gp_lab_cex + .5, "line")
  }

  axis_layout <- grid.layout(
    nrow = 2,
    ncol = 1,
    heights = unit.c(
      unit(1, "npc") - axis_height,
      axis_height
    )
  )
  pushViewport(viewport(
    layout = axis_layout,
    name = "axis_margin"
  ))
  pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 1))

  # The base viewport, set the increase.line_height paremeter if it seems a little
  # crowded between the lines that might happen when having multiple comparisons
  main_grid_layout <- grid.layout(nrow = attr(labels, "no_rows"),
                                  ncol = length(colwidths),
                                  widths = colwidths,
                                  heights = unit(rep(1 / attr(labels, "no_rows"), attr(labels, "no_rows")), "npc"),
                                  respect = TRUE)

  pushViewport(viewport(
    layout = main_grid_layout,
    name = "BaseGrid"
  ))

  # Create the fourth argument 4 the fpDrawNormalCI() function
  if (!is.null(obj$boxsize)) {
    # If matrix is provided this will convert it
    # to a vector but it doesn't matter in this case
    info <- rep(obj$boxsize, length = length(obj$mean))
  } else {
    # Get width of the lines
    cwidth <- (obj$upper - obj$lower)
    # Set cwidth to min value if the value is invalid
    # this can be the case for reference points
    cwidth[cwidth <= 0 | is.na(cwidth)] <- min(cwidth[cwidth > 0])
    textHeight <- convertUnit(grobHeight(textGrob("A", gp = do.call(gpar, obj$txt_gp$label))),
                              unitTo = "npc",
                              valueOnly = TRUE
    )

    info <- 1 / cwidth * 0.75
    if (!all(obj$is.summary)) {
      info <- info / max(info[!obj$is.summary], na.rm = TRUE)

      # Adjust the dots as it gets ridiculous with small text and huge dots
      if (any(textHeight * (attr(labels, "no_rows") + .5) * 1.5 < info)) {
        info <- textHeight * (attr(labels, "no_rows") + .5) * 1.5 * info / max(info, na.rm = TRUE) + textHeight * (attr(labels, "no_rows") + .5) * 1.5 / 4
      }
    }

    # Set summary to maximum size
    info[obj$is.summary] <- 1 / NCOL(obj$org_mean)
  }

  prFpPrintLabels(
    labels = labels,
    nc = attr(labels, "no_cols"),
    nr = attr(labels, "no_rows"),
    graph.pos = obj$graph.pos
  )

  prFpDrawLines(hrzl_lines = hrzl_lines,
                nr = attr(labels, "no_rows"),
                colwidths = colwidths,
                graph.pos = graph.pos)

  prFpPrintXaxis(axisList = axisList,
                 col = obj$col,
                 lwd.zero = obj$lwd.zero,
                 shapes_gp = obj$shapes_gp)

  # Output the different confidence intervals
  for (i in 1:attr(labels, "no_rows")) {
    if (is.matrix(obj$org_mean)) {
      low_values <- obj$org_lower[i, ]
      mean_values <- obj$org_mean[i, ]
      up_values <- obj$org_upper[i, ]
      info_values <- matrix(info, ncol = length(low_values))[i, ]
    } else {
      low_values <- obj$org_lower[i]
      mean_values <- obj$org_mean[i]
      up_values <- obj$org_upper[i]
      info_values <- info[i]
    }

    # The line and box colors may vary
    clr.line <- rep(obj$col$line, length.out = length(low_values))
    clr.marker <- rep(obj$col$box, length.out = length(low_values))
    clr.summary <- rep(obj$col$summary, length.out = length(low_values))

    line_vp <- viewport(
      layout.pos.row = i,
      layout.pos.col = obj$graph.pos * 2 - 1,
      xscale = axisList$x_range,
      name = sprintf("Line_%d_%d", i, obj$graph.pos * 2 - 1)
    )
    pushViewport(line_vp)

    # Draw multiple confidence intervals
    if (length(low_values) > 1) {
      b_height <- max(info_values)
      if (is.unit(b_height)) {
        b_height <- convertUnit(b_height, unitTo = "npc", valueOnly = TRUE)
      }

      if (is.null(obj$line.margin)) {
        obj$line.margin <- .1 + .2 / (length(low_values) - 1)
      } else if (is.unit(obj$line.margin)) {
        obj$line.margin <- convertUnit(obj$line.margin, unitTo = "npc", valueOnly = TRUE)
      }
      y.offset_base <- b_height / 2 + obj$line.margin
      y.offset_increase <- (1 - obj$line.margin * 2 - b_height) / (length(low_values) - 1)

      for (j in length(low_values):1) {
        # Start from the bottom and plot up
        # the one on top should always be
        # above the one below
        current_y.offset <- y.offset_base + (length(low_values) - j) * y.offset_increase
        if (is.na(mean_values[j])) {
          next
        }

        shape_coordinates <- c(i, j)
        attr(shape_coordinates, "max.coords") <- c(attr(labels, "no_rows"), length(low_values))

        if (obj$is.summary[i]) {
          call_list <-
            list(obj$fn.ci_sum[[i]][[j]],
                 lower_limit = low_values[j],
                 estimate = mean_values[j],
                 upper_limit = up_values[j],
                 size = info_values[j],
                 y.offset = current_y.offset,
                 col = clr.summary[j],
                 shapes_gp = obj$shapes_gp,
                 shape_coordinates = shape_coordinates
            )
        } else {
          call_list <-
            list(obj$fn.ci_norm[[i]][[j]],
                 lower_limit = low_values[j],
                 estimate = mean_values[j],
                 upper_limit = up_values[j],
                 size = info_values[j],
                 y.offset = current_y.offset,
                 clr.line = clr.line[j],
                 clr.marker = clr.marker[j],
                 lty = obj$lty.ci[[i]][[j]],
                 vertices.height = obj$ci.vertices.height,
                 shapes_gp = obj$shapes_gp,
                 shape_coordinates = shape_coordinates
            )

          if (!is.null(obj$ci.vertices)) {
            call_list$vertices <- obj$ci.vertices
          }

          if (!is.null(obj$lwd.ci)) {
            call_list$lwd <- obj$lwd.ci
          }
        }


        # Add additional arguments that are passed on
        # from the original parameters
        for (name in names(obj$extra_arguments)) {
          call_list[[name]] <- obj$extra_arguments[[name]]
        }

        # Do the actual drawing of the object
        tryCatch(eval(as.call(call_list)),
                 error = function(e) {
                   stop("On row ", i, " the print of the estimate failed: ", e$message)
                 }
        )
      }
    } else {
      shape_coordinates <- c(i, 1)
      attr(shape_coordinates, "max.coords") <- c(attr(labels, "no_rows"), 1)

      if (obj$is.summary[i]) {
        call_list <-
          list(obj$fn.ci_sum[[i]],
               lower_limit = low_values,
               estimate = mean_values,
               upper_limit = up_values,
               size = info_values,
               col = clr.summary,
               shapes_gp = obj$shapes_gp,
               shape_coordinates = shape_coordinates
          )
      } else {
        call_list <-
          list(obj$fn.ci_norm[[i]],
               lower_limit = low_values,
               estimate = mean_values,
               upper_limit = up_values,
               size = info_values,
               clr.line = clr.line,
               clr.marker = clr.marker,
               lty = obj$lty.ci[[i]],
               vertices.height = obj$ci.vertices.height,
               shapes_gp = obj$shapes_gp,
               shape_coordinates = shape_coordinates
          )

        if (!is.null(obj$ci.vertices)) {
          call_list$vertices <- obj$ci.vertices
        }

        if (!is.null(obj$lwd.ci)) {
          call_list$lwd <- obj$lwd.ci
        }
      }

      # Add additional arguments that are passed on
      # from the original parameters
      for (name in names(obj$extra_arguments)) {
        call_list[[name]] <- obj$extra_arguments[[name]]
      }

      # Do the actual drawing of the object
      if (!all(is.na(mean_values))) {
        tryCatch(eval(as.call(call_list)),
                 error = function(e) {
                   stop("On row ", i, " the print of the estimate failed: ", e$message)
                 }
        )
      }
    }

    upViewport()
  }

  if (length(legend) > 0 &&
      is.list(obj$legend_args$pos)) {
    plot(legend, margin = FALSE, legend_args = obj$legend_args, col = obj$col, graph.pos = obj$graph.pos, shapes_gp = obj$shapes_gp, legend_colgap = obj$legend_colgap)
  }

  # Go back to the original viewport
  seekViewport("forestplot_margins")
  upViewport(2)
}
