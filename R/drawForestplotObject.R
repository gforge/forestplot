#' @noRd
drawForestplotObject <- function(obj) {
  ##################
  # Build the plot #
  ##################
  hrzl_lines <- prFpGetLines(hrzl_lines = obj$hrzl_lines,
                             is.summary = obj$is.summary,
                             total_columns = attr(obj$labels, "no_cols") + 1,
                             col = obj$col,
                             shapes_gp = obj$shapes_gp)

  labels <- prGetLabelsList(labels = obj$labels,
                            align = obj$align,
                            is.summary = obj$is.summary,
                            txt_gp = obj$txt_gp,
                            col = obj$col)
  obj$labels <- NULL


  xRange <- prFpXrange(upper = obj$estimates[,3,],
                       lower = obj$estimates[,2,],
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
                                        lwd.zero = obj$lwd.zero,
                                        txt_gp = obj$txt_gp,
                                        col = obj$col,
                                        clip = obj$clip,
                                        zero = obj$zero,
                                        x_range = xRange,
                                        estimates = obj$estimates,
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
                        col = obj$col,
                        shapes_gp = obj$shapes_gp,
                        lineheight = obj$lineheight,
                        fn.legend = obj$fn.legend)

  plot(legend, margin = TRUE)

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

  info <- prepBoxSize(boxsize = obj$boxsize,
                      estimates = obj$estimates,
                      is.summary = obj$is.summary,
                      txt_gp = obj$txt_gp)

  prFpPrintLabels(
    labels = labels,
    nc = attr(labels, "no_cols"),
    nr = attr(labels, "no_rows"),
    graph.pos = obj$graph.pos
  )

  prFpDrawLines(hrzl_lines = hrzl_lines,
                nr = attr(labels, "no_rows"),
                colwidths = colwidths,
                graph.pos = obj$graph.pos)

  plot(axisList)

  # Output the different confidence intervals
  for (i in 1:nrow(obj$estimates)) {
    # The line and box colors may vary
    clr.line <- rep(obj$col$line, length.out = dim(obj$estimates)[3])
    clr.marker <- rep(obj$col$box, length.out = dim(obj$estimates)[3])
    clr.summary <- rep(obj$col$summary, length.out = dim(obj$estimates)[3])

    line_vp <- viewport(
      layout.pos.row = i,
      layout.pos.col = obj$graph.pos * 2 - 1,
      xscale = axisList$x_range,
      name = sprintf("Line_%d_%d", i, obj$graph.pos * 2 - 1)
    )
    pushViewport(line_vp)

    # Draw multiple confidence intervals
    if (dim(obj$estimates)[3] > 1) {
      b_height <- max(info[i,])
      if (is.unit(b_height)) {
        b_height <- convertUnit(b_height, unitTo = "npc", valueOnly = TRUE)
      }

      if (is.null(obj$line.margin)) {
        obj$line.margin <- .1 + .2 / (dim(obj$estimates)[3] - 1)
      } else if (is.unit(obj$line.margin)) {
        obj$line.margin <- convertUnit(obj$line.margin, unitTo = "npc", valueOnly = TRUE)
      }
      y.offset_base <- b_height / 2 + obj$line.margin
      y.offset_increase <- (1 - obj$line.margin * 2 - b_height) / (dim(obj$estimates)[3] - 1)

      for (j in dim(obj$estimates)[3]:1) {
        # Start from the bottom and plot up
        # the one on top should always be
        # above the one below
        current_y.offset <- y.offset_base + (dim(obj$estimates)[3] - j) * y.offset_increase
        if (is.na(obj$estimates[i, 1, j])) {
          next
        }

        shape_coordinates <- c(i, j)
        attr(shape_coordinates, "max.coords") <- c(attr(labels, "no_rows"), dim(obj$estimates)[3])

        if (obj$is.summary[i]) {
          call_list <-
            list(obj$fn.ci_sum[[i]][[j]],
                 estimate = obj$estimates[i, 1, j],
                 lower_limit = obj$estimates[i, 2, j],
                 upper_limit = obj$estimates[i, 3, j],
                 size = info[i, j],
                 y.offset = current_y.offset,
                 col = clr.summary[j],
                 shapes_gp = obj$shapes_gp,
                 shape_coordinates = shape_coordinates
            )
        } else {
          call_list <-
            list(obj$fn.ci_norm[[i]][[j]],
                 estimate = obj$estimates[i, 1, j],
                 lower_limit = obj$estimates[i, 2, j],
                 upper_limit = obj$estimates[i, 3, j],
                 size = info[i, j],
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
          list(obj$fn.ci_sum[[i]][[1]],
               estimate = obj$estimates[i, 1, 1],
               lower_limit = obj$estimates[i, 2, 1],
               upper_limit = obj$estimates[i, 3, 1],
               size = info[i, 1],
               col = clr.summary,
               shapes_gp = obj$shapes_gp,
               shape_coordinates = shape_coordinates
          )
      } else {
        call_list <-
          list(obj$fn.ci_norm[[i]][[1]],
               estimate = obj$estimates[i, 1, 1],
               lower_limit = obj$estimates[i, 2, 1],
               upper_limit = obj$estimates[i, 3, 1],
               size = info[i, 1],
               clr.line = clr.line,
               clr.marker = clr.marker,
               lty = obj$lty.ci[[i]][[1]],
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
      if (!all(is.na(obj$estimates[i, 1, 1]))) {
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
