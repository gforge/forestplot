#' @noRd
drawForestplotObject <- function(obj) {
  ##################
  # Build the plot #
  ##################
  hrzl_lines <- prFpGetLines(hrzl_lines = obj$hrzl_lines,
                             is.summary = obj$is.summary,
                             total_columns = obj$nc + 1,
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

  with(obj, {


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
    if (!all(is.na(boxsize))) {
      # If matrix is provided this will convert it
      # to a vector but it doesn't matter in this case
      info <- rep(boxsize, length = length(mean))
    } else {
      # Get width of the lines
      cwidth <- (upper - lower)
      # Set cwidth to min value if the value is invalid
      # this can be the case for reference points
      cwidth[cwidth <= 0 | is.na(cwidth)] <- min(cwidth[cwidth > 0])
      textHeight <- convertUnit(grobHeight(textGrob("A", gp = do.call(gpar, txt_gp$label))),
        unitTo = "npc",
        valueOnly = TRUE
      )

      info <- 1 / cwidth * 0.75
      if (!all(is.summary)) {
        info <- info / max(info[!is.summary], na.rm = TRUE)

        # Adjust the dots as it gets ridiculous with small text and huge dots
        if (any(textHeight * (nr + .5) * 1.5 < info)) {
          info <- textHeight * (nr + .5) * 1.5 * info / max(info, na.rm = TRUE) + textHeight * (nr + .5) * 1.5 / 4
        }
      }

      # Set summary to maximum size
      info[is.summary] <- 1 / NCOL(org_mean)
    }

    prFpPrintLabels(
      labels = labels,
      nc = attr(labels, "no_cols"),
      nr = attr(labels, "no_rows"),
      graph.pos = graph.pos
    )

    prFpDrawLines(hrzl_lines = hrzl_lines,
                  nr = attr(labels, "no_rows"),
                  colwidths = colwidths,
                  graph.pos = graph.pos)

    prFpPrintXaxis(axisList = axisList,
                   col = col,
                   lwd.zero = lwd.zero,
                   shapes_gp = shapes_gp)

    # Output the different confidence intervals
    for (i in 1:attr(labels, "no_rows")) {
      if (is.matrix(org_mean)) {
        low_values <- org_lower[i, ]
        mean_values <- org_mean[i, ]
        up_values <- org_upper[i, ]
        info_values <- matrix(info, ncol = length(low_values))[i, ]
      } else {
        low_values <- org_lower[i]
        mean_values <- org_mean[i]
        up_values <- org_upper[i]
        info_values <- info[i]
      }

      # The line and box colors may vary
      clr.line <- rep(col$line, length.out = length(low_values))
      clr.marker <- rep(col$box, length.out = length(low_values))
      clr.summary <- rep(col$summary, length.out = length(low_values))

      line_vp <- viewport(
        layout.pos.row = i,
        layout.pos.col = graph.pos * 2 - 1,
        xscale = axisList$x_range,
        name = sprintf("Line_%d_%d", i, graph.pos * 2 - 1)
      )
      pushViewport(line_vp)

      # Draw multiple confidence intervals
      if (length(low_values) > 1) {
        b_height <- max(info_values)
        if (is.unit(b_height)) {
          b_height <- convertUnit(b_height, unitTo = "npc", valueOnly = TRUE)
        }

        if (is.na(line.margin)) {
          line.margin <- .1 + .2 / (length(low_values) - 1)
        } else if (is.unit(line.margin)) {
          line.margin <- convertUnit(line.margin, unitTo = "npc", valueOnly = TRUE)
        }
        y.offset_base <- b_height / 2 + line.margin
        y.offset_increase <- (1 - line.margin * 2 - b_height) / (length(low_values) - 1)

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

          if (is.summary[i]) {
            call_list <-
              list(fn.ci_sum[[i]][[j]],
                lower_limit = low_values[j],
                estimate = mean_values[j],
                upper_limit = up_values[j],
                size = info_values[j],
                y.offset = current_y.offset,
                col = clr.summary[j],
                shapes_gp = shapes_gp,
                shape_coordinates = shape_coordinates
              )
          } else {
            call_list <-
              list(fn.ci_norm[[i]][[j]],
                lower_limit = low_values[j],
                estimate = mean_values[j],
                upper_limit = up_values[j],
                size = info_values[j],
                y.offset = current_y.offset,
                clr.line = clr.line[j],
                clr.marker = clr.marker[j],
                lty = lty.ci[[i]][[j]],
                vertices.height = ci.vertices.height,
                shapes_gp = shapes_gp,
                shape_coordinates = shape_coordinates
              )

            if (!all(is.na(ci.vertices))) {
              call_list$vertices <- ci.vertices
            }

            if (!all(is.na(lwd.ci))) {
              call_list$lwd <- lwd.ci
            }
          }


          # Add additional arguments that are passed on
          # from the original parameters
          for (name in names(extra_arguments)) {
            call_list[[name]] <- extra_arguments[[name]]
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
        attr(shape_coordinates, "max.coords") <- c(nr, 1)

        if (is.summary[i]) {
          call_list <-
            list(fn.ci_sum[[i]],
              lower_limit = low_values,
              estimate = mean_values,
              upper_limit = up_values,
              size = info_values,
              col = clr.summary,
              shapes_gp = shapes_gp,
              shape_coordinates = shape_coordinates
            )
        } else {
          call_list <-
            list(fn.ci_norm[[i]],
              lower_limit = low_values,
              estimate = mean_values,
              upper_limit = up_values,
              size = info_values,
              clr.line = clr.line,
              clr.marker = clr.marker,
              lty = lty.ci[[i]],
              vertices.height = ci.vertices.height,
              shapes_gp = shapes_gp,
              shape_coordinates = shape_coordinates
            )

          if (!all(is.na(ci.vertices))) {
            call_list$vertices <- ci.vertices
          }

          if (!all(is.na(lwd.ci))) {
            call_list$lwd <- lwd.ci
          }
        }

        # Add additional arguments that are passed on
        # from the original parameters
        for (name in names(extra_arguments)) {
          call_list[[name]] <- extra_arguments[[name]]
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

    # Output the legend if it is inside the main plot
    if (!all(is.na(legend)) &&
      is.list(legend_args$pos)) {
      plot_vp <- viewport(
        layout.pos.row = 1:nr,
        layout.pos.col = 2 * graph.pos - 1,
        name = "main_plot_area"
      )
      pushViewport(plot_vp)

      if ("align" %in% names(legend_args$pos) && legend_args$pos[["align"]] == "horizontal") {
        # Calculated with padding above
        height <- legend_horizontal_height
        # Calculate the horizontal width by iterating througha all elements
        # as each element may have a different width
        width <- 0
        for (i in 1:length(lGrobs)) {
          if (width > 0) {
            width <- width + convertUnit(legend_colgap, unitTo = "npc", valueOnly = TRUE)
          }
          width <- width + convertUnit(attr(lGrobs, "max_height") + legend_colgap + attr(lGrobs[[i]], "width"), unitTo = "npc", valueOnly = TRUE)
        }
        # Add the padding
        width <- unit(width + convertUnit(legend_args$padding, unitTo = "npc", valueOnly = TRUE) * 2, "npc")
      } else {
        legend_height <- attr(lGrobs, "line_height_and_spacing")[rep(1:2, length.out = length(legend) * 2 - 1)]
        if (!is.null(attr(lGrobs, "title"))) {
          legend_height <- unit.c(
            attr(lGrobs, "titleHeight"),
            attr(lGrobs, "line_height_and_spacing")[2], legend_height
          )
        }

        height <- sum(legend_args$padding, legend_height, legend_args$padding)
        width <- legend_vertical_width
      }
      pushViewport(viewport(
        x = legend_args$pos[["x"]],
        y = legend_args$pos[["y"]],
        width = width,
        height = height,
        just = legend_args$pos[["just"]]
      ))
      # Draw the legend
      prFpDrawLegend(
        lGrobs = lGrobs,
        col = col,
        shapes_gp = shapes_gp,
        colgap = legend_colgap,
        pos = legend_args$pos,
        gp = legend_args$gp,
        r = legend_args$r,
        padding = legend_args$padding,
        fn.legend = fn.legend
      )
      upViewport(2)
    }

    # Go back to the original viewport
    seekViewport("forestplot_margins")
    upViewport(2)
  })
}
