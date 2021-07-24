drawForestplotObject <- function(obj) {
  ##################
  # Build the plot #
  ##################
  with(obj, {
    # Adjust for the margins and the x-axis + label
    marList <- list()

    # This breaks without separate variables
    marList$bottom <- convertY(mar[1], "npc")
    marList$left <- convertX(mar[2], "npc")
    marList$top <- convertY(mar[3], "npc")
    marList$right <- convertX(mar[4], "npc")

    prPushMarginViewport(bottom = marList$bottom,
                         left = marList$left,
                         top = marList$top,
                         right = marList$right,
                         name = "forestplot_margins")

    if (!all(is.na(title))) {
      prGridPlotTitle(title = title, gp = txt_gp$title)
    }

    # Initiate the legend
    if (!all(is.na(legend))) {
      lGrobs <- prFpGetLegendGrobs(legend = legend,
                                   txt_gp = txt_gp,
                                   title = legend_args$title)
      legend_colgap <- colgap
      if (convertUnit(legend_colgap, unitTo = "mm", valueOnly = TRUE) >
          convertUnit(attr(lGrobs, "max_height"), unitTo = "mm", valueOnly = TRUE)) {
        legend_colgap <- attr(lGrobs, "max_height")
      }

      legend_horizontal_height <-
        sum(legend_args$padding,
            attr(lGrobs, "max_height"),
            legend_args$padding)
      if (!is.null(attr(lGrobs, "title"))) {
        legend_horizontal_height <-
          sum(attr(lGrobs, "titleHeight"),
              attr(lGrobs, "line_height_and_spacing")[2],
              legend_horizontal_height)
      }
      legend_vertical_width <-
        sum(unit.c(legend_args$padding,
                   attr(lGrobs, "max_height"),
                   legend_colgap,
                   attr(lGrobs, "max_width"),
                   legend_args$padding))



      # Prepare the viewports if the legend is not
      # positioned inside the forestplot, i.e. on the top or right side
      if ((!is.list(legend_args$pos) && legend_args$pos == "top") ||
          ("align" %in% names(legend_args$pos) && legend_args$pos[["align"]] == "horizontal")) {
        legend_layout <- grid.layout(nrow = 3, ncol = 1,
                                     heights = unit.c(legend_horizontal_height,
                                                      legend_colgap + legend_colgap,
                                                      unit(1, "npc") -
                                                        legend_horizontal_height -
                                                        legend_colgap -
                                                        legend_colgap))

        legend_pos <- list(row = 1,
                           col = 1)
        main_pos <- list(row = 3,
                         col = 1)
      }else{
        legend_layout <- grid.layout(nrow = 1, ncol = 3,
                                     widths = unit.c(unit(1, "npc") -
                                                       legend_colgap -
                                                       legend_vertical_width,
                                                     legend_colgap,
                                                     legend_vertical_width))
        legend_pos <- list(row = 1,
                           col = 3)
        main_pos <- list(row = 1,
                         col = 1)
      }
    }

    # If the legend should be positioned within the plot then wait
    # until after the plot has been drawn
    if (!all(is.na(legend)) > 0 && !is.list(legend_args$pos)) {
      pushViewport(prFpGetLayoutVP(lineheight = lineheight,
                                   labels = labels,
                                   nr = nr,
                                   legend_layout = legend_layout))
      vp <- viewport(layout.pos.row = legend_pos$row,
                     layout.pos.col = legend_pos$col,
                     name = "legend")
      pushViewport(vp)

      # Draw the legend
      prFpDrawLegend(lGrobs = lGrobs,
                     col = col,
                     shapes_gp = shapes_gp,
                     colgap = convertUnit(legend_colgap, unitTo = "mm"),
                     pos = legend_args$pos,
                     gp = legend_args$gp,
                     r = legend_args$r,
                     padding = legend_args$padding,
                     fn.legend = fn.legend)
      upViewport()

      # Reset to the main plot
      vp <- viewport(layout.pos.row = main_pos$row,
                     layout.pos.col = main_pos$col,
                     name = "main")
      pushViewport(vp)
    }else{
      pushViewport(prFpGetLayoutVP(lineheight = lineheight,
                                   labels = labels, nr = nr))
    }

    ###########################################
    # Normalize the widths to cover the whole #
    # width of the graph space.               #
    ###########################################
    if (!is.unit(graphwidth) &&
        graphwidth == "auto") {
      # If graph width is not provided as a unit the autosize it to the
      # rest of the space available
      npc_colwidths <- convertUnit(unit.c(colwidths, colgap), "npc", valueOnly = TRUE)
      graphwidth <- unit(max(.05, 1 - sum(npc_colwidths)), "npc")
    } else if (!is.unit(graphwidth)) {
      stop("You have to provide graph width either as a unit() object or as 'auto'.",
           " Auto sizes the graph to maximally use the available space.",
           " If you want to have exact mm width then use graphwidth = unit(34, 'mm').")
    }

    # Add the base grapwh width to the total column width
    # default is 2 inches
    if (graph.pos == 1) {
      colwidths <- unit.c(graphwidth, colgap, colwidths)
    }else if (graph.pos == nc + 1) {
      colwidths <- unit.c(colwidths, colgap, graphwidth)
    }else{
      spl_position <- ((graph.pos - 1) * 2 - 1)
      colwidths <- unit.c(colwidths[1:spl_position],
                          colgap,
                          graphwidth,
                          colwidths[(spl_position + 1):length(colwidths)])
    }

    # Add space for the axis and the label
    axis_height <- unit(0, "npc")
    if (is.grob(axisList$axisGrob))
      axis_height <- axis_height  + grobHeight(axisList$axisGrob)
    if (is.grob(axisList$labGrob)) {
      gp_lab_cex <- prGetTextGrobCex(axisList$labGrob)

      # The lab grob y actually includes the axis (note negative)
      axis_height <-  axis_height +
        unit(gp_lab_cex + .5, "line")
    }

    axis_layout <- grid.layout(nrow = 2,
                               ncol = 1,
                               heights = unit.c(unit(1, "npc") - axis_height,
                                                axis_height))
    pushViewport(viewport(layout = axis_layout,
                          name = "axis_margin"))
    pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 1))

    # The base viewport, set the increase.line_height paremeter if it seems a little
    # crowded between the lines that might happen when having multiple comparisons
    main_grid_layout <- grid.layout(nrow   = nr,
                                    ncol   = length(colwidths),
                                    widths = colwidths,
                                    heights = unit(rep(1/nr, nr), "npc"),
                                    respect = TRUE)
    pushViewport(viewport(layout = main_grid_layout,
                          name = "BaseGrid"))

    # Create the fourth argument 4 the fpDrawNormalCI() function
    if (!all(is.na(boxsize))) {
      # If matrix is provided this will convert it
      # to a vector but it doesn't matter in this case
      info <- rep(boxsize, length = length(mean))
    }else{
      # Get width of the lines
      cwidth <- (upper - lower)
      # Set cwidth to min value if the value is invalid
      # this can be the case for reference points
      cwidth[cwidth <= 0 | is.na(cwidth)] <- min(cwidth[cwidth > 0])
      textHeight <- convertUnit(grobHeight(textGrob("A", gp = do.call(gpar, txt_gp$label))),
                                unitTo = "npc",
                                valueOnly = TRUE)
      info <- 1/cwidth*0.75
      info <- info/max(info[!is.summary], na.rm = TRUE)
      # Adjust the dots as it gets ridiculous with small text and huge dots
      if (any(textHeight*(nr + .5) * 1.5 < info))
        info <- textHeight*(nr + .5) * 1.5 * info/max(info, na.rm = TRUE) + textHeight*(nr + .5)*1.5/4

      # Set summary to maximum size
      info[is.summary] <- 1/NCOL(org_mean)
    }

    prFpPrintLabels(labels = labels,
                    nc = nc,
                    nr = nr,
                    graph.pos = graph.pos)

    prFpDrawLines(hrzl_lines = hrzl_lines, nr = nr, colwidths = colwidths,
                  graph.pos = graph.pos)


    prFpPrintXaxis(axisList = axisList,
                   col = col,
                   lwd.zero = lwd.zero,
                   shapes_gp = shapes_gp)

    # Output the different confidence intervals
    for (i in 1:nr) {
      if (is.matrix(org_mean)) {
        low_values <- org_lower[i,]
        mean_values <- org_mean[i,]
        up_values <- org_upper[i,]
        info_values <- matrix(info, ncol = length(low_values))[i, ]
      }else{
        low_values <- org_lower[i]
        mean_values <- org_mean[i]
        up_values <- org_upper[i]
        info_values <- info[i]
      }

      # The line and box colors may vary
      clr.line <- rep(col$line, length.out = length(low_values))
      clr.marker <- rep(col$box, length.out = length(low_values))
      clr.summary <- rep(col$summary, length.out = length(low_values))

      line_vp <- viewport(layout.pos.row = i,
                          layout.pos.col = graph.pos * 2 - 1,
                          xscale = axisList$x_range,
                          name = sprintf("Line_%d_%d", i, graph.pos*2 - 1))
      pushViewport(line_vp)

      # Draw multiple confidence intervals
      if (length(low_values) > 1) {
        b_height <- max(info_values)
        if (is.unit(b_height))
          b_height <- convertUnit(b_height, unitTo = "npc", valueOnly = TRUE)

        if (is.na(line.margin)) {
          line.margin <- .1 + .2/(length(low_values) - 1)
        }else if (is.unit(line.margin)) {
          line.margin <- convertUnit(line.margin, unitTo = "npc", valueOnly = TRUE)
        }
        y.offset_base <- b_height/2 + line.margin
        y.offset_increase <- (1 - line.margin*2 - b_height)/(length(low_values) - 1)

        for (j in length(low_values):1) {
          # Start from the bottom and plot up
          # the one on top should always be
          # above the one below
          current_y.offset <- y.offset_base + (length(low_values) - j) * y.offset_increase
          if (is.na(mean_values[j]))
            next;

          shape_coordinates <- c(i, j)
          attr(shape_coordinates, "max.coords") <- c(nr, length(low_values))

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
          }else{
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

            if (!all(is.na(ci.vertices)))
              call_list$vertices = ci.vertices;

            if (!all(is.na(lwd.ci)))
              call_list$lwd <- lwd.ci
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
                   })
        }
      }else{
        shape_coordinates <- c(i,1)
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
        }else{
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

          if (!all(is.na(ci.vertices)))
            call_list$vertices = ci.vertices;

          if (!all(is.na(lwd.ci)))
            call_list$lwd <- lwd.ci
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
                   })

        }
      }

      upViewport()
    }

    # Output the legend if it is inside the main plot
    if (!all(is.na(legend)) &&
        is.list(legend_args$pos)) {
      plot_vp <- viewport(layout.pos.row = 1:nr,
                          layout.pos.col = 2 * graph.pos - 1,
                          name = "main_plot_area")
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
        width <- unit(width + convertUnit(legend_args$padding, unitTo = "npc", valueOnly = TRUE)*2, "npc")
      }else{
        legend_height <- attr(lGrobs, "line_height_and_spacing")[rep(1:2, length.out = length(legend)*2 - 1)]
        if (!is.null(attr(lGrobs, "title"))) {
          legend_height <- unit.c(attr(lGrobs, "titleHeight"),
                                  attr(lGrobs, "line_height_and_spacing")[2], legend_height)
        }

        height <- sum(legend_args$padding, legend_height, legend_args$padding)
        width <- legend_vertical_width
      }
      pushViewport(viewport(x = legend_args$pos[["x"]],
                            y = legend_args$pos[["y"]],
                            width = width,
                            height = height,
                            just = legend_args$pos[["just"]]))
      # Draw the legend
      prFpDrawLegend(lGrobs = lGrobs,
                     col = col,
                     shape_gp = shape_gp,
                     colgap = legend_colgap,
                     pos = legend_args$pos,
                     gp = legend_args$gp,
                     r = legend_args$r,
                     padding = legend_args$padding,
                     fn.legend = fn.legend)
      upViewport(2)
    }

    # Go back to the original viewport
    seekViewport("forestplot_margins")
    upViewport(2)
  })
}
