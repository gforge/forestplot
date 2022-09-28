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

  missing_rows <- apply(obj$estimates, 2, \(row) all(is.na(row)))

  fn.ci_norm <- prFpGetConfintFnList(fn = obj$fn.ci_norm,
                                     no_rows = nrow(obj$estimates),
                                     no_depth = dim(obj$estimates)[3],
                                     missing_rows = missing_rows,
                                     is.summary = obj$is.summary,
                                     summary = FALSE)
  obj$fn.ci_norm <- NULL
  fn.ci_sum <- prFpGetConfintFnList(fn = obj$fn.ci_sum,
                                    no_rows = nrow(obj$estimates),
                                    no_depth = dim(obj$estimates)[3],
                                    missing_rows = missing_rows,
                                    is.summary = obj$is.summary,
                                    summary = TRUE)
  obj$fn.ci_sum <- NULL
  lty.ci <- prPopulateList(obj$lty.ci,
                           no_rows = nrow(obj$estimates),
                           no_depth = dim(obj$estimates)[3])
  obj$lty.ci <- NULL

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

  plotGraphBox(boxGrob = obj$graph_box,
               estimates = obj$estimates,
               graph.pos = obj$graph.pos)

  plot(axisList)

  plotConfidenceInterval(obj = obj,
                         axisList = axisList,
                         info = info,
                         labels = labels,
                         fn.ci_sum = fn.ci_sum,
                         fn.ci_norm = fn.ci_norm,
                         lty.ci = lty.ci)

  if (length(legend) > 0 &&
      is.list(obj$legend_args$pos)) {
    plot(legend, margin = FALSE, legend_args = obj$legend_args, col = obj$col, graph.pos = obj$graph.pos, shapes_gp = obj$shapes_gp, legend_colgap = obj$legend_colgap)
  }

  # Go back to the original viewport
  seekViewport("forestplot_margins")
  upViewport(2)
}
