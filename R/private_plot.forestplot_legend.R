plot.forestplot_legend <- function(x, margin, col, legend_args, graph.pos, shapes_gp, legend_colgap, ...) {
  # No forestplot to output
  if (length(x) == 0) {
    return()
  }

  if (margin) {
    return(pr_plot_forestplot_legend_at_margin(x, col = col))
  }

  return(pr_plot_forestplot_legend_inside_plot(x, col, legend_args = legend_args, graph.pos = graph.pos, shapes_gp = shapes_gp, legend_colgap = legend_colgap))
}

pr_plot_forestplot_legend_at_margin <- function(x, col) {
  # If the legend should be positioned within the plot then wait
  # until after the plot has been drawn
  if (!inherits(attr(x, "pos"), "forestplot_legend_position")) {
    return(prFpGetLayoutVP(
      lineheight = attr(x, "lineheight"),
      labels = x
    ) |>
    pushViewport())
  }

  prFpGetLayoutVP(
    labels = x,
    lineheight = attr(x, "lineheight"),
    legend_layout = attr(x, "layout")
  ) |>
  pushViewport()
  viewport(
    layout.pos.row = attr(x, "pos")$row,
    layout.pos.col = attr(x, "pos")$col,
    name = "legend"
  ) |>
  pushViewport()

  # Draw the legend
  prFpDrawLegend(
    lGrobs = x,
    col = col,
    fn.legend = attr(x, "fn.legend")
  )
  upViewport()

  # Reset to the main plot
  return(viewport(
    layout.pos.row = attr(x, "main")$row,
    layout.pos.col = attr(x, "main")$col,
    name = "main"
  ) |>
  pushViewport())
}

pr_plot_forestplot_legend_inside_plot <- function(x, col, graph.pos, shapes_gp, legend_args, legend_colgap) {
  plot_vp <- viewport(
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
    for (i in 1:length(x)) {
      if (width > 0) {
        width <- width + convertUnit(legend_colgap, unitTo = "npc", valueOnly = TRUE)
      }
      width <- width + convertUnit(attr(x, "max_height") + legend_colgap + attr(lGrobs[[i]], "width"), unitTo = "npc", valueOnly = TRUE)
    }
    # Add the padding
    width <- unit(width + convertUnit(legend_args$padding, unitTo = "npc", valueOnly = TRUE) * 2, "npc")
  } else {
    legend_height <- attr(x, "line_height_and_spacing")[rep(1:2, length.out = length(x) * 2 - 1)]
    if (!is.null(attr(x, "title"))) {
      legend_height <- unit.c(
        attr(x, "titleHeight"),
        attr(x, "line_height_and_spacing")[2], legend_height
      )
    }

    height <- sum(legend_args$padding, legend_height, legend_args$padding)
    width <- attr(x, "legend_vertical_width")
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
    lGrobs = x,
    col = col,
    shapes_gp = shapes_gp,
    colgap = legend_colgap,
    pos = legend_args$pos,
    gp = legend_args$gp,
    r = legend_args$r,
    padding = legend_args$padding,
    fn.legend = attr(x, "fn.legend")
  )
  upViewport(2)
}
