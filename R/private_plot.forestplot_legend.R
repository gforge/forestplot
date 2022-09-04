plot.forestplot_legend <- function(x, margin, col, ...) {
  if (margin) {
    return(pr_plot_forestplot_legend_at_margin(x, col = col))
  }

  return(pr_plot_forestplot_legend_inside_plot(x))
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

pr_plot_forestplot_legend_inside_plot <- function(x) {
  browser()
}
