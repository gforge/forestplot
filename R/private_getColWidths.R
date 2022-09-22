getColWidths <- function(labels, graphwidth, colgap, graph.pos, nc) {
  # There is always at least one column so grab the widest one
  # and have that as the base for the column widths
  colwidths <- unit.c(prFpFindWidestGrob(labels[[1]]))
  # If multiple row label columns, add the other column widths
  if (attr(labels, "no_cols") > 1) {
    for (i in 2:attr(labels, "no_cols")) {
      colwidths <- unit.c(colwidths,
                          colgap,
                          prFpFindWidestGrob(labels[[i]]))
    }
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
    stop(
      "You have to provide graph width either as a unit() object or as 'auto'.",
      " Auto sizes the graph to maximally use the available space.",
      " If you want to have exact mm width then use graphwidth = unit(34, 'mm')."
    )
  }

  # Add the base grapwh width to the total column width
  # default is 2 inches
  if (graph.pos == 1) {
    colwidths <- unit.c(graphwidth, colgap, colwidths)
  } else if (graph.pos == attr(labels, "no_cols") + 1) {
    colwidths <- unit.c(colwidths, colgap, graphwidth)
  } else {
    spl_position <- ((graph.pos - 1) * 2 - 1)
    colwidths <- unit.c(
      colwidths[1:spl_position],
      colgap,
      graphwidth,
      colwidths[(spl_position + 1):length(colwidths)]
    )
  }

}
