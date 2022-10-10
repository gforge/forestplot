getColWidths <- function(labels, graphwidth, colgap, graph.pos, nc) {
  appendWidth <- \(x, width) x |>
    append(list(width))

  prependWidth <- \(x, width) list(width) |>
    append(x)

  # There is always at least one column so grab the widest one
  # and have that as the base for the column widths
  colwidths <- list(prFpFindWidestGrob(labels[[1]]))
  # If multiple row label columns, add the other column widths
  if (attr(labels, "no_cols") > 1) {
    for (i in 2:attr(labels, "no_cols")) {
      colwidths <- appendWidth(colwidths,
                               prFpFindWidestGrob(labels[[i]]))
    }
  }

  # Add the base graph width to the total column width
  # default is 2 inches
  if (graph.pos == 1) {
    colwidths <- prependWidth(colwidths, graphwidth)
  } else if (graph.pos == attr(labels, "no_cols") + 1) {
    colwidths <- appendWidth(colwidths, graphwidth)
  } else {
    spl_position <- graph.pos - 1
    colwidths <- appendWidth(colwidths[1:spl_position],
                             graphwidth) |>
      append(colwidths[(spl_position + 1):length(colwidths)])
  }

  colwidths_with_colgap <- list()
  for (i in 1:length(colwidths)) {
    if (i != 1) {
      colwidths_with_colgap <- appendWidth(colwidths_with_colgap, colgap)
    }
    colwidths_with_colgap <- appendWidth(colwidths_with_colgap,
                                         colwidths[[i]])
  }

  ###########################################
  # Normalize the widths to cover the whole #
  # width of the graph space.               #
  ###########################################
  if (!is.unit(graphwidth) &&
      graphwidth == "auto") {
    other_columns_width <- colwidths_with_colgap[sapply(colwidths_with_colgap, is.unit)] |>
      (\(x) do.call(unit.c, x))() |>
      sum()

    graph_index <- sapply(colwidths_with_colgap, Negate(is.unit)) |> which()
    if (length(graph_index) != 1) {
      if (length(graph_index) == 0) {
        stop("Could not find graph position")
      } else {
        stop("The graph can't be in multiple positions, this is a bug - please report to maintainer")
      }
    }

    # If graph width is not provided as a unit the autosize it to the
    # rest of the space available
    graphwidth <- unit(1, "npc") - other_columns_width
    # While the logic makes sense it seems that the auto calculating
    # function is off and we shouldn't rely on the logic below
    # as the number is smaller than the graph actually turns out
    if (convertWidth(graphwidth, unitTo = "npc", valueOnly = TRUE) < 0.05) {
      graphwidth <- unit(0.3, "npc")
    }

    colwidths_with_colgap[[graph_index]] <- graphwidth
  } else if (!is.unit(graphwidth)) {
    stop(
      "You have to provide graph width either as a unit() object or as 'auto'.",
      " Auto sizes the graph to maximally use the available space.",
      " If you want to have exact mm width then use graphwidth = unit(34, 'mm')."
    )
  }

  do.call(unit.c, colwidths_with_colgap)
}
