#' Decorate the graph
#'
#' @param x The forestplot object
#' @param box Decorate the graph by framing it in a box. If provided `TRUE` it
#'  will simply frame the graph in a black box. If you provide a string it is
#'  assumed to be the color of the graph. Acceptable arguments are also `gpar()`
#'  and a `grob` object to draw.
#'
#' @return The forestplot object with the extended decoration
#' @export
#'
#' @example inst/examples/fp_decorate_graph_example.R
#' @family graph modifiers
fp_decorate_graph <- function(x,
                              box = NULL) {
  if (!is.null(box)) {
    if (isTRUE(box)) {
      boxGrob <- rectGrob()
    } else if (is.grob(box)) {
      boxGrob <- box
    } else if (is.character(box)) {
      boxGrob <- rectGrob(gp = gpar(col = box))
    } else if (is.list(box)) {
      boxGrob <- rectGrob(gp = box)
    } else {
      stop("Invalid box argument, expected color as string, grob or a gpar()")
    }
    x$graph_box <- boxGrob
  }

  return(x)
}

plotGraphBox <- function(boxGrob, estimates, graph.pos) {
  if (is.null(boxGrob)) return();

  first_regular_row <- which(apply(estimates, \(x) all(is.na(x)), MARGIN = 1)) |> tail(1) + 1
  pushViewport(viewport(
    layout.pos.row = first_regular_row:nrow(estimates),
    layout.pos.col = graph.pos * 2 - 1,
    name = "Graph decorator"
  ))

  grid.draw(boxGrob)
  upViewport()
}
