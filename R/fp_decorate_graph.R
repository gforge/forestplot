#' Decorate the graph
#'
#' @param x The forestplot object
#' @param box Decorate the graph by framing it in a box. If provided `TRUE` it
#'  will simply frame the graph in a black box. If you provide a string it is
#'  assumed to be the color of the graph. Acceptable arguments are also `gpar()`
#'  and a `grob` object to draw.
#' @param right_bottom_txt Text to appear at the right bottom of the graph. Can
#'  be decorated fp_txt_* functions.
#' @param left_bottom_txt Text to appear at the left bottom of the graph. Can
#'  be decorated fp_txt_* functions.
#' @param right_top_txt Text to appear at the right top of the graph. Can
#'  be decorated fp_txt_* functions.
#' @param left_top_txt Text to appear at the left top of the graph. Can
#'  be decorated fp_txt_* functions.
#' @param grid If you want a discrete gray dashed grid at the level of the
#'   ticks you can set this parameter to \code{TRUE}. If you set the parameter
#'   to a vector of values lines will be drawn at the corresponding positions.
#'   If you want to specify the \code{\link[grid]{gpar}} of the lines then either
#'   directly pass a \code{\link[grid]{gpar}} object or set the gp attribute e.g.
#'   \code{attr(line_vector, "gp") <- \link[grid]{gpar}(lty = 2, col = "red")}
#' @param graph.pos The position of the graph element within the table of text. The
#'   position can be \code{1-(ncol(labeltext) + 1)}. You can also choose set the position
#'   to \code{"left"} or \code{"right"}.
#'
#' @return The forestplot object with the extended decoration
#' @export
#'
#' @example inst/examples/fp_decorate_graph_example.R
#' @family graph modifiers
#' @family forestplot functions
fp_decorate_graph <- function(x,
                              box = NULL,
                              right_bottom_txt = NULL,
                              left_bottom_txt = NULL,
                              right_top_txt = NULL,
                              left_top_txt = NULL,
                              grid = NULL,
                              graph.pos = NULL) {
  if (!is.null(box)) {
    if (isTRUE(box)) {
      boxGrob <- rectGrob(gp = gpar(fill = NA))
    } else if (is.grob(box)) {
      boxGrob <- box
    } else if (is.character(box)) {
      boxGrob <- rectGrob(gp = gpar(col = box, fill = NA))
    } else if (is.list(box)) {
      if (is.null(box$fill)) {
        box$fill <- NA
      }
      boxGrob <- rectGrob(gp = box)
    } else {
      stop("Invalid box argument, expected color as string, grob or a gpar()")
    }
    x$graph_box <- boxGrob
  }

  x$graph_right_bottom_txt <- right_bottom_txt
  x$graph_left_bottom_txt <- left_bottom_txt
  x$graph_right_top_txt <- right_top_txt
  x$graph_left_top_txt <- left_top_txt

  if (!is.null(grid)) {
    x$grid <- grid
  }

  if (!is.null(graph.pos)) {
    graph.pos <- prepGraphPositions(graph.pos, nc = attr(x$labels, "no_cols"))
    x$graph.pos <- graph.pos
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

plotGraphText <- function(obj) {
  txt_names <- paste0("graph_", c("leftt_bottom_txt", "right_bottom_txt"))
  txt_elements <- obj[which(names(obj) %in% txt_names)]
  if (length(txt_elements) == 0) return()
  estimates <- obj$estimates
  graph.pos <- obj$graph.pos

  first_regular_row <- which(apply(estimates, \(x) all(is.na(x)), MARGIN = 1)) |> tail(1) + 1
  pushViewport(viewport(
    layout.pos.row = first_regular_row:nrow(estimates),
    layout.pos.col = graph.pos * 2 - 1,
    name = "Graph text"
  ))

  drawBox <- function(name, ...) {
    elmnt <- obj[[name]]
    if (is.null(elmnt)) return()
    if (is.list(elmnt)) {
      elmnt <- elmnt[[1]]
    }

    grid.text(elmnt,
              gp = attr(elmnt, "txt_gp"),
              ...)
  }


  drawBox("graph_left_top_txt",
          x = unit(2, "mm"),
          y = unit(1, "npc") - unit(2, "mm"),
          hjust = 0,
          vjust = 1)

  drawBox("graph_right_top_txt",
          x = unit(1, "npc") - unit(2, "mm"),
          y = unit(1, "npc") - unit(2, "mm"),
          hjust = 1,
          vjust = 1)

  drawBox("graph_left_bottom_txt",
          x = unit(2, "mm"),
          y = unit(2, "mm"),
          hjust = 0,
          vjust = 0)

  drawBox("graph_right_bottom_txt",
          x = unit(1, "npc") - unit(2, "mm"),
          y = unit(2, "mm"),
          hjust = 1,
          vjust = 0)

  upViewport()
}
