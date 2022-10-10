#' Draws the horizontal lines
#'
#' @param number_of_rows Number of rows
#' @param colwidths Vector with column widths
#' @inheritParams prepLines
#' @inheritParams forestplot
#' @keywords internal
drawHorizontalLines <- function(lines,
                                number_of_rows,
                                colwidths,
                                graph.pos) {
  getCSpan <- function(columns, colwidths) {
    span_cols <- c()
    col_pos <- NULL
    for (i in 1:length(columns)) {
      pos <- columns[i]
      pos <- pos * 2 - 1
      span_cols <- c(span_cols, pos)

      if (pos < length(colwidths) &&
          i != length(columns) &&
          columns[i] + 1 == columns[i + 1]) {
        span_cols <- c(span_cols, pos + 1)
      }
    }

    span_cols
  }

  for (i in 1:number_of_rows) {
    if (!is.null(lines[[i]])) {
      span_cols <- getCSpan(lines[[i]]$columns, colwidths)

      for (c in span_cols) {
        line_vp <- viewport(
          layout.pos.row = i,
          layout.pos.col = c
        )
        pushViewport(line_vp)
        grid.lines(y = unit(c(1, 1), "npc"), gp = lines[[i]])
        popViewport()
      }
    }

    if (i == number_of_rows &&
        !is.null(lines[[i + 1]])) {
      span_cols <- getCSpan(lines[[i + 1]]$columns, colwidths)

      line_vp <- viewport(
        layout.pos.row = i,
        layout.pos.col = span_cols
      )
      pushViewport(line_vp)
      grid.lines(y = unit(c(0, 0), "npc"), gp = lines[[i + 1]])
      popViewport()
    }
  }
}


drawVerticalLines <- function(lines,
                              number_of_columns,
                              number_of_rows,
                              colwidths,
                              graph.pos) {
  for (idx in which(sapply(lines, Negate(is.null)))) {
    col_pos <- 2 * idx - 1
    element <- lines[[idx]]
    rows <- element$rows
    element$rows <- NULL
    if (!inherits(element, "gpar")) {
      element <- do.call(gpar, element)
    }
    if (idx == 1) {
      x <- unit(c(-3, -3), "mm")
      vp <- viewport(
        layout.pos.row = rows,
        layout.pos.col = 1
      )

    } else {
      x <- unit(c(0.5, 0.5), "npc")
      vp <- viewport(
        layout.pos.row = rows,
        layout.pos.col = col_pos - 1
      )
    }
    pushViewport(vp)
    grid.lines(x = x, gp = as.list(element))
    popViewport()
  }
}
