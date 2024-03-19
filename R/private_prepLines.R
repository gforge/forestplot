#' Prepares the lines for the plot
#'
#' @param number_of_columns Total number of columns
#' @param number_of_rows Total number of rows
#' @inheritParams forestplot
#' @keywords internal
#' @importFrom utils tail
prepLines <- function(lines,
                      is.summary,
                      number_of_rows,
                      number_of_columns,
                      col,
                      shapes_gp) {
  is.summary <- is.summary[1:number_of_rows]
  list(horizontal = getHorizontalLines(lines = lines$horizontal,
                                       is.summary = is.summary,
                                       number_of_columns = number_of_columns,
                                       number_of_rows = number_of_rows,
                                       col = col,
                                       shapes_gp = shapes_gp),
       vertical = getVerticaLines(lines = lines$vertical,
                                  is.summary = is.summary,
                                  number_of_columns = number_of_columns,
                                  number_of_rows = number_of_rows,
                                  col = col,
                                  shapes_gp = shapes_gp),
       number_of_rows = number_of_rows,
       number_of_columns = number_of_columns) |>
    structure(class = c("gforge_forestplot_lines", class(list())))
}

#' @export
plot.gforge_forestplot_lines <- function(x,
                                         colwidths,
                                         graph.pos,
                                         ...) {

  drawVerticalLines(lines = x$vertical,
                    number_of_rows = x$number_of_rows,
                    colwidths = colwidths,
                    graph.pos = graph.pos)

  drawHorizontalLines(lines = x$horizontal,
                      number_of_rows = x$number_of_rows,
                      colwidths = colwidths,
                      graph.pos = graph.pos)
}

getHorizontalLines <- function(lines,
                               is.summary,
                               number_of_columns,
                               number_of_rows,
                               col,
                               shapes_gp) {
  ret_hrzl_lines <- lapply(1:(number_of_rows + 1), function(x) NULL)
  if (is.null(lines) ||
      (is.logical(lines) &&
       all(lines == FALSE)) ||
      (is.list(lines) &&
       all(sapply(lines, is.null)))) {
    return(ret_hrzl_lines)
  }

  std_line <- gpar(lty = 1, lwd = 1, col = col$hrz_lines, columns = 1:number_of_columns)
  std_line <- prGetShapeGp(shapes_gp, NULL, "hrz_lines", default = std_line)

  if (inherits(lines, "gpar")) {
    std_line <- prGparMerge(std_line, lines)
    lines <- TRUE
  }

  # If provided with TRUE alone
  # Note that FALSE has already been processed above
  if (is.logical(lines) &&
      length(lines) == 1) {
    if (is.summary[1] == TRUE) {
      line_pos <- which(is.summary == FALSE)[1]
      ret_hrzl_lines[[line_pos]] <-
        std_line

      is.summary[1:line_pos] <- FALSE
    }

    if (tail(is.summary, 1)) {
      line_pos <- number_of_rows + 1 - (which(rev(is.summary) == FALSE)[1] - 1)

      ret_hrzl_lines[[line_pos]] <-
        std_line

      ret_hrzl_lines[[length(ret_hrzl_lines)]] <-
        std_line

      is.summary[line_pos:number_of_rows] <- FALSE
    }

    for (line_pos in which(is.summary == TRUE)) {
      if (is.summary[line_pos + 1]) {
        line_pos <-
          line_pos +
          tail(which(is.summary[(line_pos + 1):number_of_rows]), 1)
      }
      ret_hrzl_lines[[line_pos + 1]] <-
        std_line
    }

    return(ret_hrzl_lines)
  }

  if (is.logical(lines)) {
    if (length(lines) == (number_of_rows + 1)) {
      ret_hrzl_lines[[lines]] <-
        std_line
      return(ret_hrzl_lines)
    } else {
      stop(
        "You have provided a logical horizontal lines input of length '", length(lines), "'",
        " but the software expects the length to be number of rows + 1",
        " i.e. ", number_of_rows, " + 1 = ", number_of_rows + 1
      )
    }
  }

  if (!is.list(lines)) {
    stop("You have provided an invalid argument, expected a list but got a ", class(lines))
  }

  if (is.null(names(lines))) {
    if (length(lines) == (number_of_rows + 1)) {
      return(lapply(lines, function(x, std) {
        if (is.null(x)) {
          x
        } else if (inherits(x, "gpar")) {
          prGparMerge(std, x)
        } else {
          std
        }
      },
      std = std_line
      ))
    } else {
      stop(
        "You have provided a logical horizontal lines input of length '", length(lines), "'",
        " but the software expects the length to be number of rows + 1",
        " i.e. ", number_of_rows, " + 1 = ", number_of_rows + 1
      )
    }
  }

  if (!all(sapply(lines, function(x) inherits(x, "gpar") || x == TRUE))) {
    stop("The list must consist of only gpar or logical TRUE elements")
  }

  for (n in names(lines)) {
    nn <- as.integer(n)
    if (is.na(nn)) {
      stop("Your name '", n, "' for the list gpars cannot be converted to an integer")
    }
    if (!nn %in% 1:(number_of_rows + 1)) {
      stop(
        "The integer that you have provided '", n, "'",
        " falls outside the scope of possible values 1:", number_of_rows + 1
      )
    }
    if (is.logical(lines[[n]])) {
      ret_hrzl_lines[[nn]] <-
        std_line
    } else {
      ret_hrzl_lines[[nn]] <-
        prGparMerge(std_line, lines[[n]])
    }
  }

  return(ret_hrzl_lines)
}

getVerticaLines <- function(lines,
                            is.summary,
                            number_of_rows,
                            number_of_columns,
                            col,
                            shapes_gp) {
  ret_vrtcl_lines <- lapply(1:(number_of_columns + 1), function(x) NULL)
  if (is.null(lines)) {
    return(ret_vrtcl_lines)
  }

  # The first non-summary line
  default_start_pos <- which(is.summary == FALSE)[1]
  std_line <- gpar(lty = 1, lwd = 1, col = col$vrcl_lines, rows = default_start_pos:number_of_rows)
  std_line <- prGetShapeGp(shapes_gp, NULL, "vrtcl_lines", default = std_line)

  if (inherits(lines, "gpar")) {
    std_line <- prGparMerge(std_line, lines)
    lines <- TRUE
  }

  # If provided with TRUE alone
  # Note that FALSE has already been processed above
  if (is.logical(lines) &&
      length(lines) == 1) {
    if (is.summary[1] == TRUE) {
      line_pos <- which(is.summary == FALSE)[1]
      ret_vrtcl_lines[[line_pos]] <-
        std_line

      is.summary[1:line_pos] <- FALSE
    }

    if (tail(is.summary, 1)) {
      line_pos <- number_of_rows + 1 - (which(rev(is.summary) == FALSE)[1] - 1)

      ret_vrtcl_lines[[line_pos]] <-
        std_line

      ret_vrtcl_lines[[length(ret_vrtcl_lines)]] <-
        std_line

      is.summary[line_pos:number_of_rows] <- FALSE
    }

    for (line_pos in which(is.summary == TRUE)) {
      if (is.summary[line_pos + 1]) {
        line_pos <-
          line_pos +
          tail(which(is.summary[(line_pos + 1):number_of_rows]), 1)
      }
      ret_vrtcl_lines[[line_pos + 1]] <-
        std_line
    }

    return(ret_vrtcl_lines)
  }

  if (is.logical(lines)) {
    if (length(lines) == (number_of_rows + 1)) {
      ret_vrtcl_lines[[lines]] <-
        std_line
      return(ret_vrtcl_lines)
    } else {
      stop(
        "You have provided a logical lines input of length '", length(lines), "'",
        " but the software expects the length to be number of rows + 1",
        " i.e. ", number_of_rows, " + 1 = ", number_of_rows + 1
      )
    }
  }

  if (!is.list(lines)) {
    stop("You have provided an invalid argument, expected a list but got a ", class(lines))
  }

  if (is.null(names(lines))) {
    if (length(lines) == (number_of_columns + 1)) {
      return(lapply(lines, function(x, std) {
        if (is.null(x)) {
          x
        } else if (inherits(x, "gpar")) {
          prGparMerge(std, x)
        } else {
          std
        }
      },
      std = std_line
      ))
    } else {
      stop(
        "You have provided a logical lines input of length '", length(lines), "'",
        " but the software expects the length to be number of columns + 1",
        " i.e. ", number_of_columns, " + 1 = ", number_of_columns + 1
      )
    }
  }

  if (!all(sapply(lines, function(x) inherits(x, "gpar") || x == TRUE))) {
    stop("The list must consist of only gpar or logical TRUE elements")
  }

  for (n in names(lines)) {
    nn <- as.integer(n)
    if (is.na(nn)) {
      stop("Your name '", n, "' for the list gpars cannot be converted to an integer")
    }
    if (!nn %in% 1:(number_of_columns + 1)) {
      stop(
        "The integer that you have provided '", n, "'",
        " falls outside the scope of possible values 1:", number_of_columns + 1
      )
    }
    if (is.logical(lines[[n]])) {
      ret_vrtcl_lines[[nn]] <-
        std_line
    } else {
      ret_vrtcl_lines[[nn]] <-
        prGparMerge(std_line, lines[[n]])
    }
  }

  return(ret_vrtcl_lines)
}
