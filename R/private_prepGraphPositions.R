#' Prepares graph position
#'
#' Prepares the graph position so that it matches the label size
#'
#' @param nc The number of columns
#' @inheritParams forestplot
#'
#' @return Returns number indicating the graph position
prepGraphPositions <- function(graph.pos, nc) {
  if (is.character(graph.pos)) {
    return(switch(graph.pos,
                  right = nc + 1L,
                  last = nc + 1L,
                  left = 1L,
                  first = 1L,
                  stop(
                    "The graph.pos argument has an invalid text argument.",
                    " The only values accepted are 'left'/'right' or 'first'/'last'.",
                    " You have provided the value '", graph.pos, "'")))
  }

  if (is.numeric(graph.pos)) {
    if (!graph.pos %in% 1:(nc + 1)) {
      stop(
        "The graph position must be between 1 and ", (nc + 1), ".",
        " You have provided the value '", graph.pos, "'."
      )
    }
    return(graph.pos)
  }

  stop("The graph pos must either be a string consisting of 'left'/'right' (alt. 'first'/'last')",
       ", or an integer value between 1 and ", (nc + 1))
}
