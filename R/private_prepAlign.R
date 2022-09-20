#' Prepares graph position
#'
#' Prepares the graph position so that it matches the label size
#'
#' @param nc The number of columns
#' @param graph.pos An integer indicating the position of the graph
#' @inheritParams forestplot
#'
#' @return Returns vector of `"l", "c", "r"` values
prepAlign <- function(align, graph.pos, nc) {
  # Prepare the summary and align variables
  if (is.null(align)) {
    if (graph.pos == 1) {
      return(rep("l", nc))
    }

    if (graph.pos == nc + 1) {
      return(c("l", rep("r", nc - 1)))
    }

    return(c("l", rep("c", nc - 1)))
  }

  if (any(!c("l", "c", "r") %in% align)) {
    stop("The align argument must only contain 'l', 'c', or 'r'. You provided: ", align)
  }
  rep(align, length.out = nc)
}
