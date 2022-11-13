#' Adds a line to the graph
#'
#' Adds a line to the graph, defaults to horizontal. Lines with prefix `v_` will be vertical, no prefix or `h_` will be horizontal.
#' If argument is `TRUE` or just empty: A line will be added based upon the \code{is.summary} rows. If the first line is a summary it
#' will choose the last non-summary row.
#'
#' If you provide the argument as a number it will add the line to that particular line. 1 corresponds to the
#' top row and the max row is `num_rows + 1`. If the argument is `TRUE` it will default to a standard line.
#' A string will default to the color of that string. If you provide a [grid::gpar] element it will style
#' the line according to the `gpar object`. Apart from allowing standard \code{\link[grid]{gpar}} line descriptions,
#' `lty`, `lwd`, `col`, and more you can also specify `gpar(columns = c(1:3, 5))` if you for instance want
#' the line to skip a column.
#'
#' If you want to add mix vertical and horizontal lines you can prefix the lines with `h_` and `v_`, e.g. `v_2` for the
#' second column.
#'
#' @inheritParams fp_insert_row
#' @param ... A set of arguments. Can either be \code{TRUE} or a set of
#'  numbered arguments with \code{\link[grid]{gpar}}. See line section
#'  below for details.
#'
#' @return The foresplot object with the styles
#' @export
#'
#' @example inst/examples/fp_add_lines_example.R
#' @family graph modifiers
#' @family forestplot functions
fp_add_lines <- function(x,
                        ...) {
  args <- list(...) |>
    sapply(\(x) {
      if (is.character(x)) {
        return(gpar(col = x))
      }

      return(x)
    },
    simplify = FALSE)
  if (length(args) == 1 && isTRUE(args[[1]]) || length(args) == 0) {
    x$lines$horizontal <- TRUE
  } else if (length(args) == 1 && inherits(args[[1]], "gpar") && is.null(names(args))) {
    x$lines$horizontal <- args[[1]]
  } else {
    if (is.null(names(args))) {
      names(args) <- 1:length(args)
    } else {
      invalid_names <- names(args)[!grepl("^(h_|v_|)\\d+$", names(args))]
      if (length(invalid_names) > 0) {
        stop("Unused arguments: ", paste(invalid_names, collapse = ", "))
      }
    }

    appendArgs <- function(new_args, org_args) {
      if (length(new_args) == 0) {
        return(org_args)
      }

      if (is.null(org_args) || !is.list(org_args)) {
        return(new_args)
      }

      for (n in names(new_args)) {
        org_args[[n]] <- new_args[[n]]
      }

      return(org_args)
    }

    x$lines$horizontal <- args[grep("^(h_\\d+|\\d+)", names(args))] |>
      (\(x) {
        names(x) <- gsub("^h_", "", names(x))
        return(x)
      })() |>
      appendArgs(org_args = x$lines$horizontal)

    x$lines$vertical <- args[grep("^v_", names(args))] |>
      (\(x) {
        names(x) <- gsub("^v_", "", names(x))
        return(x)
      })() |>
      appendArgs(org_args = x$lines$vertical)
  }

  return(x)
}

