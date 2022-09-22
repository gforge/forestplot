#' Prepares label text
#'
#' Prepares an object that contains the number of columns and rows
#'
#' @param labeltext The label text input, either `expression`, `list`
#'  `vector` or `matrix`
#' @param nr The number of rows
#'
#' @return Returns a `forestplot_labeltext` object with attributes:
#'  - no_cols
#'  - no_rows
#'  - widthcolumn
#'  - label_type
#' @rdname prepLabelText
prepLabelText <- function(labeltext, nr) {
  # Get the number of columns (nc) and number of rows (nr)
  # if any columns are to be spacers the widthcolumn variable
  if (is.expression(labeltext)) {
    widthcolumn <- c(TRUE)
    # Can't figure out multiple levels of expressions
    nc <- 1
    label_type <- "expression"
    label_nr <- length(labeltext)
  } else if (is.list(labeltext)) {
    if (sapply(labeltext, \(x)  length(x) == 1 && !is.list(x)) |> all()) {
      labeltext <- list(labeltext)
    }

    if (!prFpValidateLabelList(labeltext)) {
      stop("Invalid labellist, it has to be formed as a matrix m x n elements")
    }

    # Can't figure out multiple levels of expressions
    nc <- length(labeltext)

    widthcolumn <- c()
    # Should mark the columns that don't contain
    # epressions, text or numbers as widthcolumns
    for (col.no in seq(along = labeltext)) {
      empty_row <- TRUE
      for (row.no in seq(along = labeltext[[col.no]])) {
        if (is.expression(labeltext[[col.no]][[row.no]]) ||
            !is.na(labeltext[[col.no]][[row.no]])) {
          empty_row <- FALSE
          break
        }
      }
      widthcolumn <- append(widthcolumn, empty_row)
    }

    label_type <- "list"
    label_nr <- length(labeltext[[1]])
  } else if (is.vector(labeltext)) {
    widthcolumn <- c(FALSE)
    nc <- 1

    labeltext <- matrix(labeltext, ncol = 1)
    label_type <- "matrix"
    label_nr <- NROW(labeltext)
  } else {
    # Original code for matrixes
    widthcolumn <- !apply(is.na(labeltext), 1, any)
    nc <- NCOL(labeltext)
    label_type <- "matrix"
    label_nr <- NROW(labeltext)
  }

  if (nr != label_nr) {
    stop(
      "You have provided ", nr, " rows in your",
      " mean arguement while the labels have ", label_nr, " rows"
    )
  }

  structure(labeltext,
            no_cols = nc,
            no_rows = label_nr,
            widthcolumn = widthcolumn,
            label_type = label_type,
            class = "forestplot_labeltext")
}

#' @describeIn prepLabelText Pick the value that corresponds to the row and column.
#'  Returns `expression`, `call`, or `text`.
#' @param x A `forestplot_labeltext` object
#' @param i The row
#' @param j The column
#'
#' @inheritParams forestplot
#' @keywords internal
`[.forestplot_labeltext` <- function(x, i, j, ...)
{
  label_type <- attr(x, "label_type")
  if (label_type == "expression") {
    # Haven't figured out it this is possible with
    # a multilevel expression
    row_column_text <- x[[i]]
  } else if (label_type == "list") {
    # I get annoying warnings with this
    # if (!is.expression(x[[j]][[i]]) && is.na(x[[j]][[i]]))
    #    return(FALSE)
    row_column_text <- x[[j]][[i]]
  } else {
    ret <- NextMethod()
    if (is.na(ret)) {
      return(FALSE)
    }
    row_column_text <- ret
  }

  if (!is.expression(row_column_text) &&
      !is.call(row_column_text) &&
      (is.na(row_column_text) ||
       is.null(row_column_text))) {
    return("")
  }

  return(row_column_text)
}
