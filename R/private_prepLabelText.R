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
    label_nr <- length(labeltext)
    # Names are retained
    labeltext <- as.list(labeltext)
  } else if (is.data.frame(labeltext)) {
    # If labeltext is a data frame, handle it differently than a generic list
    widthcolumn <- !apply(is.na(labeltext), 1, any)
    nc <- ncol(labeltext)
    label_nr <- nrow(labeltext)
    cn <- colnames(labeltext)
    labeltext <- lapply(seq(nc), function(i) as.list(labeltext[[i]]))
    names(labeltext) <- cn
  } else if (is.list(labeltext)) {
    if (isValidLabelList(labeltext)) {
      labeltext <- list(labeltext)
    }
    labeltext <- sapply(labeltext,
      function(x) {
        if (is.list(x)) {
          return(x)
        }

        return(as.list(x))
      },
      simplify = FALSE,
      USE.NAMES = TRUE
    )

    if (!prFpValidateLabelList(labeltext)) {
      stop("Invalid labellist, it has to be formed as a matrix m x n elements")
    }

    # Can't figure out multiple levels of expressions
    nc <- length(labeltext)

    widthcolumn <- c()
    # Should mark the columns that don't contain
    # expressions, text or numbers as width columns
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

    label_nr <- length(labeltext[[1]])
  } else if (is.vector(labeltext)) {
    widthcolumn <- c(FALSE)
    nc <- 1
    label_nr <- length(labeltext)

    labeltext <- list(as.list(labeltext))
  } else {
    # Original code for matrixes
    widthcolumn <- !apply(is.na(labeltext), 1, any)
    nc <- NCOL(labeltext)
    label_nr <- NROW(labeltext)
    label_colnames <- colnames(labeltext)
    labeltext <- (\(x) lapply(
      seq(NCOL(labeltext)),
      function(i) as.list(x[, i])
    ))(labeltext)
    names(labeltext) <- label_colnames
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
    class = "forestplot_labeltext"
  )
}

# Helper function to validate if all elements of a list are atomic (not lists) and of length 1
isValidLabelList <- function(listData) {
  sapply(listData, function(x) length(x) == 1 && !is.list(x)) |> all()
}

#' @describeIn prepLabelText Pick the value that corresponds to the row and column.
#'  Returns `expression`, `call`, or `text`.
#' @param x A `forestplot_labeltext` object
#' @param i The row
#' @param j The column
#'
#' @inheritParams forestplot
#' @keywords internal
`[.forestplot_labeltext` <- function(x, i, j, ...) {
  # I get annoying warnings with this
  # if (!is.expression(x[[j]][[i]]) && is.na(x[[j]][[i]]))
  #    return(FALSE)
  row_column_text <- x[[j]][[i]]

  if (!is.expression(row_column_text) &&
    !is.call(row_column_text) &&
    (is.na(row_column_text) ||
      is.null(row_column_text))) {
    return("")
  }

  return(row_column_text)
}
