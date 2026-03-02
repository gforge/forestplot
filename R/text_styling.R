#' Text styling
#'
#' This is a collection of functions to allow styling of text
#'
#' @param txt The text to styl
#' @returns A list of txt with style attributes
#'
#' @examples
#' fp_txt_italic("Italic text")
#' @export
#' @rdname text_styling
fp_txt_italic <- function(txt) {
  sapply(txt, \(str) {
    txt_gp <- attr(str, "txt_gp")
    if (is.null(txt_gp)) {
      txt_gp <- gpar()
    }
    txt_gp$fontface <- "italic"
    attr(str, "txt_gp") <- txt_gp
    return(str)
  },
  simplify = FALSE,
  USE.NAMES = FALSE
  )
}

#' @export
#' @rdname text_styling
fp_txt_bold <- function(txt) {
  sapply(txt, \(str) {
    txt_gp <- attr(str, "txt_gp")
    if (is.null(txt_gp)) {
      txt_gp <- gpar()
    }
    txt_gp$fontface <- "bold"
    attr(str, "txt_gp") <- txt_gp
    return(str)
  },
  simplify = FALSE,
  USE.NAMES = FALSE
  )
}

#' @export
#' @rdname text_styling
fp_txt_plain <- function(txt) {
  sapply(txt, \(str) {
    txt_gp <- attr(str, "txt_gp")
    if (is.null(txt_gp)) {
      txt_gp <- gpar()
    }
    txt_gp$fontface <- "plain"
    attr(str, "txt_gp") <- txt_gp
    return(str)
  },
  simplify = FALSE,
  USE.NAMES = FALSE
  )
}

#' @export
#' @rdname text_styling
#' @param gp A [grid::gpar()] style to apply
fp_txt_gp <- function(txt, gp) {
  sapply(txt, \(str) {
    txt_gp <- attr(str, "txt_gp")
    if (is.null(txt_gp)) {
      txt_gp <- gpar()
    }
    for (n in names(gp)) {
      txt_gp[[n]] <- gp[[n]]
    }

    attr(str, "txt_gp") <- txt_gp
    return(str)
  },
  simplify = FALSE,
  USE.NAMES = FALSE
  )
}

#' @export
#' @rdname text_styling
fp_align_left <- function(txt) {
  sapply(txt, \(str) {
    attr(str, "align") <- "l"
    return(str)
  },
  simplify = FALSE,
  USE.NAMES = FALSE
  )
}

#' @export
#' @rdname text_styling
fp_align_center <- function(txt) {
  sapply(txt, \(str) {
    attr(str, "align") <- "c"
    return(str)
  },
  simplify = FALSE,
  USE.NAMES = FALSE
  )
}

#' @export
#' @rdname text_styling
#' @param columns Integer vector of column indices to span. The span covers the
#'   continuous range between the smallest and largest values. Values are
#'   validated when the plot is built; they must lie between 1 and the number of
#'   label columns and may not include the graph column.
#'
#' @description
#' Apply a column span to a text element. The text will be printed in a
#' grid viewport whose layout.pos.col spans the specified columns. If called
#' multiple times on the same object, the most recent call overwrites the
#' previous span (i.e. last call wins).
#'
#' @examples
#' # centre "Events / N" under columns 2 and 3
#' fp_span("Events / N", columns = c(2, 3))
#'
fp_span <- function(txt, columns) {
  sapply(txt, \(str) {
    if (!is.numeric(columns) || any(columns < 1) || any(columns %% 1 != 0)) {
      stop("'columns' must be an integer vector")
    }
    attr(str, "span") <- as.integer(columns)
    return(str)
  },
  simplify = FALSE,
  USE.NAMES = FALSE
  )
}

#' @export
#' @rdname text_styling
fp_align_right <- function(txt) {
  sapply(txt, \(str) {
    attr(str, "align") <- "r"
    return(str)
  },
  simplify = FALSE,
  USE.NAMES = FALSE
  )
}


merge_with_txt_gp <- function(gp_list, txt_out) {
  txt_gp <- attr(txt_out, "txt_gp")
  if (is.null(txt_gp)) {
    return(gp_list)
  }

  for (n in names(txt_gp)) {
    gp_list[[n]] <- txt_gp[[n]]
  }

  return(gp_list)
}


fpAssertPlotObject <- function(x) {
  if (!inherits(x, "gforge_forestplot")) {
    stop("This helper requires a gforge_forestplot object. Pipe from forestplot(...)")
  }
}

fpBuildSelectorData <- function(labels) {
  nr <- attr(labels, "no_rows")
  nc <- attr(labels, "no_cols")
  cols <- vector("list", nc)
  nm <- names(labels)
  if (is.null(nm)) {
    nm <- paste0("V", seq_len(nc))
  }

  for (j in seq_len(nc)) {
    vals <- lapply(seq_len(nr), function(i) labels[[j]][[i]])
    is_atomic_scalar <- vapply(vals, function(z) {
      is.atomic(z) && length(z) == 1 && !is.expression(z) && !is.call(z)
    }, logical(1))
    if (all(is_atomic_scalar)) {
      cols[[j]] <- unlist(vals)
    } else {
      cols[[j]] <- I(vals)
    }
  }

  out <- as.data.frame(cols,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  names(out) <- nm
  out
}

fpResolveRows <- function(x, rows = NULL, where = NULL) {
  labels <- x$labels
  nr <- attr(labels, "no_rows")
  idx <- seq_len(nr)

  if (!is.null(rows)) {
    if (is.logical(rows)) {
      if (length(rows) != nr) {
        stop("Logical 'rows' selector must have length ", nr)
      }
      idx <- which(rows)
    } else if (is.numeric(rows)) {
      rows <- as.integer(rows)
      if (any(rows < 1 | rows > nr)) {
        stop("Row selector out of bounds. Valid rows are 1 to ", nr)
      }
      idx <- unique(rows)
    } else {
      stop("'rows' must be NULL, numeric indices, or a logical vector")
    }
  }

  if (!is.null(where)) {
    selector_data <- fpBuildSelectorData(labels)
    pred <- if (is.function(where)) {
      where(selector_data)
    } else if (inherits(where, "formula")) {
      eval(where[[2]], envir = selector_data, enclos = parent.frame())
    } else {
      stop("'where' must be a function or one-sided formula, e.g. ~ type == 'subtotal'")
    }

    if (!is.logical(pred) || length(pred) != nrow(selector_data)) {
      stop("'where' must evaluate to a logical vector with length equal to no_rows")
    }
    idx <- intersect(idx, which(pred))
  }

  idx
}

fpResolveCols <- function(x, cols = NULL) {
  labels <- x$labels
  nc <- attr(labels, "no_cols")

  if (is.null(cols)) {
    return(seq_len(nc))
  }

  if (is.logical(cols)) {
    if (length(cols) != nc) {
      stop("Logical 'cols' selector must have length ", nc)
    }
    return(which(cols))
  }

  if (is.numeric(cols)) {
    cols <- as.integer(cols)
    if (any(cols < 1 | cols > nc)) {
      stop("Column selector out of bounds. Valid columns are 1 to ", nc)
    }
    return(unique(cols))
  }

  if (is.character(cols)) {
    label_names <- names(labels)
    if (is.null(label_names)) {
      stop("Character column selectors require named label columns")
    }
    pos <- match(cols, label_names)
    if (any(is.na(pos))) {
      stop("Unknown column selector(s): ", paste(cols[is.na(pos)], collapse = ", "))
    }
    return(unique(as.integer(pos)))
  }

  stop("'cols' must be NULL, numeric indices, logical vector, or column names")
}

fpApplyCellTransform <- function(x, rows = NULL, cols = NULL, where = NULL, transform) {
  fpAssertPlotObject(x)
  row_idx <- fpResolveRows(x, rows = rows, where = where)
  col_idx <- fpResolveCols(x, cols = cols)

  for (j in col_idx) {
    for (i in row_idx) {
      x$labels[[j]][[i]] <- transform(x$labels[[j]][[i]])
    }
  }

  x
}

#' Selector-based summary rows
#'
#' Set `is.summary` rows on a `gforge_forestplot` object using a row predicate.
#' This helper is intended for pipe-first workflows where summary rows are
#' decided after label extraction/remapping.
#'
#' @param x A `gforge_forestplot` object.
#' @param where A row predicate. Supports:
#'   * a bare expression evaluated in source data columns (when available), e.g.
#'   `startsWith(type, "Subtotal") | startsWith(type, "Total")`
#'   * a one-sided formula, e.g. `~ Type %in% c("header", "subtotal")`
#'   * a function receiving selector data and returning a logical vector
#'   * a logical scalar/vector
#'
#' @return A modified `gforge_forestplot` object.
#' @examples
#' data(inventors_vs_mello)
#' inventors_vs_mello |>
#'   forestplot(mean = est, lower = lb, upper = ub, labeltext = author) |>
#'   fp_extract_labels(Study = author) |>
#'   fp_set_summary(startsWith(type, "subtotal") | startsWith(type, "total"))
#' @export
fp_set_summary <- function(x, where) {
  fpAssertPlotObject(x)

  selector_data <- x$extra_arguments$.fp_data
  if (!is.null(selector_data) && inherits(selector_data, "grouped_df")) {
    selector_data <- dplyr::ungroup(selector_data)
  }

  if (is.null(selector_data) || nrow(selector_data) != attr(x$labels, "no_rows")) {
    selector_data <- fpBuildSelectorData(x$labels)
  }

  nr <- nrow(selector_data)

  where_quo <- rlang::enquo(where)
  if (rlang::quo_is_missing(where_quo)) {
    stop("Provide a row predicate, e.g. fp_set_summary(startsWith(Type, 'Subtotal'))")
  }

  pred <- rlang::eval_tidy(where_quo, data = selector_data)
  if (is.function(pred)) {
    pred <- pred(selector_data)
  } else if (inherits(pred, "formula")) {
    pred <- eval(pred[[2]], envir = selector_data, enclos = parent.frame())
  }

  if (!is.logical(pred)) {
    stop("'where' must evaluate to a logical value/vector")
  }

  if (length(pred) == 1L) {
    pred <- rep(pred, nr)
  }

  if (length(pred) != nr) {
    stop("'where' must evaluate to length ", nr, " (or a length-1 logical)")
  }

  pred[is.na(pred)] <- FALSE
  x$is.summary <- pred
  x
}

#' Selector-based alignment
#'
#' Apply alignment to selected cells in a `gforge_forestplot` object.
#' This is intended for pipe-first workflows where styling is applied after
#' building the plot object.
#'
#' @param x A `gforge_forestplot` object.
#' @param align One of "l", "c", or "r".
#' @param rows Optional row selector (`NULL`, numeric indices, or logical vector).
#' @param cols Optional column selector (`NULL`, numeric indices, logical vector,
#'   or column names).
#' @param where Optional row predicate supplied as a function or one-sided formula,
#'   e.g. `~ type %in% c("header", "subtotal")`.
#'
#' @return A modified `gforge_forestplot` object.
#' @export
fp_align_where <- function(x,
                           align,
                           rows = NULL,
                           cols = NULL,
                           where = NULL) {
  if (!align %in% c("l", "c", "r")) {
    stop("'align' must be one of 'l', 'c', 'r'")
  }

  fpApplyCellTransform(
    x = x,
    rows = rows,
    cols = cols,
    where = where,
    transform = function(cell) {
      attr(cell, "align") <- align
      cell
    }
  )
}

#' Selector-based text gp
#'
#' Apply [grid::gpar()] settings to selected cells in a `gforge_forestplot` object.
#'
#' @inheritParams fp_align_where
#' @param gp A [grid::gpar()] style to apply.
#'
#' @return A modified `gforge_forestplot` object.
#' @export
fp_txt_where <- function(x,
                         gp,
                         rows = NULL,
                         cols = NULL,
                         where = NULL) {
  fpApplyCellTransform(
    x = x,
    rows = rows,
    cols = cols,
    where = where,
    transform = function(cell) {
      txt_gp <- attr(cell, "txt_gp")
      if (is.null(txt_gp)) {
        txt_gp <- gpar()
      }
      for (n in names(gp)) {
        txt_gp[[n]] <- gp[[n]]
      }
      attr(cell, "txt_gp") <- txt_gp
      cell
    }
  )
}

#' Selector-based spanning
#'
#' Apply span attributes to selected cells in a `gforge_forestplot` object.
#'
#' @inheritParams fp_align_where
#' @param columns Integer vector of target columns to span.
#'
#' @return A modified `gforge_forestplot` object.
#' @export
fp_span_where <- function(x,
                          columns,
                          rows = NULL,
                          cols = NULL,
                          where = NULL) {
  if (!is.numeric(columns) || any(columns < 1) || any(columns %% 1 != 0)) {
    stop("'columns' must be an integer vector")
  }

  fpApplyCellTransform(
    x = x,
    rows = rows,
    cols = cols,
    where = where,
    transform = function(cell) {
      attr(cell, "span") <- as.integer(columns)
      cell
    }
  )
}
