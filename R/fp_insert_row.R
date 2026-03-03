#' Insert/append rows into forestplot
#'
#' These functions are used for inserting or appending
#' a row into a forestplot object. Can be used for inputting multiple
#' rows. Just make sure that all elements are of equal length.
#'
#' @param x The forestplot object
#' @param ... Either named arguments that correspond to the original column
#'  names or unnamed arguments that will map in appearing order.
#' @param mean Either a mean or all the values if three columns (mean, lower, upper)
#' @param lower A vector or matrix with the lower confidence interval
#' @param upper A vector or matrix with the upper confidence interval
#' @param position The row position to input at. Either a row number or "last".
#' @param is.summary Whether the row is a summary.
#' @param boxsize The box size for the drawn estimate line
#'
#' @return The foresplot object with the added rows
#' @export
#'
#' @family graph modifiers
#' @family forestplot functions
#' @example inst/examples/fp_insert_row_example.R
#' @rdname row_manipulation
fp_insert_row <- function(x,
                          ...,
                          mean = NULL, lower = NULL, upper = NULL,
                          position = 1,
                          is.summary = FALSE,
                          boxsize = NA) {
  args <- list(...)
  labels <- pr_coerce_insert_labels(args)
  estimates <- pr_convert_insert_estimates(
    mean = mean,
    lower = lower,
    upper = upper,
    label_length = length(labels[[1]]),
    xlog = x$xlog,
    depth = dim(x$estimates)[3]
  )
  stopifnot(all(nrow(estimates) == sapply(labels, length)))

  if (position == "last") {
    x$estimates <- abind::abind(x$estimates, estimates, along = 1)
  } else {
    x$estimates <- abind::abind(x$estimates[0:(position - 1), , , drop = FALSE],
      estimates,
      x$estimates[position:nrow(x$estimates), , , drop = FALSE],
      along = 1
    )
  }

  if (is.null(labels)) {
    if (length(labels) > attr(x$labels, "no_cols")) {
      stop("Mismatch between number of columns in labels and provided number of columns")
    }
  } else if (is.null(x$labels)) {
    stop(
      "Original data lacks labels and columns, i.e. names ",
      paste(names(labels), collapse = ", "),
      " can't be matched to original labels"
    )
  } else {
    desired_colnames <- names(labels)
    lacking_match <- desired_colnames[!(desired_colnames %in% names(x$labels))]
    if (length(lacking_match) > 0) {
      stop(
        "Unkown label columns ", paste(lacking_match, collapse = ", "),
        " not present among: ", paste(names(x$labels), collapse = ", ")
      )
    }
  }


  for (i in 1:attr(x$labels, "no_cols")) {
    if (is.null(names(labels)) && i > length(labels)) {
      val <- as.list(rep(NA, length.out = nrow(estimates)))
    } else {
      if (is.null(names(labels))) {
        val <- labels[[i]]
      } else {
        n <- names(x$labels)[i]
        val <- labels[[n]]
        if (is.null(val)) {
          val <- list(NA)
        }
      }
    }

    if (position == "last") {
      x$labels[[i]] <- c(x$labels[[i]], val)
    } else {
      x$labels[[i]] <- c(
        x$labels[[i]][0:(position - 1)],
        val,
        x$labels[[i]][position:length(x$labels[[i]])]
      )
    }
  }

  attr(x$labels, "no_rows") <- nrow(x$estimates)

  is.summary <- rep(is.summary, length.out = nrow(estimates))
  if (position == "last") {
    x$is.summary <- c(x$is.summary, is.summary)
  } else {
    x$is.summary <- c(
      x$is.summary[0:(position - 1)],
      is.summary,
      x$is.summary[position:length(x$is.summary)]
    )
  }

  if (!is.null(x$boxsize) && !all(is.na(boxsize))) {
    boxsize <- rep(boxsize, length.out = nrow(estimates))
    if (position == "last") {
      x$boxsize <- c(x$boxsize, boxsize)
    } else {
      x$boxsize <- c(
        x$boxsize[0:(position - 1)],
        boxsize,
        x$boxsize[position:length(x$boxsize)]
      )
    }
  }


  return(x)
}

#' @rdname row_manipulation
#' @export
fp_add_header <- function(x, ..., position = 1, is.summary = TRUE) {
  args <- list(...)
  args <- pr_prepare_header_args(args, existing_label_names = names(x$labels))
  labels <- pr_coerce_insert_labels(args)
  pr_validate_header_span_collisions(
    labels = labels,
    existing_label_names = names(x$labels)
  )

  do.call(
    fp_insert_row,
    c(
      list(
        x = x,
        position = position,
        is.summary = is.summary
      ),
      args
    )
  )
}

#' @rdname row_manipulation
#' @export
fp_append_row <- function(x, ..., position = "last", is.summary = FALSE) {
  fp_insert_row(x, ..., position = position, is.summary = is.summary)
}

pr_convert_insert_estimates <- function(mean, lower, upper, label_length, xlog, depth) {
  stopifnot(is.null(lower) == is.null(upper))
  if (is.null(mean)) {
    return(array(NA, dim = c(label_length, 3, depth), dimnames = list(NULL, c("mean", "lower", "upper"), NULL)))
  }

  if (is.null(lower)) {
    stopifnot(!is.null(dim(mean)) && ncol(mean) == 3)
    if (length(dim(mean)) == 2) {
      mean <- array(mean, dim = c(dim(mean), 1))
    }
    lower <- mean[, 2, , drop = FALSE]
    upper <- mean[, 3, , drop = FALSE]
    mean <- mean[, 1, , drop = FALSE]
  } else {
    stopifnot(all.equal(dim(mean), dim(lower), dim(upper)))
    base_dims <- dim(mean)
    if (is.null(base_dims)) {
      base_dims <- c(1, 1)
    }
    if (length(base_dims) < 3) {
      mean <- array(mean, dim = c(base_dims, 1))
      lower <- array(lower, dim = c(base_dims, 1))
      upper <- array(upper, dim = c(base_dims, 1))
    }
  }

  if (label_length != nrow(mean)) {
    stop("Label length is not equal to values", label_length, " != ", nrow(mean))
  }

  estimates <- abind::abind(mean, lower, upper, along = 2, new.names = list(NULL, c("mean", "lower", "upper"), NULL))
  if (depth != dim(estimates)[3]) {
    stop("Expected the dimension of the estimates to be of ", depth, " and not ", dim(estimates)[3])
  }
  if (xlog) {
    estimates <- log(estimates)
  }
  return(estimates)
}

pr_coerce_insert_labels <- function(args) {
  sapply(args,
    FUN = function(var) {
      if (is.list(var)) {
        return(var)
      }

      if (is.expression(var) || is.character(var)) {
        return(lapply(seq_along(var), \(i) var[i]))
      }

      return(as.list(var))
    },
    simplify = FALSE,
    USE.NAMES = TRUE
  )
}

pr_prepare_header_args <- function(args, existing_label_names) {
  if (length(args) == 0) {
    return(args)
  }

  if (is.null(existing_label_names) || !length(existing_label_names)) {
    return(args)
  }

  arg_names <- names(args)
  if (is.null(arg_names)) {
    arg_names <- rep("", length(args))
  }

  used_cols <- integer(0)
  for (i in seq_along(args)) {
    if (!nzchar(arg_names[i])) {
      next
    }
    pos <- match(arg_names[i], existing_label_names)
    if (!is.na(pos)) {
      used_cols <- c(used_cols, pos)
    }
  }

  next_free_col <- 1L

  for (i in seq_along(args)) {
    if (nzchar(arg_names[i])) {
      next
    }

    values <- pr_coerce_insert_labels(list(args[[i]]))[[1]]
    if (length(values) == 0) {
      stop("Unnamed fp_add_header() entries must provide at least one value")
    }

    span <- attr(values[[1]], "span")
    if (is.null(span)) {
      while (next_free_col %in% used_cols && next_free_col <= length(existing_label_names)) {
        next_free_col <- next_free_col + 1L
      }
      start_col <- next_free_col
      span_cols <- start_col
    } else {
      if (!is.numeric(span) || any(span < 1) || any(span %% 1 != 0)) {
        stop("Invalid 'span' attribute: must be integer column indices")
      }
      span <- sort(unique(as.integer(span)))
      start_col <- min(span)
      span_cols <- seq.int(min(span), max(span))
    }

    if (start_col < 1 || start_col > length(existing_label_names)) {
      stop(
        "The unnamed spanned header starts at column ", start_col,
        " but valid columns are 1 to ", length(existing_label_names), "."
      )
    }

    arg_names[i] <- existing_label_names[start_col]
    used_cols <- c(used_cols, span_cols)
    if (start_col == next_free_col) {
      next_free_col <- next_free_col + 1L
    }
  }

  names(args) <- arg_names
  args
}

pr_validate_header_span_collisions <- function(labels, existing_label_names) {
  if (length(labels) == 0) {
    return(invisible(NULL))
  }

  if (is.null(existing_label_names) || !length(existing_label_names)) {
    return(invisible(NULL))
  }

  if (is.null(names(labels))) {
    return(invisible(NULL))
  }

  label_positions <- stats::setNames(seq_along(existing_label_names), existing_label_names)

  rows_n <- max(sapply(labels, length))

  for (row_idx in seq_len(rows_n)) {
    spans <- list()
    span_names <- character(0)

    for (label_name in names(labels)) {
      col_pos <- label_positions[[label_name]]
      if (!nzchar(label_name) || is.null(col_pos) || is.na(col_pos)) {
        next
      }

      values <- labels[[label_name]]
      if (length(values) < row_idx) {
        next
      }

      value <- values[[row_idx]]
      span <- attr(value, "span")
      if (is.null(span)) {
        span <- col_pos
      }

      span <- sort(unique(as.integer(span)))
      spans[[length(spans) + 1]] <- c(min(span), max(span))
      span_names <- c(span_names, label_name)
    }

    if (length(spans) < 2) {
      next
    }

    starts <- vapply(spans, \(x) x[1], integer(1))
    ends <- vapply(spans, \(x) x[2], integer(1))
    ord <- order(starts, ends)
    starts <- starts[ord]
    ends <- ends[ord]
    span_names <- span_names[ord]

    for (idx in seq_len(length(starts) - 1)) {
      current_end <- ends[idx]
      next_start <- starts[idx + 1]
      max_allowed <- next_start - 1

      if (current_end >= next_start) {
        stop(
          "Header span collision on inserted row ", row_idx,
          ": column '", span_names[idx], "' spans through column ", current_end,
          " but the next spanned column '", span_names[idx + 1],
          "' starts at ", next_start,
          ". Columns are too close/overlapping; max allowed end for '",
          span_names[idx], "' is ", max_allowed, "."
        )
      }
    }
  }

  invisible(NULL)
}

if (FALSE) {
  base_data |>
    forestplot(
      labeltext = c(study, deaths_steroid, deaths_placebo, OR),
      clip = c(0.1, 2.5),
      xlog = TRUE,
      col = fpColors(
        box = "royalblue",
        line = "darkblue",
        summary = "royalblue"
      )
    ) |>
    fp_insert_row(c("asdasd", "Asd"))
}
