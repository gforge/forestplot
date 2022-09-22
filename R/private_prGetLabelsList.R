#' Gets the forestplot labels
#'
#' A function that gets all the labels
#'
#' @param labels A `forestplot_labeltext` object
#' @param align Alignment, should be equal to \code{attr(labels, "no_cols")}
#' @return \code{list} A list with \code{attr(labels, "no_cols")} where each element contains
#'  a list of \code{attr(labels, "no_rows")} elements with attributes width/height for each
#'  element and max_width/max_height for the total
#'
#' @inheritParams forestplot
#' @keywords internal
prGetLabelsList <- function(labels,
                            align,
                            is.summary,
                            txt_gp,
                            col) {
  if (attr(txt_gp$label, "txt_dim") %in% 0:1) {
    txt_gp$label <- prListRep(list(prListRep(txt_gp$label, attr(labels, "no_cols"))), sum(!is.summary))
  } else {
    ncols <- sapply(txt_gp$label, length)
    if (all(ncols != ncols[1])) {
      stop(
        "Your fpTxtGp$label list has invalid number of columns",
        ", they should all be of equal length - yours have ",
        "'", paste(ncols, collapse = "', '"), "'"
      )
    }
    if (length(txt_gp$label) != sum(!is.summary)) {
      stop(
        "Your fpTxtGp$label list has invalid number of rows",
        ", the should be equal the of the number rows that aren't summaries.",
        " you have '", length(txt_gp$label), "' rows in the fpTxtGp$label",
        ", while the labeltext argument has '", attr(labels, "no_rows"), "' rows",
        " where '", sum(!is.summary), "' are not summaries."
      )
    }
  }

  if (attr(txt_gp$summary, "txt_dim") %in% 0:1) {
    txt_gp$summary <-
      prListRep(list(prListRep(txt_gp$summary, attr(labels, "no_cols"))), sum(is.summary))
  } else {
    ncols <- sapply(txt_gp$summary, length)
    if (all(ncols != ncols[1])) {
      stop(
        "Your fpTxtGp$summary list has invalid number of columns",
        ", they should all be of equal length - yours have ",
        "'", paste(ncols, collapse = "', '"), "'"
      )
    }
    if (length(txt_gp$summary) != sum(is.summary)) {
      stop(
        "Your fpTxtGp$summary list has invalid number of rows",
        ", the should be equal the of the number rows that aren't summaries.",
        " you have '", length(txt_gp$summary), "' rows in the fpTxtGp$summary",
        ", while the labeltext argument has '", attr(labels, "no_rows"), "' rows",
        " where '", sum(is.summary), "' are not summaries."
      )
    }
  }

  fixed_labels <- vector("list", attr(labels, "no_cols"))
  max_height <- NULL
  max_width <- NULL
  # Walk through the labeltext
  # Creates a list matrix with
  # The column part
  for (j in 1:attr(labels, "no_cols")) {
    fixed_labels[[j]] <- vector("list", attr(labels, "no_rows"))

    # The row part
    for (i in 1:attr(labels, "no_rows")) {
      txt_out <- labels[i, j]

      # If it's a call created by bquote or similar it
      # needs evaluating
      if (is.call(txt_out)) {
        txt_out <- eval(txt_out)
      }

      if (is.expression(txt_out) || is.character(txt_out) || is.numeric(txt_out) || is.factor(txt_out)) {
        x <- switch(align[j],
                    l = 0,
                    r = 1,
                    c = 0.5
        )

        just <- switch(align[j],
                       l = "left",
                       r = "right",
                       c = "center"
        )

        # Bold the text if this is a summary
        if (is.summary[i]) {
          x <- switch(align[j],
                      l = 0,
                      r = 1,
                      c = 0.5
          )

          gp_list <- txt_gp$summary[[sum(is.summary[1:i])]][[j]]
          gp_list[["col"]] <- rep(col$text, length = attr(labels, "no_rows"))[i]

          # Create a textGrob for the summary
          # The row/column order is in this order
          # in order to make the following possible:
          # list(rownames(x), list(expression(1 >= a), "b", "c"))
          fixed_labels[[j]][[i]] <-
            textGrob(txt_out,
                     x = x,
                     just = just,
                     gp = do.call(gpar, gp_list)
            )
        } else {
          gp_list <- txt_gp$label[[sum(!is.summary[1:i])]][[j]]
          if (is.null(gp_list$col)) {
            gp_list[["col"]] <- rep(col$text, length = attr(labels, "no_rows"))[i]
          }

          # Create a textGrob with the current row-cell for the label
          fixed_labels[[j]][[i]] <-
            textGrob(txt_out,
                     x = x,
                     just = just,
                     gp = do.call(gpar, gp_list)
            )
        }

        attr(fixed_labels[[j]][[i]], "height") <- grobHeight(fixed_labels[[j]][[i]])
        attr(fixed_labels[[j]][[i]], "width") <- grobWidth(fixed_labels[[j]][[i]])
        if (is.null(max_height)) {
          max_height <- attr(fixed_labels[[j]][[i]], "height")
          max_width <- attr(fixed_labels[[j]][[i]], "width")
        } else {
          max_height <- max(max_height, attr(fixed_labels[[j]][[i]], "height"))
          max_width <- max(max_width, attr(fixed_labels[[j]][[i]], "width"))
        }
      }
    }
  }

  structure(fixed_labels,
            max_height = max_height,
            max_width = max_width,
            cex =  ifelse(any(is.summary),
                          txt_gp$summary[[1]][[1]]$cex,
                          txt_gp$label[[1]][[1]]$cex),
            no_cols = attr(labels, "no_cols"),
            no_rows = attr(labels, "no_rows"))
}
