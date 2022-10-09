#' Get a function list
#'
#' This function helps the \code{\link{forestplot}}
#' to deal with multiple drawing functions for the
#' confidence intervals.
#'
#' @param fn The function list/matrix. If a list it
#'  should be in the format [[row]][[col]], the function
#'  tries to handle this but in cases where the columns
#'  and rows are the same it will not know what is a column
#'  and what is a row.
#' @param no_rows Number of rows
#' @param no_depth Number of columns
#' @param missing_rows The rows that don't have a CI
#' @return \code{list} The function returns a list that has
#' the format [[row]][[col]] where each element contains the
#' function that you need to call using the \code{\link[base]{as.call}}
#' and \code{\link[base]{eval}} functions: \code{eval(as.call(list(fn[[row]][[col]], arg_1 = 1, arg_2 = 2)))}
#'
#' @inheritParams forestplot
#' @keywords internal
prFpGetConfintFnList <- function(fn, no_rows, no_depth, missing_rows, is.summary, summary) {
  ret <- prPopulateList(fn,
                        no_rows = no_rows,
                        no_depth = no_depth,
                        missing_rows = missing_rows,
                        is.summary = is.summary,
                        summary = summary)

  makeCalleable <- function(value) {
    if (is.function(value)) {
      return(value)
    }

    if (is.character(value)) {
      return(get(value))
    }

    if (!is.list(value)) {
      stop("Cannot handle non-list/character/function elements in this function")
    }

    for (i in 1:length(value)) {
      value[[i]] <- makeCalleable(value[[i]])
    }
    return(value)
  }
  ret <- makeCalleable(ret)

  return(ret)
}

#' Populate a list corresponding to matrix specs
#'
#' This function helps the \code{\link{forestplot}}
#' to deal with different arguments for the
#' confidence intervals.
#'
#' @param elmnt The element item/list/matrix. If a list it
#'  should be in the format [[row]][[col]], the function
#'  tries to handle this but in cases where the columns
#'  and rows are the same it will not know what is a column
#'  and what is a row.
#' @param no_rows Number of rows
#' @param no_depth Number of outcomes per row, i.e. depth
#' @param missing_rows The rows that don't have data
#' @return \code{list} The function returns a list that has
#'  the format [[row]][[col]] where each element contains the
#'  corresponding element
#'
#' @inheritParams forestplot
#' @keywords internal
prPopulateList <- function(elmnt, no_rows, no_depth, missing_rows, is.summary, summary) {
  # Return a list that has
  # a two dim structure of [[row]][[col]]
  # if you have a matrix provided but if you
  # have only a vector with only 1 column then you
  # get the [[row]] by default
  # If the fn is a character or a matrix then
  ret <- list()
  if (is.function(elmnt)) {
    if (no_depth == 1) {
      for (i in 1:no_rows) {
        ret[[i]] <- elmnt
      }
    } else {
      for (i in 1:no_rows) {
        ret[[i]] <- list()
        for (ii in 1:no_depth) {
          ret[[i]][[ii]] <- elmnt
        }
      }
    }
  } else if (is.character(elmnt) ||
    is.numeric(elmnt)) {
    if (is.matrix(elmnt)) {
      if (ncol(elmnt) != no_depth) {
        stop(
          "Your columns do not add upp for your",
          " confidence interval funcitons, ",
          ncol(elmnt), " != ", no_depth
        )
      }
      if (nrow(elmnt) != no_rows) {
        stop(
          "Your rows do not add upp for your",
          " confidence interval funcitons, ",
          nrow(elmnt), " != ", no_rows
        )
      }
    } else if (length(elmnt) == no_depth) {
      elmnt <- matrix(elmnt, nrow = no_rows, ncol = no_depth, byrow = TRUE)
    } else if (length(elmnt) %in% c(1, no_rows)) {
      elmnt <- matrix(elmnt, nrow = no_rows, ncol = no_depth)
    } else {
      stop(
        "You have not provided the expected",
        " number of elements: ",
        length(elmnt), " is not 1, ", no_depth, " (columns), or ", no_rows, " (rows)"
      )
    }

    # Convert into function format
    for (i in 1:no_rows) {
      if (no_depth == 1) {
        ret[[i]] <- elmnt[i, 1]
      } else {
        ret[[i]] <- list()
        for (ii in 1:no_depth) {
          ## Go by row for the elmnt
          ret[[i]][[ii]] <- elmnt[i, ii]
        }
      }
    }
  } else if (is.list(elmnt)) {
    if (no_depth == 1) {
      # Actually correct if the lengths add up
      if (length(elmnt) != no_rows) {
        if (length(elmnt) == sum(is.summary == summary)) {
          ret <- list()
          i <- 1
          for (is_row_summary in is.summary) {
            if (is_row_summary == summary) {
              ret <- append(ret, elmnt[[i]])
              i <- i + 1
            } else {
              # For simplicity all non-same elements have the last summary element
              # As this element isn't outputted it doesn't matter
              ret <- append(ret, elmnt[[i]])
            }
          }
        } else if (length(elmnt) == sum(is.summary == summary & !missing_rows)) {
          ret <- list()
          i <- 1
          for (row_no in 1:length(is.summary)) {
            if (is.summary[row_no] == summary & !missing_rows[row_no]) {
              ret <- append(ret, elmnt[[i]])
              i <- i + 1
            } else {
              # For simplicity all non-same elements have the last summary element
              # As this element isn't outputted it doesn't matter
              ret <- append(ret, elmnt[[i]])
            }
          }
        } else {
          stop(
            "You do not have the same number of ",
            "confidence interval functions as you have ",
            "number of rows: ", length(elmnt), "!= ", no_rows,
            " You should provide the same number."
          )
        }
      } else {
        ret <- elmnt
      }
    } else {
      # Populate a new elmnt list
      if (length(elmnt) == no_rows) {
        # One dim-list provided
        # now generate a two-dim list
        if (!is.list(elmnt[[1]])) {
          for (i in 1:no_rows) {
            ret[[i]] <- list()
            for (ii in 1:no_depth) {
              ## Go by row for the elmnt
              ret[[i]][[ii]] <- elmnt[[i]]
            }
          }
        } else {
          # Verify that the list structure
          # is provided as a valid matrix
          # with the correct size
          n <- sapply(elmnt, length)
          if (any(n != no_depth)) {
            stop(
              "You need to provide a 'square' list (of dim. n x m)",
              " of the same dimension as the number of lines",
              " in order for this function to work. Currently your",
              " confidence interval function has the format",
              " ", no_rows, " x ", paste(n, collapse = "/"),
              " where you want all of the second argument to be",
              " equal to ", no_depth
            )
          }

          ret <- elmnt
        }
      } else if (length(elmnt) == no_depth) {
        # One dim-list provided
        # now generate a two-dim list
        if (!is.list(elmnt[[1]])) {
          for (i in 1:no_rows) {
            ret[[i]] <- list()
            for (ii in 1:no_depth) {
              ## Go by row for the elmnt
              ret[[i]][[ii]] <- elmnt[[ii]]
            }
          }
        } else {
          # Verify that the list structure
          # is provided as a matrix
          n <- sapply(elmnt, length)
          if (any(n != no_rows)) {
            stop(
              "You need to provide a 'square' list (of dim. n x m)",
              " of the same dimension as the number of lines",
              " in order for this function to work. Currently your",
              " confidence interval function has the format",
              " ", no_rows, " x ", paste(n, collapse = "/"),
              " where you want all of the second argument to be",
              " equal to ", no_depth
            )
          }

          # Change to the [[row]][[col]] format
          for (i in 1:no_rows) {
            ret[[i]] <- list()
            for (ii in 1:no_depth) {
              ## Go by row for the elmnt
              ret[[i]][[ii]] <- elmnt[[ii]][[i]]
            }
          }
        }
      } else {
        stop(
          "The number of provided confidence intervals",
          " functions, ", length(elmnt), ", ",
          " does not seem to match up with either",
          " number of rows, ", no_rows,
          " or number of cols, ", no_depth
        )
      }
    }
  } else {
    stop(
      "You have provided something else than",
      " a function, list or function name: ",
      class(elmnt)
    )
  }

  return(ret)
}

#' Plots the labels
#'
#' This is a helper function to the \code{\link{forestplot}}
#' function.
#'
#' @param labels A list to the labels
#' @param nc Number of columns
#' @param nr Number of rows
#' @inheritParams forestplot
#' @return \code{void}
#'
#' @keywords internal
prFpPrintLabels <- function(labels, nc, nr, graph.pos) {
  # Output the labels
  # The column
  cols <- 1:(nc + 1)
  cols <- cols[cols != graph.pos]
  cols <- cols * 2 - 1

  for (label_col in 1:nc) {
    j <- cols[label_col]
    # The row
    for (i in 1:nr) {
      if (!is.null(labels[[label_col]][[i]])) {
        # The column position is 2 * j - 1 due to the column gap
        vp <- viewport(
          layout.pos.row = i,
          layout.pos.col = j,
          name = sprintf("Label_vp_%d_%d", i, j)
        )
        pushViewport(vp)
        grid.draw(labels[[label_col]][[i]])
        upViewport()
      }
    }
  }
}

#' An alternative to rep()
#'
#' The rep() doesn't work with length.out
#' when lists are supposed to be their own
#' elements
#'
#' @param x The list to be repeated
#' @param length.out The length of the resulting list
#' @return \code{list}
#' @keywords internal
prListRep <- function(x, length.out) {
  lapply(0:(length.out - 1),
    function(x, g) {
      if (!is.list(g) ||
        !is.list(g[[1]])) {
        return(g)
      }

      return(g[[(x %% length(g)) + 1]])
    },
    g = x
  )
}

#' Gets the x-axis range
#'
#' If the borders are smaller than the upper/lower limits
#' then clip the graph. The line will have arrows indicating
#' that it continues beyond the graph The zero bar has to
#' be on the chart though!
#'
#' @return \code{vector} Contains a min and max value
#' @inheritParams forestplot
#'
#' @keywords internal
prFpXrange <- function(upper, lower, clip, zero, xticks, xlog) {
  top <- min(max(upper, na.rm = TRUE), clip[2])
  bottom <- max(min(lower, na.rm = TRUE), clip[1])

  # Although perhaps not entirely intuitive
  # I've decided that the function should
  # extend the range to include the clip
  # endpoints unless there are pre-specified
  # ticks indicating that the end-points aren't
  # included in the x-axis
  if (is.null(xticks)) {
    ret <- c(
      min(
        zero,
        bottom,
        na.rm = TRUE
      ),
      max(
        zero,
        top,
        na.rm = TRUE
      )
    )
  } else {
    ret <- c(
      min(
        c(zero, bottom, xticks),
        na.rm = TRUE
      ),
      max(
        c(zero, top, xticks),
        na.rm = TRUE
      )
    )
  }

  return(ret)
}

#' Get the label
#'
#' A function used for fetching the text or
#' expression from the supplied labeltext.
#'
#' @param label_type The type of label
#' @param i The row
#' @param j The column
#' @return An expression or a text
#'
#' @inheritParams forestplot
#' @keywords internal
prFpFetchRowLabel <- function(label_type, labeltext, i, j) {
  if (label_type == "expression") {
    # Haven't figured out it this is possible with
    # a multilevel expression
    row_column_text <- labeltext[[i]]
  } else if (label_type == "list") {
    # I get annoying warnings with this
    # if (!is.expression(labeltext[[j]][[i]]) && is.na(labeltext[[j]][[i]]))
    #    return(FALSE)
    row_column_text <- labeltext[[j]][[i]]
  } else {
    if (is.na(labeltext[i, j])) {
      return(FALSE)
    }
    row_column_text <- labeltext[i, j]
  }

  if (!is.expression(row_column_text) &&
      !is.call(row_column_text) &&
      is.na(row_column_text)) {
    return("")
  }

  return(row_column_text)
}

#' Get the main `forestplot`
#'
#' The layout makes space for a legend if needed
#'
#' @param labels The labels
#' @param legend_layout A legend layout object if applicable
#' @return \code{viewport} Returns the `viewport` needed
#'
#' @inheritParams forestplot
#' @keywords internal
prFpGetLayoutVP <- function(lineheight, labels, legend_layout = NULL) {
  if (!is.unit(lineheight)) {
    if (lineheight == "auto") {
      lvp_height <- unit(1, "npc")
    } else if (lineheight == "lines") {
      lvp_height <- unit(attr(labels, "no_rows") * attr(labels, "cex") * 1.5, "lines")
    } else {
      stop("The lineheight option '", lineheight, "'is yet not implemented")
    }
  } else {
    lvp_height <- (convertY(lineheight,
                            unitTo = "lines",
                            valueOnly = TRUE) * attr(labels, "no_rows")) |>
      unit("lines")
  }

  # If there is a legend on top then the size should be adjusted
  if (!is.null(legend_layout) &&
    legend_layout$nrow == 3 &&
    convertY(lvp_height, "npc", valueOnly = TRUE) < 1) {
    lvp_height <- sum(lvp_height, legend_layout$heights[1:2])
  }

  lvp <- viewport(
    height = lvp_height,
    layout = legend_layout,
    name = ifelse(is.null(legend_layout), "main", "main_and_legend")
  )
  return(lvp)
}

#' Validate the forestplot label list
#'
#' Checks that all list elements have equal
#' length, i.e. there is a m x n relation
#'
#' @param labelList The list of labels
#' @return \code{boolean} TRUE or FALSE
#'
#'
#' @keywords internal
prFpValidateLabelList <- function(labelList) {
  l <- length(labelList[[1]])
  if (length(labelList) == 1) {
    return(TRUE)
  }

  for (i in 2:length(labelList)) {
    # All elements should have the same length
    if (l != length(labelList[[i]])) {
      return(FALSE)
    }
  }

  return(TRUE)
}

#' Finds the widest grob in the current list of grobs
#'
#' @param grob.list A list of grobs
#' @param return_unit A valid \code{\link[grid]{unit}} specifier
#' @return \code{grid::unit} Returns the width \code{\link[grid]{unit}}
#'  for the widest grob
#' @keywords internal
prFpFindWidestGrob <- function(grob.list, return_unit = "mm") {
  len <- c()
  for (i in seq(along.with = grob.list)) {
    if (is.object(grob.list[[i]])) {
      # There is a tendency of underestemating grob size
      # when there are expressions
      grob_width <- convertWidth(grobWidth(grob.list[[i]]), return_unit, valueOnly = TRUE)
      len <- append(len, grob_width)
    } else {
      len <- append(len, 0)
    }
  }

  return(unit(max(len), return_unit))
}

#' Converts legend position to a standard position
#'
#' Used for the forestplot legend box.
#'
#' @return \code{list} Returns the \code{pos} list with
#'  the correct x/y/adjust values
#'
#' @inheritParams fpLegend
#' @keywords internal
prFpGetLegendBoxPosition <- function(pos) {
  valid_txt_pos <- c("bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", "right", "center")
  if (!all(c("x", "y") %in% names(pos)) &&
    !(("x" %in% pos &&
      any(pos[["x"]] == valid_txt_pos)) ||
      any(pos[[1]] == valid_txt_pos))) {
    stop(
      "If you want to specify the legend position in a certain corner",
      " within the main plot then you need to have list names x and y specified,",
      " or you should have the first list element to be '", paste(valid_txt_pos, collapse = "'/'"), "',",
      " if you don't specify the first element then it can be the 'x' element"
    )
  }

  # Convert to the x & y format to make things easier
  if (!all(c("x", "y") %in% names(pos))) {
    if ("x" %in% names(pos)) {
      txt_pos <- pos[["x"]]
    } else {
      txt_pos <- pos[[1]]
    }

    # The inset offsets the position
    if (!"inset" %in% names(pos)) {
      pos[["inset"]] <- unit(0, "npc")
    } else if (!is.unit(pos[["inset"]])) {
      if (pos[["inset"]] > 1 || pos[["inset"]] < 0) {
        stop("If you have not specified the unit of the pos inset then it should be between 0 and 1")
      }
      pos[["inset"]] <- unit(pos[["inset"]], "npc")
    } else {
      if (convertUnit(pos[["inset"]], unitTo = "npc", valueOnly = TRUE) > 1) {
        stop("You have provided a value outside the possible range ('npc' bigger than 1)")
      }
    }

    if (txt_pos == "bottomright") {
      pos[["x"]] <- unit(1, "npc") - pos[["inset"]]
      pos[["y"]] <- unit(0, "npc") + pos[["inset"]]
      pos[["just"]] <- c("right", "bottom")
    } else if (txt_pos == "bottom") {
      pos[["x"]] <- unit(0.5, "npc")
      pos[["y"]] <- unit(0, "npc") + pos[["inset"]]
      pos[["just"]] <- c("center", "bottom")
    } else if (txt_pos == "bottomleft") {
      pos[["x"]] <- unit(0, "npc") + pos[["inset"]]
      pos[["y"]] <- unit(0, "npc") + pos[["inset"]]
      pos[["just"]] <- c("left", "bottom")
    } else if (txt_pos == "left") {
      pos[["x"]] <- unit(0, "npc") + pos[["inset"]]
      pos[["y"]] <- unit(.5, "npc")
      pos[["just"]] <- c("left", "center")
    } else if (txt_pos == "topleft") {
      pos[["x"]] <- unit(0, "npc") + pos[["inset"]]
      pos[["y"]] <- unit(1, "npc") - pos[["inset"]]
      pos[["just"]] <- c("left", "top")
    } else if (txt_pos == "top") {
      pos[["x"]] <- unit(0.5, "npc")
      pos[["y"]] <- unit(1, "npc") - pos[["inset"]]
      pos[["just"]] <- c("center", "top")
    } else if (txt_pos == "topright") {
      pos[["x"]] <- unit(1, "npc") - pos[["inset"]]
      pos[["y"]] <- unit(1, "npc") - pos[["inset"]]
      pos[["just"]] <- c("right", "top")
    } else if (txt_pos == "right") {
      pos[["x"]] <- unit(1, "npc") - pos[["inset"]]
      pos[["y"]] <- unit(.5, "npc")
      pos[["just"]] <- c("right", "center")
    } else if (txt_pos == "center" || txt_pos == "centre") {
      pos[["x"]] <- unit(.5, "npc")
      pos[["y"]] <- unit(.5, "npc")
      pos[["just"]] <- c("center", "center")
    } else {
      stop("Position '", pos[["x"]], "'not yet implemented")
    }
  } else if (!"just" %in% names(pos)) {
    pos[["just"]] <- c("center", "center")
  }
  return(pos)
}

#' Prepares the legend marker function
#'
#' @param fn.legend The unknown parameter
#' @param col_no The number of columns
#' @param row_no The number of rows
#' @param fn.ci_norm The original fn.ci_norm input
#' @return \code{list}
#'
#' @keywords internal
prFpPrepareLegendMarker <- function(fn.legend, col_no, row_no, fn.ci_norm) {
  if (!is.null(fn.legend)) {
    if (is.function(fn.legend)) {
      return(lapply(1:col_no, function(x) fn.legend))
    }
    if (is.character(fn.legend)) {
      if (length(fn.legend) == 1) {
        fn.legend <- rep(fn.legend, times = col_no)
      } else if (length(fn.legend) != col_no) {
        stop(
          "The number of legend markers, ", length(fn.legend),
          ", should be the same as the number of columns for the mean, ", col_no
        )
      }

      tmp <- list()
      for (i in 1:length(fn.legend)) {
        tmp[[i]] <- get(fn.legend[i])
      }

      return(tmp)
    }

    if (is.list(fn.legend)) {
      if (length(fn.legend) != col_no) {
        stop(
          "The number of legend markers, ", length(fn.legend), ",",
          " should be the same as the number of columns for the mean, ", col_no
        )
      } else if (!all(sapply(fn.legend, function(x) is.function(x)))) {
        stop("If you provide a list for fn.legend then each element should be a function")
      }

      return(fn.legend)
    }

    stop(
      "The legend marked function designated by the fn.legend",
      " is neither a character or a function"
    )
  }

  if (length(fn.ci_norm) == col_no) {
    return(prFpGetConfintFnList(fn = fn.ci_norm,
                                no_rows = row_no,
                                no_depth = col_no)[[1]])
  }

  # Not sure what to do if the number don't match the number of legends
  # and it ain't 1 and it therefore defaults to the normal confidence
  # interval marker
  if (length(fn.ci_norm) != 1) {
    fn.ci_norm <- fpDrawNormalCI
  }

  return(lapply(1:col_no, function(x) fn.ci_norm))
}

#' Converts a 2D or 3D array to mean, lower, upper
#'
#' @param x The array to convert
#' @return \code{list(mean = mean, lower = lower, upper = upper)}
#' @importFrom stats na.omit
#' @keywords internal
prFpConvertMultidimArray <- function(x) {
  cleanX <- na.omit(x)
  switch(as.character(length(dim(cleanX))),
    "2" = {
      # Loop through the different rows as a row with only a label may have NA in it
      lower_cnr <- NULL
      upper_cnr <- NULL
      for (d1 in dim(cleanX)[1]) {
        if (length(unique(cleanX[d1, ])) < 3) {
          next
        }

        lower_cnr <- which.min(cleanX[d1, ])
        upper_cnr <- which.max(cleanX[d1, ])
        if (length(lower_cnr) == 1 &&
          length(upper_cnr) == 1) {
          break
        }
      }
      if (length(lower_cnr) != 1 ||
        length(upper_cnr) != 1) {
        stop(
          "Sorry did not manage to automatically identify",
          " the upper/lower boundaries."
        )
      }

      lower <- x[, lower_cnr, drop = TRUE]
      upper <- x[, upper_cnr, drop = TRUE]
      mean <- x[, -c(upper_cnr, lower_cnr), drop = TRUE]
    },
    "3" = {
      # Loop through the different rows as a row with only a label may have NA in it
      # this is a little complicated as we're doing a 3D loop and exiting
      # as soon as the vars have been identified
      lower_cnr <- NULL
      upper_cnr <- NULL
      for (d3 in 1:dim(cleanX)[3]) {
        for (d1 in 1:dim(cleanX)[1]) {
          if (length(unique(cleanX[d1, , d3])) < 3) {
            next
          }

          lower_cnr <- which.min(cleanX[d1, , d3])
          upper_cnr <- which.max(cleanX[d1, , d3])

          if (length(lower_cnr) == 1 &&
            length(upper_cnr) == 1) {
            break
          }
        }
        if (length(lower_cnr) == 1 &&
          length(upper_cnr) == 1) {
          break
        }
      }
      if (length(lower_cnr) != 1 ||
        length(upper_cnr) != 1) {
        stop(
          "Sorry did not manage to automatically identify",
          " the upper/lower boundaries."
        )
      }
      lower <- x[, lower_cnr, , drop = TRUE]
      upper <- x[, upper_cnr, , drop = TRUE]
      mean <- x[, -c(upper_cnr, lower_cnr), , drop = TRUE]
    },
    {
      stop(
        "Invalid number of dimensions of the mean argument,",
        " should be either 2 or 3 - you have '", length(dim(mean)), "'"
      )
    }
  )

  if (!all(lower <= upper, na.rm = TRUE) ||
    !all(lower <= mean, na.rm = TRUE) ||
    !all(mean <= upper, na.rm = TRUE)) {
    stop(
      "Sorry did not manage to correctly identify",
      " the upper/lower boundaries from the input matrix."
    )
  }

  return(list(mean = mean, lower = lower, upper = upper))
}

#' Pushes viewport with margins
#'
#' A \code{\link[grid]{grid.layout}} object is used to
#' generate the margins. A second viewport selecting the
#' mid-row/col is used to create the effect of margins
#'
#' @param bottom The margin object, either in npc or a \code{\link[grid]{unit}} object
#' @param left The margin object, either in npc or a \code{\link[grid]{unit}} object
#' @param top The margin object, either in npc or a \code{\link[grid]{unit}} object
#' @param right The margin object, either in npc or a \code{\link[grid]{unit}} object
#' @param name The name of the last viewport
#' @return \code{void}
#'
#' @keywords internal
prPushMarginViewport <- function(bottom, left, top, right, name = NULL) {
  if (!is.unit(bottom)) {
    bottom <- unit(bottom, "npc")
  }

  if (!is.unit(top)) {
    top <- unit(top, "npc")
  }

  if (!is.unit(left)) {
    left <- unit(left, "npc")
  }

  if (!is.unit(right)) {
    right <- unit(right, "npc")
  }

  layout_name <- NULL
  if (!is.character(name)) {
    layout_name <- sprintf("margin_grid_%s", name)
  }

  gl <- grid.layout(
    nrow = 3, ncol = 3,
    heights = unit.c(top, unit(1, "npc") - top - bottom, bottom),
    widths = unit.c(left, unit(1, "npc") - left - right, right)
  )

  pushViewport(viewport(layout = gl, name = layout_name))
  pushViewport(viewport(layout.pos.row = 2, layout.pos.col = 2, name = name))
}

#' Adds a title to the plot
#'
#' Adds the title and generates a new
#' main viewport below the title
#'
#' @param title The title as accepted by \code{\link[grid]{textGrob}}
#' @param space_below The space below, defaults to 1/5 of the title height
#' @return \code{NULL} The function does not return a value
#'
#' @inheritParams forestplot
#' @keywords internal
prGridPlotTitle <- function(title,
                            gp,
                            space_below) {
  tg_list <- list(
    label = title,
    just = "center"
  )
  if (!is.null(gp$just)) {
    tg_list$just <- gp$just
    gp$just <- NULL
  }
  tg_list$gp <- do.call(gpar, gp)

  titleGrob <- do.call(
    textGrob,
    tg_list
  )

  # The y/g/j letters are not included in the height
  gh <- unit(convertUnit(grobHeight(titleGrob), "mm", valueOnly = TRUE) * 1.5, "mm")
  if (missing(space_below)) {
    space_below <- unit(convertUnit(gh, "mm", valueOnly = TRUE) / 2, "mm")
  } else if (!is.unit(space_below)) {
    space_below <- unit(space_below, "npc")
  }

  gl <- grid.layout(
    nrow = 3, ncol = 1,
    heights = unit.c(gh, space_below, unit(1, "npc") - space_below - gh)
  )

  pushViewport(viewport(layout = gl, name = "title_layout"))
  pushViewport(viewport(layout.pos.row = 1, name = "title"))
  grid.draw(titleGrob)
  upViewport()

  pushViewport(viewport(layout.pos.row = 3, name = "main"))
}

#' Just a simple access to the gp$cex parameter
#'
#' @param x The text-grob of interest
#' @return \code{numeric} The cex value, 1 if no cex was present
#' @keywords internal
prGetTextGrobCex <- function(x) {
  cex <- 1
  if (!is.null(x$gp$cex)) {
    cex <- x$gp$cex
  }

  return(cex)
}

#' Merges two \code{\link[grid]{gpar}} elements
#'
#' The second elements overrides any conflicting elements within the first
#'
#' @param l1 A \code{\link[grid]{gpar}} element
#' @param l2 A \code{\link[grid]{gpar}} element
#' @return Returns a \code{\link[grid]{gpar}} element
#' @keywords internal
prGparMerge <- function(l1, l2) {
  cleanFont4Fontface <- function(element) {
    if (is.null(element$font)) {
      return(element)
    }

    if (is.null(element$fontface)) {
      element$fontface <- names(element$font)
    }
    #  Delete font in favor of fontface
    element$font <- NULL

    return(element)
  }
  l1 <- cleanFont4Fontface(l1)
  l2 <- cleanFont4Fontface(l2)
  out <- c(l1, l2)
  if (!any(duplicated(names(out)))) {
    return(out)
  }

  dups <- unique(names(out)[duplicated(names(out))])
  for (n in dups) {
    wd <- which(names(out) == n)
    out <- out[-wd[1:(length(wd) - 1)]]
  }
  class(out) <- unique(c(class(out), class(l1)))
  return(out)
}
