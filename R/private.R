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
#' @param no_cols Number of columns
#' @return \code{list} The function returns a list that has
#' the format [[row]][[col]] where each element contains the
#' function that you need to call using the \code{\link[base]{as.call}}
#' and \code{\link[base]{eval}} functions: \code{eval(as.call(list(fn[[row]][[col]], arg_1=1, arg_2=2)))}
#'
#'
#' @keywords internal
prFpGetConfintFnList <- function(fn, no_rows, no_cols){
  # Return a list that has
  # a two dim structure of [[row]][[col]]
  # if you have a matrix provided but if you
  # have only a vector with only 1 column then you
  # get the [[row]] by default
  # If the fn is a character or a matrix then
  ret <- list()
  if (is.function(fn)){
    if (no_cols == 1){
      for (i in 1:no_rows){
        ret[[i]] <- fn
      }
    }else{
      for (i in 1:no_rows){
        ret[[i]] <- list()
        for (ii in 1:no_cols){
          ret[[i]][[ii]] <- fn
        }
      }
    }
  }else if (typeof(fn) == "character"){
    if (is.matrix(fn)){
      if (ncol(fn) != no_cols)
        stop("Your columns do not add upp for your",
             " confidence interval funcitons, ",
             ncol(fn), " != ", no_cols)
      if (nrow(fn) != no_rows)
        stop("Your rows do not add upp for your",
             " confidence interval funcitons, ",
             nrow(fn), " != ", no_rows)

    }else if (length(fn) %in% c(1, no_rows)){
      fn <- matrix(fn, nrow=no_rows, ncol=no_cols)
    }else if (length(fn) == no_cols){
      fn <- matrix(fn, nrow=no_rows, ncol=no_cols, byrow=TRUE)
    }else{
      stop("You have not provided the expected",
           " number of funciton names: ",
           length(fn), " is not 1, ", no_cols, ", or ", no_rows)

    }

    # Convert into function format
    for (i in 1:no_rows){
      if (no_cols == 1){
        ret[[i]] <- get(fn[i, 1])
      }else{
        ret[[i]] <- list()
        for (ii in 1:no_cols){
          ## Go by row for the fn
          ret[[i]][[ii]] <- get(fn[i, ii])
        }
      }
    }

  }else if (is.list(fn)){
    if (no_cols == 1){
      # Actually correct if the lengths add up
      if (length(fn) != no_rows)
        stop("You do not have the same number of ",
             "confidence interval functions as you have ",
             "number of rows: ", length(fn), "!=", no_rows,
             " You should provide the same number.")
      ret <- fn
    }else{
      # Populate a new fn list
      if (length(fn) == no_rows){
        # One dim-list provided
        # now generate a two-dim list
        if (!is.list(fn[[1]])){
          for (i in 1:no_rows){
            ret[[i]] <- list()
            for (ii in 1:no_cols){
              ## Go by row for the fn
              ret[[i]][[ii]] <- fn[[i]]
            }
          }
        }else{
          # Verify that the list structure
          # is provided as a valid matrix
          # with the correct size
          n <- sapply(fn, length)
          if (any(n != no_cols)){
            stop("You need to provide a 'square' list (of dim. n x m)",
                 " of the same dimension as the number of lines",
                 " in order for this function to work. Currently your",
                 " confidence interval function has the format",
                 " ", no_rows , " x ", paste(n, collapse="/"),
                 " where you want all of the second argument to be",
                 " equal to ", no_cols)
          }

          ret <- fn
        }
      }else if (length(fn) == no_cols){
        # One dim-list provided
        # now generate a two-dim list
        if (!is.list(fn[[1]])){
          for (i in 1:no_rows){
            ret[[i]] <- list()
            for (ii in 1:no_cols){
              ## Go by row for the fn
              ret[[i]][[ii]] <- fn[[ii]]
            }
          }
        }else{
          # Verify that the list structure
          # is provided as a matrix
          n <- sapply(fn, length)
          if (any(n != no_rows)){
            stop("You need to provide a 'square' list (of dim. n x m)",
                 " of the same dimension as the number of lines",
                 " in order for this function to work. Currently your",
                 " confidence interval function has the format",
                 " ", no_rows , " x ", paste(n, collapse="/"),
                 " where you want all of the second argument to be",
                 " equal to ", no_cols)
          }

          # Change to the [[row]][[col]] format
          for (i in 1:no_rows){
            ret[[i]] <- list()
            for (ii in 1:no_cols){
              ## Go by row for the fn
              ret[[i]][[ii]] <- fn[[ii]][[i]]
            }
          }
        }
      }else{
        stop("The number of provided confidence intervals",
             " functions, ", length(fn), ", ",
             " does not seem to match up with either",
             " number of rows, ", no_rows,
             " or number of cols, ", no_cols)
      }
    }
  }else{
    stop("You have provided something else than",
         " a function, list or function name: ",
         class(fn))
  }

  return(ret)
}

#' A helper function to forestplot
#'
#' Gets the x-label and zero-bar details
#'
#' @param x_range The range that the values from the different confidence
#'  interval span
#' @param mean The original means, either matrix or vector
#' @return \code{list} Returns a list with axis_vp, axisGrob, labGrob, zero and clip
#'
#'
#' @inheritParams forestplot
#' @keywords internal
prFpGetGraphTicksAndClips <- function(xticks,
                                      xticks.digits,
                                      xlog,
                                      xlab,
                                      lwd.xaxis,
                                      col,
                                      txt_gp,
                                      clip,
                                      zero,
                                      x_range,
                                      mean,
                                      graph.pos){

  # Active rows are all excluding the top ones with NA in the mean value
  if (is.matrix(mean)){
    for (from in 1:nrow(mean))
      if (!all(is.na(mean[from, ])))
        break;
    to <- nrow(mean)
  }else{
    for (from in 1:length(mean))
      if (!is.na(mean[from]))
        break;
    to <- length(mean)
  }

  if (xlog) {
    clip[clip < 0] <- 0
    clip <- log(clip)
    zero <- log(zero)

    if (missing(xticks)) {
      ticks <- getTicks(exp(x_range),
                        clip=clip,
                        exp=xlog,
                        digits=xticks.digits)

      # Add the endpoint ticks to the tick list if
      # it's not already there
      if (is.infinite(clip[1]) == FALSE &&
            min(ticks, na.rm = TRUE) < clip[1])
        ticks <- unique(c(exp(clip[1]), ticks))

      if (is.infinite(clip[2]) == FALSE &&
            max(ticks, na.rm = TRUE) > clip[2])
        ticks <- unique(c(ticks, exp(clip[2])))

      # Update the range so that it includes the ticks
      if (min(x_range) > log(min(ticks)))
        x_range[which.min(x_range)] <- log(min(ticks))
      if (max(x_range) < max(ticks))
        x_range[which.max(x_range)] <- log(max(ticks))

    } else {
      ticks <- xticks
    }

    axis_vp <- viewport(layout.pos.col = graph.pos * 2 - 1,
                        layout.pos.row = from:to,
                        xscale         = x_range,
                        name           = "axis")



    # Draw the x-axis if there are any ticks
    if (length(ticks)) {

      # Decide on the number of digits, if below zero then there should
      # be by default one more digit
      ticklabels <- ifelse(ticks < 1 | abs(floor(ticks*10)-ticks*10) > 0,
                           format(ticks, digits = 2, nsmall = 2),
                           format(ticks, digits = 1, nsmall = 1))
      ticks <- log(ticks)
    }else{
      ticks <- NULL
      ticklabels <- FALSE
    }


  } else {
    if (missing(xticks)){
      ticks <- getTicks(x_range,
                        clip=clip,
                        exp=xlog,
                        digits=xticks.digits)

      # Add the endpoint ticks to the tick list if
      # it's not already there
      if (is.infinite(clip[1]) == FALSE &&
            min(ticks, na.rm = TRUE) < clip[1])
        ticks <- unique(c(clip[1], ticks))

      if (is.infinite(clip[2]) == FALSE &&
            max(ticks, na.rm = TRUE) > clip[2])
        ticks <- unique(c(ticks, clip[2]))

      ticklabels <- TRUE

      # Update the range so that it includes the ticks
      if (min(x_range) > min(ticks))
        x_range[which.min(x_range)] <- min(ticks)
      if (max(x_range) < max(ticks))
        x_range[which.max(x_range)] <- max(ticks)

    } else{
      ticks <- xticks
      ticklabels <- TRUE
    }

    axis_vp <- viewport(layout.pos.col = 2 * graph.pos - 1,
                        layout.pos.row = from:to,
                        xscale         = x_range,
                        name           = "axis")

  }

  # Clean
  if (any(ticks < .Machine$double.eps &
            ticks > -.Machine$double.eps))
    ticks[ticks < .Machine$double.eps &
            ticks > -.Machine$double.eps] <- 0

  if (length(ticks) != 1 || ticks != 0){
    gp_list <- txt_gp$ticks
    gp_list$col <- col$axes

    if(!missing(lwd.xaxis))
      gp_list$lwd <- lwd.xaxis

    dg <- xaxisGrob(at    = ticks,
                    label = ticklabels,
                    gp    = do.call(gpar, gp_list))
  }else{
    dg <- FALSE
  }

  if (length(xlab) == 1 && nchar(xlab) > 0){
    gp_list <- txt_gp$xlab
    gp_list$col <- col$axes
    # Write the label for the x-axis
    labGrob <- textGrob(xlab,
                        gp = do.call(gpar, gp_list))

  }else{
    labGrob <- FALSE
  }


  return(list(axis_vp = axis_vp,
              axisGrob = dg,
              labGrob = labGrob,
              zero = zero,
              clip = clip,
              x_range = x_range))
}

#' Plots the x-axis for forestplot
#'
#' A helper function to the \code{\link{forestplot}}
#' function.
#'
#' @param axisList The list from \code{\link{prFpGetGraphTicksAndClips}}
#' @return void
#'
#' @inheritParams forestplot
#' @keywords internal
prFpPrintXaxis <- function(axisList,
                           col,
                           lwd.zero){
  # Now plot the axis inkluding the horizontal bar
  pushViewport(axisList$axis_vp)

  # Plot the vertical "zero" axis
  gp_list <- list(col = col$zero)
  if (!missing(lwd.zero))
    gp_list$lwd <- lwd.zero

  if (length(axisList$zero) == 1){
    grid.lines(x  = unit(axisList$zero, "native"),
               y  = 0:1,
               gp = do.call(gpar, gp_list))
  }else if (length(axisList$zero) == 2){
    gp_list$fill <- gp_list$col
    grid.polygon(x  = unit(c(axisList$zero,
                             rev(axisList$zero)),
                           "native"),
                 y  = c(0, 0, 1, 1),
                 gp = do.call(gpar, gp_list))
  }

  lab_y <- unit(0, "mm")
  lab_grob_height <- unit(-2, "mm")
  # Omit the axis if specified as 0
  if (is.grob(axisList$axisGrob)){
    # Plot the actual x-axis
    grid.draw(axisList$axisGrob)
    lab_grob_height <- grobHeight(axisList$axisGrob)
    lab_y <- lab_y - lab_grob_height
  }

  if (is.grob(axisList$labGrob)){
    # Add some padding between text and ticks proportional to the ticks height
    padding <-
      unit(convertY(lab_grob_height, "lines", valueOnly=TRUE)*0.1,
           "lines")

    # The text is strangely messy
    # and needs its own viewport
    pushViewport(viewport(height=grobHeight(axisList$labGrob),
                          y=lab_y - padding, just="top"))
    grid.draw(axisList$labGrob)
    upViewport()
  }
  upViewport()
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
prFpPrintLabels <- function(labels, nc, nr, graph.pos){
  # Output the labels
  # The column
  cols <- 1:(nc + 1)
  cols <- cols[cols !=  graph.pos]
  cols <- cols*2-1
  for (label_col in 1:nc) {
    j <- cols[label_col]
    # The row
    for (i in 1:nr) {
      if (!is.null(labels[[label_col]][[i]])) {
        # The column position is 2 * j - 1 due to the column gap
        vp <- viewport(layout.pos.row = i,
                       layout.pos.col = j,
                       name           = sprintf("Label_vp_%d_%d", i, j))
        pushViewport(vp)
        grid.draw(labels[[label_col]][[i]])
        upViewport()
      }
    }
  }
}

#' An alternativ to rep()
#'
#' The rep() doesn't work with length.out
#' when lists are supposed to be their own
#' elements
#'
#' @param x The list to be repeated
#' @param length.out The length of the resulting list
#' @return \code{list}
#' @keywords internal
prListRep <- function(x, length.out){
  lapply(0:(length.out-1),
         function(x, g){
           if (!is.list(g) ||
                 !is.list(g[[1]]))
             return(g)

           return(g[[(x %% length(g)) + 1]])
         }, g = x)
}

#' Gets the forestplot legend grobs
#'
#' @return \code{list} A "Legend" class that derives from a
#'  list with all the different legends. The list also contains
#'  attributes such as height, width, max_height,
#'  max_width, line_height_and_spacing. The title of the
#'  legend is saved inside \code{attr("title")}
#'
#' @inheritParams forestplot
#' @inheritParams fpLegend
#' @keywords internal
prFpGetLegendGrobs <- function(legend,
                               txt_gp,
                               title){
  lGrobs <- list()
  max_width <- 0
  max_height <- 0
  gp <- prListRep(txt_gp$legend, length.out = length(legend))
  for (n in 1:length(legend)){
    lGrobs[[n]] <- textGrob(legend[n], x=0, just="left",
                            gp=do.call(gpar, gp[[n]]))

    gw <- convertUnit(grobWidth(lGrobs[[n]]), "mm", valueOnly=TRUE)
    gh <- convertUnit(grobHeight(lGrobs[[n]]), "mm", valueOnly=TRUE)
    if (gw > max_width)
      max_width <- gw
    if (gh > max_height)
      max_height <- gh

    attr(lGrobs[[n]], "width") <- unit(gw, "mm")
    attr(lGrobs[[n]], "height") <- unit(gh, "mm")
  }
  attr(lGrobs, "max_height") <- unit(max_height, "mm")
  attr(lGrobs, "max_width") <- unit(max_width, "mm")
  attr(lGrobs, "line_height_and_spacing") <- unit.c(attr(lGrobs, "max_height"),
                                                    unit(.5, "lines"))

  # Do title stuff if present
  if (is.character(title)){
    title <- textGrob(title, x=0, just="left",
                      gp=do.call(gpar, txt_gp$legend.title))
    attr(lGrobs, "title") <- title

    attr(lGrobs, "titleHeight") <- grobHeight(title)
    attr(lGrobs, "titleWidth") <- grobHeight(title)
    if (convertUnit(attr(lGrobs, "titleWidth"), unitTo="npc", valueOnly=TRUE) >
          convertUnit(attr(lGrobs, "max_width"), unitTo="npc", valueOnly=TRUE))
      attr(lGrobs, "max_width") <- attr(lGrobs, "titleWidth")
  }
  class(lGrobs) <- c("Legend", class(lGrobs))
  return(lGrobs)
}

#' Draw the forestplot legend
#'
#' Takes the grobs and outputs the legend
#' inside the current viewport.
#'
#' @param lGrobs A list with all the grobs, see \code{\link{prFpGetLegendGrobs}}
#' @param col The colors of the legends.
#' @param colgap The gap between the box and the text
#' @param fn.legend The function for drawing the marker
#' @param ... Passed to the legend \code{fn.legend}
#' @return \code{void}
#'
#' @inheritParams forestplot
#' @inheritParams fpLegend
#'
#' @keywords internal
prFpDrawLegend <- function (lGrobs,
                            col,
                            colgap,
                            pos,
                            gp,
                            r,
                            padding,
                            fn.legend,
                            ...) {
  if (!inherits(lGrobs, "Legend"))
    stop("The lGrobs object should be created by the internal Gmisc:::prFpGetLegendGrobs and be of class 'Legend'.")

  # Draw the rounded rectangle at first
  # if there is a gpar specified.
  if (length(gp) > 0){
    grid.roundrect(gp = gp, r=r)
    inner_vp <- viewport(width=unit(1, "npc") - padding - padding,
                         height=unit(1, "npc") - padding - padding)
    pushViewport(inner_vp)
  }
  legend_width <- 0
  legend_height <- 0
  if (!is.list(pos) && pos == "top" ||
        is.list(pos) && "align" %in% names(pos) && pos[["align"]] == "horizontal"){
    orientation <- "horizontal"
  }else{
    orientation <- "vertical"
  }

  boxSize <- attr(lGrobs, "max_height")

  drawBox <- function(vp, i, col, lGrobs){
    pushViewport(vp)

    call_list <-
      list(fn.legend[[i]],
           lower_limit=0,
           estimate=.5,
           upper_limit=1,
           size=attr(lGrobs, "max_height"),
           y.offset = .5,
           clr.marker = col$box[i],
           clr.line = col$lines[i],
           lwd=1,
           ... = ...)

    # Do the actual drawing of the object
    eval(as.call(call_list))

    upViewport()
  }

  if (orientation == "horizontal"){
    # Output the horizontal boxes and texts
    widths <- NULL
    for (n in 1:length(lGrobs)){
      if (length(widths) == 0)
        widths <- unit.c(boxSize, colgap, attr(lGrobs[[n]], "width"))
      else
        widths <- unit.c(widths, colgap, boxSize, colgap, attr(lGrobs[[n]], "width"))
    }
    heights <- attr(lGrobs, "max_height")
    # Add title height if any
    if (!is.null(attr(lGrobs, "title"))) heights <- unit.c(attr(lGrobs, "titleHeight"),
                                                           attr(lGrobs, "line_height_and_spacing")[2],
                                                           heights)

    l_layout <- grid.layout(nrow=length(heights),
                            heights = heights,
                            ncol=length(widths),
                            widths=widths)
    lvp <- viewport(layout = l_layout,
                    name = "legend_details")
    pushViewport(lvp)
    row <- 1
    # Output title
    if (!is.null(attr(lGrobs, "title"))){
      vp <- viewport(layout.pos.row = 1)
      pushViewport(vp)
      pushViewport(viewport(width=attr(lGrobs, "titleWidth")))
      grid.draw(attr(lGrobs, "title"))
      upViewport(2)
      row <- 3
    }
    for (i in 1:length(lGrobs)){
      offset <- 4*(i-1)
      vp <- viewport(layout.pos.row = row,
                     layout.pos.col = 1 + offset,
                     xscale=c(0, 1))
      drawBox(vp, i, col, lGrobs)
      vp <- viewport(layout.pos.row = row,
                     layout.pos.col = 3 + offset)
      pushViewport(vp)
      grid.draw(lGrobs[[i]])
      upViewport()
    }
    upViewport()

  }else{
    # Output the vertical boxes and texts
    widths <- unit.c(boxSize, colgap, attr(lGrobs, "max_width"))

    # Remove bottom line
    heights <- attr(lGrobs, "line_height_and_spacing")[rep(1:2, length.out=length(lGrobs)*2-1)]
    #heights <- unit(convertUnit(heights, unitTo="npc", valueOnly=TRUE)/sum(convertUnit(heights, unitTo="npc", valueOnly=TRUE), "npc")
    # Add title height if any
    if (!is.null(attr(lGrobs, "title"))) heights <- unit.c(attr(lGrobs, "titleHeight"),
                                                           attr(lGrobs, "line_height_and_spacing")[2],
                                                           heights)

    l_layout <- grid.layout(ncol=length(widths),
                            nrow=length(heights),
                            widths=widths,
                            heights=heights)

    lvp <- viewport(layout = l_layout, just="left", x=0,
                    name="legend")
    pushViewport(lvp)
    row_start <- 1
    # Output title
    if (!is.null(attr(lGrobs, "title"))){
      vp <- viewport(layout.pos.row = 1)
      pushViewport(vp)
      grid.draw(attr(lGrobs, "title"))
      upViewport()
      row_start <- 3
    }

    for (i in 1:length(lGrobs)){
      vp <- viewport(layout.pos.row = row_start + (i-1)*2,
                     layout.pos.col = 1,
                     xscale=c(0,1))
      drawBox(vp, i, col, lGrobs)

      vp <- viewport(layout.pos.row = row_start + (i-1)*2,
                     layout.pos.col = 3)
      pushViewport(vp)
      grid.draw(lGrobs[[i]])
      upViewport()
    }
    upViewport()
  }

  if (length(gp) > 0){
    upViewport()
  }
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
prFpXrange <- function(upper, lower, clip, zero, xticks, xlog){
  top <- min(max(upper, na.rm = TRUE), clip[2])
  bottom <- max(min(lower, na.rm = TRUE), clip[1])
  # Although perhops not entirely intuitive
  # I've decided that the function should
  # extend the range to include the clip
  # endpoints unless there are prespecified
  # ticks indicating that the end-points aren't
  # included in the x-axis
  if (missing(xticks)){
    ret <- c(
      min(
        zero,
        bottom
      ),
      max(
        zero,
        top
      )
    )

  }else{
    ret <- c(
      min(
        c(zero, bottom, xticks)
      ),
      max(
        c(zero, top, xticks)
      )
    )
  }

  if (xlog){
    return(log(ret))
  }else{
    return(ret)
  }
}

#' Gets the forestplot labels
#'
#' A function that gets all the labels
#'
#' @param label_type The type of text labels
#' @param align Alignment, should be equal to \code{length(nc}
#' @param nc Number of columns
#' @param nr Number of rows
#' @return \code{list} A list with \code{length(nc)} where each element contains
#'  a list of \code{length(nr)} elements with attributes width/height for each
#'  element and max_width/max_height for the total
#'
#' @inheritParams forestplot
#' @keywords internal
prFpGetLabels <- function(label_type, labeltext, align,
                          nc, nr,
                          is.summary,
                          txt_gp,
                          col){
  labels <- vector("list", nc)

  if (attr(txt_gp$label, "txt_dim") %in% 0:1){
    txt_gp$label <-
      prListRep(list(prListRep(txt_gp$label, nc)), sum(!is.summary))
  }else{
    ncols <- sapply(txt_gp$label, length)
    if (all(ncols != ncols[1]))
      stop("Your fpTxtGp$label list has invalid number of columns",
           ", they should all be of equal length - yours have ",
           "'", paste(ncols, collapse="', '"), "'")
    if (length(txt_gp$label) != sum(!is.summary))
      stop("Your fpTxtGp$label list has invalid number of rows",
           ", the should be equal the of the number rows that aren't summaries.",
           " you have '", length(txt_gp$label) , "' rows in the fpTxtGp$label",
           ", while the labeltext argument has '", nr, "' rows",
           " where '", sum(!is.summary), "' are not summaries.")
  }

  if (attr(txt_gp$summary, "txt_dim") %in% 0:1){
    txt_gp$summary <-
      prListRep(list(prListRep(txt_gp$summary, nc)), sum(is.summary))
  }else{
    ncols <- sapply(txt_gp$summary, length)
    if (all(ncols != ncols[1]))
      stop("Your fpTxtGp$summary list has invalid number of columns",
           ", they should all be of equal length - yours have ",
           "'", paste(ncols, collapse="', '"), "'")
    if (length(txt_gp$summary) != sum(is.summary))
      stop("Your fpTxtGp$summary list has invalid number of rows",
           ", the should be equal the of the number rows that aren't summaries.",
           " you have '", length(txt_gp$summary) , "' rows in the fpTxtGp$summary",
           ", while the labeltext argument has '", nr, "' rows",
           " where '", sum(is.summary), "' are not summaries.")
  }

  max_height <- NULL
  max_width <- NULL
  # Walk through the labeltext
  # Creates a list matrix with
  # The column part
  for (j in 1:nc) {
    labels[[j]] <- vector("list", nr)

    # The row part
    for (i in 1:nr) {
      txt_out <- prFpFetchRowLabel(label_type, labeltext, i, j)
      # If it's a call created by bquote or similar it
      # needs evaluating
      if (is.call(txt_out))
        txt_out <- eval(txt_out)

      if (is.expression(txt_out) || is.character(txt_out) || is.numeric(txt_out)){
        x <- switch(align[j], l = 0, r = 1, c = 0.5)

        just <- switch(align[j],
                       l = "left",
                       r = "right",
                       c = "center")

        # Bold the text if this is a summary
        if (is.summary[i]){
          if (is.expression(txt_out)){
            x <- 0.5
          }else{
            x <- switch(align[j], l = 0, r = 1, c = 0.5)
          }

          gp_list <- txt_gp$summary[[sum(is.summary[1:i])]][[j]]
          gp_list[["col"]] <- rep(col$text, length = nr)[i]

          # Create a textGrob for the summary
          # The row/column order is in this order
          # in order to make the following possible:
          # list(rownames(x), list(expression(1 >= a), "b", "c"))
          labels[[j]][[i]] <-
            textGrob(txt_out, x = x,
                     just = just,
                     gp = do.call(gpar, gp_list))
        }else{
          gp_list <- txt_gp$label[[sum(!is.summary[1:i])]][[j]]
          if (is.null(gp_list$col))
            gp_list[["col"]] <- rep(col$text, length = nr)[i]

          # Create a textGrob with the current row-cell for the label
          labels[[j]][[i]] <-
            textGrob(txt_out, x = x,
                     just = just,
                     gp = do.call(gpar, gp_list))
        }

        attr(labels[[j]][[i]], "height") <- grobHeight(labels[[j]][[i]])
        attr(labels[[j]][[i]], "width") <- grobWidth(labels[[j]][[i]])
        if (is.null(max_height)){
          max_height <- attr(labels[[j]][[i]], "height")
          max_width <- attr(labels[[j]][[i]], "width")
        }else{
          max_height <- max(max_height, attr(labels[[j]][[i]], "height"))
          max_width <- max(max_width, attr(labels[[j]][[i]], "width"))
        }
      }
    }
  }
  attr(labels, "max_height") <- max_height
  attr(labels, "max_width") <- max_width
  attr(labels, "cex") <- ifelse(any(is.summary),
                                txt_gp$summary[[1]][[1]]$cex,
                                txt_gp$label[[1]][[1]]$cex)
  return(labels)
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
prFpFetchRowLabel <- function(label_type, labeltext, i, j){
  if (label_type=="expression"){
    # Haven't figured out it this is possible with
    # a multilevel expression
    row_column_text <- labeltext[[i]]
  }
  else if(label_type=="list"){
    # I get annoying warnings with this
    #if (!is.expression(labeltext[[j]][[i]]) && is.na(labeltext[[j]][[i]]))
    #    return(FALSE)
    row_column_text <- labeltext[[j]][[i]]
  }
  else{
    if (is.na(labeltext[i, j]))
      return(FALSE)
    row_column_text <- labeltext[i, j]
  }
  if (!is.expression(row_column_text) &&
        !is.call(row_column_text) &&
        is.na(row_column_text))
    return("")

  return(row_column_text)
}

#' Get the main foresplot
#'
#' The layout makes space for a legend if needed
#'
#' @param labels The labels
#' @param nr Number of rows
#' @param legend_layout A legend layout object if applicable
#' @return \code{viewport} Returns the viewport needed
#'
#' @inheritParams forestplot
#' @keywords internal
prFpGetLayoutVP <- function (lineheight, labels, nr, legend_layout = NULL) {
  if (!is.unit(lineheight)){
    if (lineheight == "auto"){
      lvp_height <- unit(1, "npc")
    }else if (lineheight == "lines"){
      lvp_height <- unit(nr*attr(labels, "cex")*1.5, "lines")
    }else{
      stop("The lineheight option '", lineheight, "'is yet not implemented")
    }
  }else{
    lvp_height <- unit(convertY(lineheight,
                                unitTo="lines",
                                valueOnly=TRUE)*nr,
                       "lines")
  }

  # If there is a legend on top then the size should be adjusted
  if (!is.null(legend_layout) &&
        legend_layout$nrow == 3 &&
        convertY(lvp_height, "npc", valueOnly=TRUE) < 1){
    lvp_height <- sum(lvp_height, legend_layout$heights[1:2])
  }

  lvp <- viewport(height=lvp_height,
                  layout = legend_layout,
                  name = ifelse(is.null(legend_layout), "main", "main_and_legend"))
  return (lvp)
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
prFpValidateLabelList <- function(labelList){
  l = length(labelList[[1]])
  if (length(labelList) == 1)
    return(TRUE)

  for(i in 2:length(labelList)){
    # All elements should have the same length
    if (l != length(labelList[[i]]))
      return(FALSE)
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
prFpFindWidestGrob <- function (grob.list, return_unit="mm"){
  len <- c()
  for (i in seq(along.with=grob.list)){
    if (is.object(grob.list[[i]])){
      # There is a tendency of underestemating grob size
      # when there are expressions
      grob_width <- convertWidth(grobWidth(grob.list[[i]]), return_unit, valueOnly=TRUE)
      len <- append(len, grob_width)
    }else{
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
prFpGetLegendBoxPosition <- function (pos) {
  valid_txt_pos <- c("bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", "right", "center")
  if (!all(c("x", "y") %in% names(pos)) &&
        !(("x" %in% pos &&
             any(pos[["x"]] == valid_txt_pos)) ||
            any(pos[[1]] == valid_txt_pos)))
    stop("If you want to specify the legend position in a certain corner",
         " within the main plot then you need to have list names x and y specified,",
         " or you should have the first list element to be '", paste(valid_txt_pos, collapse="'/'"), "',",
         " if you don't specify the first element then it can be the 'x' element")

  # Convert to the x & y format to make things easier
  if (!all(c("x", "y") %in% names(pos))){
    if ("x" %in% names(pos))
      txt_pos <- pos[["x"]]
    else
      txt_pos <- pos[[1]]

    # The inset offsets the position
    if (!"inset" %in% names(pos)){
      pos[["inset"]] <- unit(0, "npc")
    }else if (!is.unit(pos[["inset"]])){
      if (pos[["inset"]] > 1 || pos[["inset"]] < 0)
        stop("If you have not specified the unit of the pos inset then it should be between 0 and 1")
      pos[["inset"]] <- unit(pos[["inset"]], "npc")
    }else{
      if (convertUnit(pos[["inset"]], unitTo="npc", valueOnly=TRUE) > 1)
        stop("You have provided a value outside the possible range ('npc' bigger than 1)")
    }

    if (txt_pos == "bottomright"){
      pos[["x"]] <- unit(1, "npc") - pos[["inset"]]
      pos[["y"]] <- unit(0, "npc") + pos[["inset"]]
      pos[["just"]] <- c("right", "bottom")
    }else if(txt_pos == "bottom"){
      pos[["x"]] <- unit(0.5, "npc")
      pos[["y"]] <- unit(0, "npc") + pos[["inset"]]
      pos[["just"]] <- c("center", "bottom")
    }else if (txt_pos == "bottomleft"){
      pos[["x"]] <- unit(0, "npc") + pos[["inset"]]
      pos[["y"]] <- unit(0, "npc") + pos[["inset"]]
      pos[["just"]] <- c("left", "bottom")
    }else if (txt_pos == "left"){
      pos[["x"]] <- unit(0, "npc") + pos[["inset"]]
      pos[["y"]] <- unit(.5, "npc")
      pos[["just"]] <- c("left", "center")
    }else if (txt_pos == "topleft"){
      pos[["x"]] <- unit(0, "npc") + pos[["inset"]]
      pos[["y"]] <- unit(1, "npc") - pos[["inset"]]
      pos[["just"]] <- c("left", "top")
    }else if (txt_pos == "top"){
      pos[["x"]] <- unit(0.5, "npc")
      pos[["y"]] <- unit(1, "npc") - pos[["inset"]]
      pos[["just"]] <- c("center", "top")
    }else if (txt_pos == "topright"){
      pos[["x"]] <- unit(1, "npc") - pos[["inset"]]
      pos[["y"]] <- unit(1, "npc") - pos[["inset"]]
      pos[["just"]] <- c("right", "top")
    }else if (txt_pos == "right"){
      pos[["x"]] <- unit(1, "npc") - pos[["inset"]]
      pos[["y"]] <- unit(.5, "npc")
      pos[["just"]] <- c("right", "center")
    }else if (txt_pos == "center" || txt_pos == "centre"){
      pos[["x"]] <- unit(.5, "npc")
      pos[["y"]] <- unit(.5, "npc")
      pos[["just"]] <- c("center", "center")
    }else{
      stop("Position '", pos[["x"]], "'not yet implemented")
    }
  }else if(!"just" %in% names(pos)){
    pos[["just"]] <- c("center", "center")
  }
  return (pos)
}

#' Prepares the legend marker function
#'
#' @param fn.legend The unknown parameter
#' @param col_no The number of columns
#' @param fn.ci_norm The original fn.ci_norm input
#' @return \code{list}
#'
#' @keywords internal
prFpPrepareLegendMarker <- function (fn.legend, col_no, fn.ci_norm) {
  if (!missing(fn.legend)){
    if (is.function(fn.legend)){
      return(lapply(1:col_no, function(x) fn.legend))
    }
    if(is.character(fn.legend)){
      if (length(fn.legend) == 1){
        fn.legend <- rep(fn.legend, times=col_no)
      }else if (length(fn.legend) != col_no){
        stop("The number of legend markers, ", length(fn.legend),
             ", should be the same as the number of columns for the mean, ", col_no)
      }

      tmp <- list()
      for (i in 1:length(fn.legend)){
        tmp[[i]] <- get(fn.legend[i])
      }

      return(tmp)
    }

    if(is.list(fn.legend)){
      if(length(fn.legend) != col_no){
        stop("The number of legend markers, ", length(fn.legend), ",",
             " should be the same as the number of columns for the mean, ", col_no)
      }else if(!all(sapply(fn.legend, function(x) is.function(x)))){
        stop("If you provide a list for fn.legend then each element should be a function")
      }

      return(fn.legend)
    }

    stop("The legend marked function designated by the fn.legend",
         " is neither a character or a function")
  }

  if (length(fn.ci_norm) == col_no){
    return(prFpGetConfintFnList(fn = fn.ci_norm,
                                no_rows = NROW(mean),
                                no_cols = col_no)[[1]])
  }

  # Not sure what to do if the number don't match the number of legends
  # and it ain't 1 and it therefore defaults to the normal confidence
  # interval marker
  if (length(fn.ci_norm) != 1)
    fn.ci_norm <- fpDrawNormalCI

  return(lapply(1:col_no, function(x) fn.ci_norm))
}

#' Converts a 2D or 3D array to mean, lower, upper
#'
#' @param x The array to convert
#' @return \code{list(mean = mean, lower = lower, upper = upper)}
#' @keywords internal
prFpConvertMultidimArray <- function(x){
  switch(as.character(length(dim(x))),
         "2" = {
           # Loop through the different rows as a row with only a label may have NA in it
           lower_cnr <- NULL
           upper_cnr <- NULL
           for (d1 in dim(x)[1]){
             if (length(unique(x[d1,])) < 3)
               next;

             lower_cnr <- which.min(x[d1,])
             upper_cnr <- which.max(x[d1,])
             if (length(lower_cnr) == 1 &&
                   length(upper_cnr) == 1){
               break;
             }
           }
           if (length(lower_cnr) != 1 ||
                 length(upper_cnr) != 1)
             stop("Sorry did not manage to automatically identify",
                  " the upper/lower boundaries.")

           lower <- x[,lower_cnr,drop=TRUE]
           upper <- x[,upper_cnr,drop=TRUE]
           mean <- x[,-c(upper_cnr, lower_cnr),drop=TRUE]},
         "3" = {
           # Loop through the different rows as a row with only a label may have NA in it
           # this is a little complicated as we're doing a 3D loop and exiting
           # as soon as the vars have been identified
           lower_cnr <- NULL
           upper_cnr <- NULL
           for (d3 in 1:dim(x)[3]){
             for (d1 in 1:dim(x)[1]){
               if (length(unique(x[d1,,d3])) < 3)
                 next;

               lower_cnr <- which.min(x[d1,,d3])
               upper_cnr <- which.max(x[d1,,d3])

               if (length(lower_cnr) == 1 &&
                     length(upper_cnr) == 1)
                 break;
             }
             if (length(lower_cnr) == 1 &&
                   length(upper_cnr) == 1)
               break;

           }
           if (length(lower_cnr) != 1 ||
                 length(upper_cnr) != 1)
             stop("Sorry did not manage to automatically identify",
                  " the upper/lower boundaries.")
           lower_cnr <- which.min(x[1,,1])
           upper_cnr <- which.max(x[1,,1])
           lower <- x[,lower_cnr,,drop=TRUE]
           upper <- x[,upper_cnr,,drop=TRUE]
           mean <- x[,-c(upper_cnr, lower_cnr),,drop=TRUE]},{
             stop("Invalid number of dimensions of the mean argument,",
                  " should be either 2 or 3 - you have '", length(dim(mean)),"'")
           })
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
prPushMarginViewport <- function(bottom, left, top, right, name=NULL){
  if (!is.unit(bottom))
    bottom <- unit(bottom, "npc")

  if (!is.unit(top))
    top <- unit(top, "npc")

  if (!is.unit(left))
    left <- unit(left, "npc")

  if (!is.unit(right))
    right <- unit(right, "npc")

  layout_name <- NULL
  if (!is.character(name))
    layout_name <- sprintf("margin_grid_%s", name)

  gl <- grid.layout(nrow=3, ncol=3,
                    heights = unit.c(top, unit(1, "npc") - top - bottom, bottom),
                    widths = unit.c(left, unit(1, "npc") - left - right, right))

  pushViewport(viewport(layout=gl, name=layout_name))
  pushViewport(viewport(layout.pos.row=2, layout.pos.col=2, name=name))
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
                            space_below){
  tg_list <- list(
    label = title,
    just = "center")
  if (!is.null(gp$just)){
    tg_list$just <- gp$just
    gp$just <- NULL
  }
  tg_list$gp <- do.call(gpar, gp)

  titleGrob <- do.call(textGrob,
                       tg_list)

  # The y/g/j letters are not included in the height
  gh <- unit(convertUnit(grobHeight(titleGrob), "mm", valueOnly=TRUE)*1.5, "mm")
  if (missing(space_below)){
    space_below <- unit(convertUnit(gh, "mm", valueOnly=TRUE)/2, "mm")
  }else if (!is.unit(space_below)){
    space_below <- unit(space_below, "npc")
  }

  gl <- grid.layout(nrow=3, ncol=1,
                    heights = unit.c(gh, space_below, unit(1, "npc") - space_below - gh))

  pushViewport(viewport(layout=gl, name="title_layout"))
  pushViewport(viewport(layout.pos.row=1, name="title"))
  grid.draw(titleGrob)
  upViewport()

  pushViewport(viewport(layout.pos.row=3, name="main"))
}

#' Just a simple acces to the gp$cex parameter
#'
#' @param x The text-grob of interest
#' @return \code{numeric} The cex value, 1 if no cex was present
#' @keywords internal
prGetTextGrobCex <-  function(x) {
  cex <- 1
  if (!is.null(x$gp$cex))
    cex <- x$gp$cex

  return(cex)
}


#' Prepares the hrzl_lines for the plot
#'
#' @param total_columns Total number of columns
#' @inheritParams forestplot
#' @keywords internal
prFpGetLines <- function(hrzl_lines,
                         is.summary,
                         total_columns,
                         col){
  ret_lines <- lapply(1:(length(is.summary) + 1), function(x) NULL)
  if (missing(hrzl_lines) ||
        (is.logical(hrzl_lines) &&
           all(hrzl_lines == FALSE)) ||
        (is.list(hrzl_lines) &&
           all(sapply(hrzl_lines, is.null)))){
    return(ret_lines)
  }

  std_line <- gpar(lty=1, lwd=1, col=col$hrz_lines, columns = 1:total_columns)
  if (inherits(hrzl_lines, "gpar")){
    std_line <- prGparMerge(std_line, hrzl_lines)
    hrzl_lines <- TRUE
  }

  # If provided with TRUE alone
  # Note that FALSE has already been processed above
  if (is.logical(hrzl_lines) &&
        length(hrzl_lines) == 1){
      if (is.summary[1] == TRUE){
        line_pos <- which(is.summary == FALSE)[1]
        ret_lines[[line_pos]] <-
          std_line

        is.summary[1:line_pos] <- FALSE
      }

      if (tail(is.summary, 1)){
        line_pos <- length(is.summary) + 1-
          (which(rev(is.summary) == FALSE)[1] - 1)

        ret_lines[[line_pos]] <-
          std_line

        ret_lines[[length(ret_lines)]] <-
          std_line

        is.summary[line_pos:length(is.summary)] <- FALSE
      }

      for (line_pos in which(is.summary == TRUE)){
        if (is.summary[line_pos + 1]){
          line_pos <-
            line_pos +
            tail(which(is.summary[(line_pos + 1):length(is.summary)]), 1)
        }
        ret_lines[[line_pos + 1]] <-
          std_line
      }

      return(ret_lines)
  }

  if (is.logical(hrzl_lines)){
    if (length(hrzl_lines) == (length(is.summary) + 1)){
      ret_lines[[hrzl_lines]] <-
        std_line
      return(ret_lines)
    }else{
      stop("You have provided a logical hrzl_lines input of length '", length(hrzl_lines), "'",
           " but the software expects the length to be number of rows + 1",
           " i.e. ", length(is.summary), " + 1 = ", length(is.summary) + 1)
    }
  }

  if (!is.list(hrzl_lines)){
    stop("You have provided an invalid argument, expected a list but got a ", class(hrzl_lines))
  }

  if (is.null(names(hrzl_lines))){
    if (length(hrzl_lines) == (length(is.summary) + 1)){
      return(lapply(hrzl_lines, function(x, std) {
        if (is.null(x)) {
          x
        }else if (inherits(x, "gpar")){
          prGparMerge(std, x)
        }else{
          std
        }
      }
      , std = std_line))
    }else{
      stop("You have provided a logical hrzl_lines input of length '", length(hrzl_lines), "'",
           " but the software expects the length to be number of rows + 1",
           " i.e. ", length(is.summary), " + 1 = ", length(is.summary) + 1)
    }
  }

  if (!all(sapply(hrzl_lines, function(x) inherits(x, "gpar") || x == TRUE)))
    stop("The list must consist of only gpar or logical TRUE elements")

  for (n in names(hrzl_lines)){
    nn <- as.integer(n)
    if (is.na(nn))
      stop("Your name '", n ,"' for the list gpars cannot be converted to an integer")
    if (!nn %in% 1:(length(is.summary) + 1))
      stop("The integer that you have provided '", n, "'",
           " falls outside the scope of possible values 1:", length(is.summary) + 1)
    if (is.logical(hrzl_lines[[n]])){
      ret_lines[[nn]] <-
        std_line
    }else{
      ret_lines[[nn]] <-
        prGparMerge(std_line, hrzl_lines[[n]])
    }
  }

  return(ret_lines)
}

#' Draws the horizontal lines
#'
#' @param nr Number of rows
#' @param colwidths Vector with column widths
#' @inheritParams prFpGetLines
#' @inheritParams forestplot
#' @keywords internal
prFpDrawLines <- function(hrzl_lines, nr, colwidths,
                          graph.pos){
  getCSpan <- function (columns, colwidths) {
    span_cols <- c()
    col_pos <- NULL
    for (i in 1:length(columns)){
      pos <- columns[i]
      pos <- pos*2 - 1
      span_cols <- c(span_cols, pos)

      if (pos < length(colwidths) &&
            i != length(columns) &&
            columns[i] + 1 == columns[i + 1])
        span_cols <- c(span_cols, pos + 1)
    }

    span_cols
  }

  for (i in 1:nr) {
    if (!is.null(hrzl_lines[[i]])){
      span_cols <- getCSpan(hrzl_lines[[i]]$columns, colwidths)

      for(c in span_cols){
        line_vp <- viewport(layout.pos.row = i,
                            layout.pos.col = c)
        pushViewport(line_vp)
        grid.lines(y = unit(c(1,1), "npc"), gp = hrzl_lines[[i]])
        popViewport()
      }
    }

    if (i == nr &&
          !is.null(hrzl_lines[[i + 1]])){
      span_cols <- getCSpan(hrzl_lines[[i + 1]]$columns, colwidths)

      line_vp <- viewport(layout.pos.row = i,
                          layout.pos.col = span_cols)
      pushViewport(line_vp)
      grid.lines(y = unit(c(0,0), "npc"), gp = hrzl_lines[[i + 1]])
      popViewport()
    }
  }
}

#' Merges two \code{\link[grid]{gpar}} elements
#'
#' The second elements overrides any conflicting elements within the first
#'
#' @param l1 A \code{\link[grid]{gpar}} element
#' @param l2 A \code{\link[grid]{gpar}} element
#' @return Returns a \code{\link[grid]{gpar}} element
#' @keywords internal
prGparMerge <- function(l1, l2){
  out <- c(l1, l2)
  if (!any(duplicated(names(out))))
    return(out)

  dups <- unique(names(out)[duplicated(names(out))])
  for (n in dups){
    wd <- which(names(out) == n)
    out <- out[-wd[1:(length(wd) - 1)]]
  }
  class(out) <- unique(c(class(out), class(l1)))
  return(out)
}

