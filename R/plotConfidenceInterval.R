plotConfidenceInterval <- function(obj, axisList, info, labels, fn.ci_sum, fn.ci_norm, lty.ci) {
  # Output the different confidence intervals
  for (i in 1:nrow(obj$estimates)) {
    # The line and box colors may vary
    clr.line <- rep(obj$col$line, length.out = dim(obj$estimates)[3])
    clr.marker <- rep(obj$col$box, length.out = dim(obj$estimates)[3])
    clr.summary <- rep(obj$col$summary, length.out = dim(obj$estimates)[3])

    line_vp <- viewport(
      layout.pos.row = i,
      layout.pos.col = obj$graph.pos * 2 - 1,
      xscale = axisList$x_range,
      name = sprintf("Line_%d_%d", i, obj$graph.pos * 2 - 1)
    )
    pushViewport(line_vp)

    # Draw multiple confidence intervals
    if (dim(obj$estimates)[3] > 1) {
      b_height <- max(info[i,], na.rm = TRUE)
      if (is.unit(b_height)) {
        b_height <- convertUnit(b_height, unitTo = "npc", valueOnly = TRUE)
      }

      if (is.null(obj$line.margin)) {
        obj$line.margin <- .1 + .2 / (dim(obj$estimates)[3] - 1)
      } else if (is.unit(obj$line.margin)) {
        obj$line.margin <- convertUnit(obj$line.margin, unitTo = "npc", valueOnly = TRUE)
      }
      y.offset_base <- b_height / 2 + obj$line.margin
      y.offset_increase <- (1 - obj$line.margin * 2 - b_height) / (dim(obj$estimates)[3] - 1)

      for (j in dim(obj$estimates)[3]:1) {
        # Start from the bottom and plot up
        # the one on top should always be
        # above the one below
        current_y.offset <- y.offset_base + (dim(obj$estimates)[3] - j) * y.offset_increase
        if (is.na(obj$estimates[i, 1, j])) {
          next
        }

        shape_coordinates <- c(i, j)
        attr(shape_coordinates, "max.coords") <- c(attr(labels, "no_rows"), dim(obj$estimates)[3])

        if (obj$is.summary[i]) {
          call_list <-
            list(fn.ci_sum[[i]][[j]],
                 estimate = obj$estimates[i, 1, j],
                 lower_limit = obj$estimates[i, 2, j],
                 upper_limit = obj$estimates[i, 3, j],
                 size = info[i, j],
                 y.offset = current_y.offset,
                 col = clr.summary[j],
                 shapes_gp = obj$shapes_gp,
                 shape_coordinates = shape_coordinates
            )
        } else {
          call_list <-
            list(fn.ci_norm[[i]][[j]],
                 estimate = obj$estimates[i, 1, j],
                 lower_limit = obj$estimates[i, 2, j],
                 upper_limit = obj$estimates[i, 3, j],
                 size = info[i, j],
                 y.offset = current_y.offset,
                 clr.line = clr.line[j],
                 clr.marker = clr.marker[j],
                 lty = lty.ci[[i]][[j]],
                 vertices.height = obj$ci.vertices.height,
                 shapes_gp = obj$shapes_gp,
                 shape_coordinates = shape_coordinates
            )

          if (!is.null(obj$ci.vertices)) {
            call_list$vertices <- obj$ci.vertices
          }

          if (!is.null(obj$lwd.ci)) {
            call_list$lwd <- obj$lwd.ci
          }
        }


        # Add additional arguments that are passed on
        # from the original parameters
        for (name in names(obj$extra_arguments)) {
          call_list[[name]] <- obj$extra_arguments[[name]]
        }

        # Do the actual drawing of the object
        tryCatch(eval(as.call(call_list)),
                 error = function(e) {
                   stop("On row ", i, " the print of the estimate failed: ", e$message)
                 }
        )
      }
    } else {
      shape_coordinates <- c(i, 1)
      attr(shape_coordinates, "max.coords") <- c(attr(labels, "no_rows"), 1)

      if (obj$is.summary[i]) {
        call_list <-
          list(fn.ci_sum[[i]],
               estimate = obj$estimates[i, 1, 1],
               lower_limit = obj$estimates[i, 2, 1],
               upper_limit = obj$estimates[i, 3, 1],
               size = info[i, 1],
               col = clr.summary,
               shapes_gp = obj$shapes_gp,
               shape_coordinates = shape_coordinates
          )
      } else {
        call_list <-
          list(fn.ci_norm[[i]],
               estimate = obj$estimates[i, 1, 1],
               lower_limit = obj$estimates[i, 2, 1],
               upper_limit = obj$estimates[i, 3, 1],
               size = info[i, 1],
               clr.line = clr.line,
               clr.marker = clr.marker,
               lty = lty.ci[[i]][[1]],
               vertices.height = obj$ci.vertices.height,
               shapes_gp = obj$shapes_gp,
               shape_coordinates = shape_coordinates
          )

        if (!is.null(obj$ci.vertices)) {
          call_list$vertices <- obj$ci.vertices
        }

        if (!is.null(obj$lwd.ci)) {
          call_list$lwd <- obj$lwd.ci
        }
      }

      # Add additional arguments that are passed on
      # from the original parameters
      for (name in names(obj$extra_arguments)) {
        call_list[[name]] <- obj$extra_arguments[[name]]
      }

      # Do the actual drawing of the object
      if (!all(is.na(obj$estimates[i, 1, 1]))) {
        tryCatch(eval(as.call(call_list)),
                 error = function(e) {
                   stop("On row ", i, " the print of the estimate failed: ", e$message)
                 }
        )
      }
    }

    upViewport()
  }
}

