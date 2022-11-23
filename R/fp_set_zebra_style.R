#' Decorate the plot with a zebra pattern
#'
#' @param x The forestplot object
#' @param ... The styles for each row
#' @param ignore_subheaders The zebra will automatically restart at sub-headers,
#'   i.e. when there is a *summary* row that doesn't have any values.
#'
#' @return The forestplot object with the zebra style
#' @export
#' @family graph modifiers
#'
#' @example inst/examples/fp_set_zebra_example.R
#' @family forestplot functions
fp_set_zebra_style <- function(x, ..., ignore_subheaders = FALSE) {
  zebra_styles <- list(...) |>
    lapply(function(style) {
      if (is.grob(style)) return(style)

      if (is.character(style)) {
        return(gpar(fill = style, col = style))
      }

      if (is.list(style)) {
        return(style)
      }

      stop("Unknown style: ", style,
           " only grob, character and gpar() allowed")
    })

  if (length(zebra_styles) == 1) {
    zebra_styles <- c(list(NA), zebra_styles)
  }

  x$zebra_styles <- zebra_styles
  x$zebra_styles_ignore_subheaders <- ignore_subheaders

  return(x)
}

plotZebraStyle <- function(obj) {
  if (is.null(obj$zebra_styles)) return()
  estimates <- obj$estimates

  empty_header_rows <- apply(estimates, \(x) all(is.na(x)), MARGIN = 1)
  last_headers <- which(head(empty_header_rows, length(empty_header_rows) - 1) & !tail(empty_header_rows, length(empty_header_rows) - 1))
  if (length(last_headers) == 0) {
    last_headers <- 0
  }

  if (isTRUE(obj$zebra_styles_ignore_subheaders)) {
    last_headers <- last_headers[1]
  }

  for (i in 1:length(last_headers)) {
    last_header <- last_headers[i]
    if (i == length(last_headers)) {
      rows <- nrow(estimates)
    } else {
      rows <- last_headers[i + 1] - 1
    }

    styles <- rep(obj$zebra_styles, length.out = rows - last_header)
    for (i in 1:(rows - last_header)) {
      pushViewport(viewport(
        layout.pos.row = last_header + i,
        name = paste("Zebra", i)
      ))
      if (is.grob(styles[[i]])) {
        grid.draw(styles[[i]])
      } else if (!all(is.na(styles[[i]]))) {
        grid.rect(gp = styles[[i]])
      }
      upViewport()
    }
  }
}
