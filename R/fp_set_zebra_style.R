#' Decorate the plot with a zebra pattern
#'
#' @param x The forestplot object
#' @param ... The styles for each row
#'
#' @return The forestplot object with the zebra style
#' @export
#' @family graph modifiers
#'
#' @example inst/examples/fp_set_zebra_example.R
fp_set_zebra_style <- function(x, ...) {
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

  return(x)
}

plotZebraStyle <- function(obj) {
  if (is.null(obj$zebra_styles)) return()
  estimates <- obj$estimates

  last_header <- which(apply(estimates, \(x) all(is.na(x)), MARGIN = 1)) |> tail(1)
  styles <- rep(obj$zebra_styles, length.out = nrow(estimates) - last_header)
  for (i in 1:(nrow(estimates) - last_header)) {
    pushViewport(viewport(
      layout.pos.row = last_header + i,
      name = paste("Zebra", i)
    ))
    if (is.grob(styles[[i]])) {
      grid.draw(styles[[i]])
    } else if (!all(is.na(styles[[i]]))){
      grid.rect(gp = styles[[i]])
    }
    upViewport()
  }
}
