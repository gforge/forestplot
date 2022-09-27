#' Set the style of the graph
#'
#' Sets the output style associated with the `foresplot`
#'
#' @inheritParams fp_insert_row
#' @inheritParams fpShapesGp
#' @param txt_gp Set the fonts etc for all text elements. See [`fpTxtGp()`]
#'   for details
#'
#' @return The foresplot object with the styles
#' @export
#'
#' @example inst/examples/fp_set_style_example.R
#' @rdname style_manipulation
fp_set_style <- function(x,
                         default = NULL,
                         box = NULL,
                         lines = NULL,
                         vertices = NULL,
                         summary = NULL,
                         zero = NULL,
                         axes = NULL,
                         hrz_lines = NULL,
                         grid = NULL,
                         txt_gp = NULL) {
  new_gp <- fpShapesGp(default = default,
                       box = box,
                       lines = lines,
                       vertices = vertices,
                       summary = summary,
                       zero = zero,
                       axes = axes,
                       hrz_lines = hrz_lines,
                       grid = grid)
  for (n in names(x$shapes_gp)) {
    if (!is.null(new_gp[[n]])) {
      x$shapes_gp[[n]] <- new_gp[[n]]
    }
  }

  if (!is.null(txt_gp)) {
    x$txt_gp <- txt_gp
  }

  return(x)
}

