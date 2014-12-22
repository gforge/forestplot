
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
#' @param base_cex The base cex used for the plot
#' @param cex_mult The multiplier of the base - i.e. the increase of the
#'  text size for the title as compared to the general
#' @param fontface The type of fontfacte
#' @param space_below The space below, defaults to 1/5 of the title height
#' @return \code{NULL} The function does not return a value
#'
#' @keywords internal
prGridPlotTitle <- function(title,
                            base_cex,
                            cex_mult = 1.2,
                            fontface = "bold",
                            space_below = NULL){
  titleGrob <- textGrob(title, just="center",
                        gp=gpar(fontface = fontface,
                                cex = base_cex*cex_mult))

  # The y/g/j letters are not included in the height
  gh <- unit(convertUnit(grobHeight(titleGrob), "mm", valueOnly=TRUE)*1.5, "mm")
  if (is.null(space_below)){
    space_below <- unit(convertUnit(gh, "mm", valueOnly=TRUE)/3, "mm")
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