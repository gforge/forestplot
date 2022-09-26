#' @importFrom abind adrop
prepBoxSize <- function(boxsize, estimates, is.summary, txt_gp) {
  # Create the fourth argument 4 the fpDrawNormalCI() function
  if (!is.null(boxsize)) {
    # If matrix is provided this will convert it
    # to a vector but it doesn't matter in this case
    return(matrix(boxsize,
                  nrow = nrow(estimates),
                  ncol = dim(estimates)[3]))
  }


  # Get width of the lines, upper CI - lower CI
  cwidth <- (estimates[,3,,drop = FALSE] - estimates[,2,,drop = FALSE])

  # Set cwidth to min value if the value is invalid
  # this can be the case for reference points
  cwidth[cwidth <= 0] <- min(cwidth[cwidth > 0], na.rm = TRUE)
  cwidth[is.na(cwidth)] <- min(cwidth, na.rm = TRUE)

  # As the line may be very high we want the box to relate to actual box height
  textHeight <- convertUnit(grobHeight(textGrob("A", gp = do.call(gpar, txt_gp$label))),
                            unitTo = "npc",
                            valueOnly = TRUE)

  boxsize <- 1 / cwidth * 0.75
  if (!all(is.summary)) {
    boxsize <- boxsize / max(boxsize[!is.summary,,], na.rm = TRUE)

    # Adjust the dots as it gets ridiculous with small text and huge dots
    if (any(textHeight * (nrow(estimates) + .5) * 1.5 < boxsize)) {
      boxsize <- textHeight * (nrow(estimates) + .5) * 1.5 * boxsize / max(boxsize, na.rm = TRUE) + textHeight * (nrow(estimates) + .5) * 1.5 / 4
    }
  }

  # Set summary to maximum size
  boxsize[is.summary,,] <- 1 / dim(estimates)[3]
  return(abind::adrop(boxsize, drop = 2))
}
