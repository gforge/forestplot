#' Gets the legend to output
#'
#' @param legend The legend to output
#' @param txt_gp The text styling
#' @param legend_args Legend arguments
#' @param colgap The column gap
#' @param shapes_gp The shapes for the legend
#' @param lineheight The line height
#' @param fn.legend The function for plotting the legend
#' @returns `forestplot_legend` object with attributes `main` and `pos`
#' @noRd
buildLegend <- function(legend,
                        txt_gp,
                        legend_args,
                        colgap,
                        shapes_gp,
                        lineheight,
                        fn.legend) {
  if (all(is.na(legend))) {
    return(structure(list(),
      pos = NULL,
      main = NULL,
      class = "forestplot_legend"
    ))
  }

  lGrobs <- list()
  max_width <- 0
  max_height <- 0
  gp <- prListRep(txt_gp$legend, length.out = length(legend))
  for (n in 1:length(legend)) {
    lGrobs[[n]] <- textGrob(legend[n],
                            x = 0, just = "left",
                            gp = do.call(gpar, gp[[n]])
    )

    gw <- convertUnit(grobWidth(lGrobs[[n]]), "mm", valueOnly = TRUE)
    gh <- convertUnit(grobHeight(lGrobs[[n]]), "mm", valueOnly = TRUE)
    if (gw > max_width) {
      max_width <- gw
    }
    if (gh > max_height) {
      max_height <- gh
    }

    attr(lGrobs[[n]], "width") <- unit(gw, "mm")
    attr(lGrobs[[n]], "height") <- unit(gh, "mm")
  }
  max_height <- unit(max_height, "mm")
  max_width <- unit(max_width, "mm")
  line_height_and_spacing <- unit.c(max_height, unit(.5, "lines"))

  title_attributes <- list()
  # Do title stuff if present
  if (is.character(legend_args$title)) {
    title <- textGrob(legend_args$title,
                      x = 0, just = "left",
                      gp = do.call(gpar, txt_gp$legend.title))
    title_attributes$title <- title

    title_attributes$titleHeight <- grobHeight(title)
    title_attributes$titleWidth <- grobHeight(title)
    if (convertUnit(title_attributes$titleWidth, unitTo = "npc", valueOnly = TRUE) >
        convertUnit(max_width, unitTo = "npc", valueOnly = TRUE)) {
      max_width <- title_attributes$titleWidth
    }
  }


  legend_colgap <- colgap
  if (convertUnit(legend_colgap, unitTo = "mm", valueOnly = TRUE) >
    convertUnit(max_height, unitTo = "mm", valueOnly = TRUE)) {
    legend_colgap <- max_height
  }

  legend_horizontal_height <- sum(
    legend_args$padding,
    max_height,
    legend_args$padding
  )
  if (!is.null(title_attributes$title)) {
    legend_horizontal_height <- unit.c(
      title_attributes$titleHeight,
      line_height_and_spacing[2],
      legend_horizontal_height) |>
      sum()
  }

  legend_vertical_width <- unit.c(
    legend_args$padding,
    max_height,
    legend_colgap,
    max_width,
    legend_args$padding
  ) |> sum()

  # Prepare the viewports if the legend is not
  # positioned inside the forestplot, i.e. on the top or right side
  if ((!is.list(legend_args$pos) && legend_args$pos == "top") ||
    ("align" %in% names(legend_args$pos) && legend_args$pos[["align"]] == "horizontal")) {
    legend_layout <- grid.layout(
      nrow = 3, ncol = 1,
      heights = unit.c(
        legend_horizontal_height,
        legend_colgap + legend_colgap,
        unit(1, "npc") -
          legend_horizontal_height -
          legend_colgap -
          legend_colgap
      )
    )

    legend_pos <- list(
      row = 1,
      col = 1
    )
    main_pos <- list(
      row = 3,
      col = 1
    )
  } else {
    legend_layout <- grid.layout(
      nrow = 1, ncol = 3,
      widths = unit.c(
        unit(1, "npc") -
          legend_colgap -
          legend_vertical_width,
        legend_colgap,
        legend_vertical_width
      )
    )
    legend_pos <- list(
      row = 1,
      col = 3
    )
    main_pos <- list(
      row = 1,
      col = 1
    )
  }

  position_desc <- legend_args$pos
  if (!is.list(position_desc)) {
    position_desc <- structure(legend_pos,
                               class = "forestplot_legend_position")
  }

  lGrobs |>
    structure(layout = legend_layout,
              pos = position_desc,
              main = main_pos,
              gp = legend_args$gp,
              r = legend_args$r,
              padding = legend_args$padding,
              shapes_gp = shapes_gp,
              max_height = max_height,
              max_width = max_width,
              line_height_and_spacing  = line_height_and_spacing,
              title = title_attributes$title,
              titleHeight = title_attributes$titleHeight,
              titleWidth = title_attributes$titleWidth,
              colgap = legend_colgap,
              lineheight = lineheight,
              fn.legend = fn.legend,
              class = c("forestplot_legend", class(lGrobs)))
}
