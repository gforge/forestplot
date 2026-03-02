library(testthat)

context("column spanning with fp_span")

# helper to quickly build minimal forestplot object
build <- function(span = NULL) {
    lab <- list(list("x", "y"), list("u", "v"))
    if (!is.null(span)) {
        lab[[1]][[1]] <- fp_span(lab[[1]][[1]], columns = span)[[1]]
    }
    forestplot(
        labeltext = lab,
        mean = c(1, 2),
        lower = c(0.5, 1),
        upper = c(1.5, 2.5)
    )
}


test_that("fp_span sets span attribute and is overwritten by later calls", {
    txt <- "hello"
    out1 <- fp_span(txt, columns = c(1, 2))
    expect_equal(attr(out1[[1]], "span"), c(1L, 2L))
    out2 <- fp_span(out1, columns = 2)
    expect_equal(attr(out2[[1]], "span"), 2L)
})


test_that("span attribute propagated through prGetLabelsList", {
    obj <- build(span = c(1, 2))
    lbls <- prGetLabelsList(
        labels = obj$labels,
        align = obj$align,
        is.summary = obj$is.summary,
        txt_gp = obj$txt_gp,
        col = obj$col
    )
    grob <- lbls[[1]][[1]]
    expect_equal(attr(grob, "span"), c(1L, 2L))
    # alignment should default to centered when spanning multiple columns
    expect_equal(grob$just, "center")
    # x position is stored as a unit; compare in npc space
    expect_equal(as.numeric(convertUnit(grob$x, "npc", valueOnly = TRUE)), 0.5)
})


test_that("invalid span values throw an error", {
    expect_error(fp_span("x", columns = c(0, 5)), "integer vector")
})


test_that("fp_span composes with alignment and bold styling", {
    # apply span then align and bold; then also try reverse order
    txt <- "combo"
    combo1 <- txt |>
        fp_span(columns = c(1, 2)) |>
        fp_align_right() |>
        fp_txt_bold()
    expect_equal(attr(combo1[[1]], "span"), c(1L, 2L))
    expect_equal(attr(combo1[[1]], "align"), "r")
    expect_equal(attr(combo1[[1]], "txt_gp")$fontface, "bold")

    combo2 <- txt |>
        fp_align_center() |>
        fp_span(columns = c(2, 3)) |>
        fp_txt_bold()
    expect_equal(attr(combo2[[1]], "span"), c(2L, 3L))
    expect_equal(attr(combo2[[1]], "align"), "c")
    expect_equal(attr(combo2[[1]], "txt_gp")$fontface, "bold")
})


# test for grob values in labeltext

test_that("labeltext can contain grid grobs without error", {
    # create a label list where second column consists of grobs
    groblist <- lapply(1:3, function(i) grid::textGrob(paste0("G", i)))
    # list of two columns; each column is a list of length 3
    lbl <- list(
        A = list("x", "y", "z"),
        B = groblist
    )
    # should not error when creating or printing
    obj <- forestplot(
        labeltext = lbl,
        mean = c(1, 2, 3),
        lower = c(0.5, 1, 2),
        upper = c(1.5, 2, 3)
    )

    lbls <- prGetLabelsList(obj$labels, obj$align, obj$is.summary, obj$txt_gp, obj$col)
    expect_true(inherits(lbls[[2]][[1]], "grob"))

    expect_s3_class(obj, "gforge_forestplot")
    expect_silent(print(obj))
})

# visual/viewport test
library(grid)

test_that("Viewport spans multiple layout columns", {
    obj <- build(span = c(1, 2))
    lbls <- prGetLabelsList(obj$labels, obj$align, obj$is.summary, obj$txt_gp, obj$col)
    # open a new page to allow grid viewports
    grid.newpage()
    prFpPrintLabels(
        labels = lbls,
        nc = attr(lbls, "no_cols"),
        nr = attr(lbls, "no_rows"),
        graph.pos = obj$graph.pos
    )
    # look for any Label_vp viewport and inspect its layout
    ls <- grid.ls(viewports = TRUE, print = FALSE)
    vpnames <- ls$name[grepl("Label_vp", ls$name)]
    expect_true(length(vpnames) > 0)
    found <- FALSE
    for (n in vpnames) {
        seekViewport(n)
        if (length(current.viewport()$layout.pos.col) > 1) {
            found <- TRUE
            break
        }
        upViewport()
    }
    expect_true(found)
})
