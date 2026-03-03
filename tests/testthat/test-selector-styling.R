library(testthat)

context("selector-based styling helpers")

build_named_plot <- function() {
    lbl <- list(
        type = list("header", "study", "subtotal"),
        val = list("A", "1", "Sum")
    )
    forestplot(
        labeltext = lbl,
        mean = c(1, 1.2, 1.1),
        lower = c(0.8, 1.0, 0.9),
        upper = c(1.3, 1.4, 1.35)
    )
}

test_that("fp_align_where works for explicit row/col indices", {
    obj <- build_named_plot()
    out <- fp_align_where(obj, align = "r", rows = 1, cols = 2)

    expect_equal(attr(out$labels[[2]][[1]], "align"), "r")
    expect_null(attr(out$labels[[2]][[2]], "align"))
})

test_that("fp_align_where supports column name selectors", {
    obj <- build_named_plot()
    out <- fp_align_where(obj, align = "r", cols = "val")

    expect_equal(attr(out$labels[[2]][[1]], "align"), "r")
    expect_equal(attr(out$labels[[2]][[2]], "align"), "r")
    expect_equal(attr(out$labels[[2]][[3]], "align"), "r")
})

test_that("fp_txt_where supports formula row predicates", {
    obj <- build_named_plot()
    out <- fp_txt_where(obj,
        gp = gpar(fontface = "bold"),
        cols = "type",
        where = ~ type %in% c("header", "subtotal")
    )

    expect_true(!is.null(attr(out$labels[[1]][[1]], "txt_gp")$font))
    expect_null(attr(out$labels[[1]][[2]], "txt_gp"))
    expect_true(!is.null(attr(out$labels[[1]][[3]], "txt_gp")$font))
})

test_that("fp_span_where applies span attribute on selected cells", {
    obj <- build_named_plot()
    out <- fp_span_where(obj, columns = c(1, 2), rows = c(1, 3), cols = "type")

    expect_equal(attr(out$labels[[1]][[1]], "span"), c(1L, 2L))
    expect_null(attr(out$labels[[1]][[2]], "span"))
    expect_equal(attr(out$labels[[1]][[3]], "span"), c(1L, 2L))
})

test_that("last-applied wins for overlapping selectors", {
    obj <- build_named_plot()
    out <- obj |>
        fp_align_where(align = "r", cols = "val") |>
        fp_align_where(align = "l", rows = 1, cols = "val")

    expect_equal(attr(out$labels[[2]][[1]], "align"), "l")
    expect_equal(attr(out$labels[[2]][[2]], "align"), "r")
    expect_equal(attr(out$labels[[2]][[3]], "align"), "r")
})

test_that("fp_set_summary supports bare predicate expressions", {
    obj <- build_named_plot() |>
        fp_set_summary(type %in% c("header", "subtotal"))

    expect_equal(obj$is.summary, c(TRUE, FALSE, TRUE))
})

test_that("fp_set_summary can use source columns not displayed in labels", {
    df <- data.frame(
        type = c("header", "study", "subtotal"),
        study = c("Inventors", "Alfred Nobel", "Subtotal"),
        est = c(NA, 1.1, 1.0),
        lb = c(NA, 0.9, 0.8),
        ub = c(NA, 1.3, 1.2),
        stringsAsFactors = FALSE
    )

    obj <- df |>
        forestplot(mean = est, lower = lb, upper = ub, labeltext = study) |>
        fp_extract_labels(Study = study) |>
        fp_set_summary(type %in% c("header", "subtotal"))

    expect_equal(names(obj$labels), "Study")
    expect_equal(obj$is.summary, c(TRUE, FALSE, TRUE))
})

test_that("fp_set_summary supports length-1 logical", {
    obj <- build_named_plot() |>
        fp_set_summary(TRUE)

    expect_equal(obj$is.summary, c(TRUE, TRUE, TRUE))
})

test_that("fp_set_summary errors when predicate length is invalid", {
    obj <- build_named_plot()

    expect_error(
        obj |> fp_set_summary(c(TRUE, FALSE)),
        "must evaluate to length"
    )
})
