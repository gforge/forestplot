library(testthat)

context("label extraction helper")

test_that("fp_extract_labeltext supports tidyselect and character selectors", {
    df <- data.frame(
        study = c("A", "B"),
        e1 = c(1, 2),
        n1 = c(10, 20),
        mean = c(1.1, 1.2),
        lower = c(0.9, 1.0),
        upper = c(1.3, 1.4)
    )

    out_tidy <- fp_extract_labeltext(df, study, e1)
    out_char <- fp_extract_labeltext(df, cols = c("study", "e1"))

    expect_equal(colnames(out_tidy), c("study", "e1"))
    expect_equal(colnames(out_char), c("study", "e1"))
    expect_equal(out_tidy$study, c("A", "B"))
    expect_equal(out_char$e1, c(1, 2))
})

test_that("fp_extract_labeltext supports names and NA replacement", {
    df <- data.frame(
        study = c("A", NA),
        e1 = c(1, 2),
        mean = c(1.1, 1.2),
        lower = c(0.9, 1.0),
        upper = c(1.3, 1.4),
        stringsAsFactors = FALSE
    )

    out <- fp_extract_labeltext(
        df,
        cols = c("study", "e1"),
        names = c("Study", "Events"),
        na = ""
    )

    expect_equal(colnames(out), c("Study", "Events"))
    expect_equal(out$Study, c("A", ""))
})

test_that("fp_extract_labeltext preserves non-atomic list cells", {
    df <- data.frame(
        mean = c(1.1, 1.2),
        lower = c(0.9, 1.0),
        upper = c(1.3, 1.4)
    )
    df$label <- I(list(grid::textGrob("a"), NA))

    out <- fp_extract_labeltext(df, cols = "label", na = "")

    expect_true(inherits(out$label[[1]], "text"))
    expect_equal(out$label[[2]], "")
})

test_that("fp_extract_labeltext uses grouped alignment logic", {
    safeLoadPackage("dplyr")

    gdf <- data.frame(
        grp = c("G1", "G1", "G2"),
        label = c("L1", "L2", "L1"),
        mean = c(1.1, 1.2, 1.3),
        lower = c(0.9, 1.0, 1.1),
        upper = c(1.3, 1.4, 1.5),
        stringsAsFactors = FALSE
    ) |>
        dplyr::group_by(grp)

    out <- fp_extract_labeltext(gdf, label)

    expect_equal(colnames(out), "label")
    expect_equal(out$label, c("L1", "L2"))
})

test_that("fp_extract_labeltext errors for grouped data without estimate columns", {
    safeLoadPackage("dplyr")

    gdf <- data.frame(
        grp = c("G1", "G2"),
        label = c("L1", "L1"),
        stringsAsFactors = FALSE
    ) |>
        dplyr::group_by(grp)

    expect_error(
        fp_extract_labeltext(gdf, label),
        "Grouped label extraction requires estimate columns"
    )
})

test_that("fp_extract_labels remaps labels in pipe style for data.frame plots", {
    df <- data.frame(
        type = c("header", "study"),
        author = c("G1", "Study 1"),
        ai = c(NA, 2),
        n1i = c(NA, 20),
        ci = c(NA, 1),
        n2i = c(NA, 22),
        est = c(NA, 1.2),
        lb = c(NA, 0.9),
        ub = c(NA, 1.6)
    )

    out <- df |>
        forestplot(mean = est, lower = lb, upper = ub, labeltext = author) |>
        fp_extract_labels(Type = type, Study = author, E1 = ai, N1 = n1i, E2 = ci, N2 = n2i, na = "")

    expect_equal(names(out$labels), c("Type", "Study", "E1", "N1", "E2", "N2"))
    expect_equal(out$labels[[2]][[1]], "G1")
    expect_equal(out$labels[[2]][[2]], "Study 1")
})

test_that("fp_extract_labels supports grouped source data", {
    safeLoadPackage("dplyr")

    gdf <- data.frame(
        grp = c("G1", "G1", "G2"),
        author = c("L1", "L2", "L1"),
        orci = c("1.0 [0.8, 1.2]", "1.1 [0.9, 1.3]", "0.9 [0.7, 1.1]"),
        est = c(1.0, 1.1, 0.9),
        lb = c(0.8, 0.9, 0.7),
        ub = c(1.2, 1.3, 1.1),
        stringsAsFactors = FALSE
    ) |>
        dplyr::group_by(grp)

    out <- gdf |>
        forestplot(mean = est, lower = lb, upper = ub, labeltext = author) |>
        fp_extract_labels(Study = author)

    expect_equal(attr(out$labels, "no_rows"), nrow(out$estimates))
    expect_equal(names(out$labels), c("Study"))
})

test_that("fp_extract_labels errors without stored source data", {
    obj <- forestplot(
        labeltext = c("A", "B"),
        mean = c(1.0, 1.1),
        lower = c(0.8, 0.9),
        upper = c(1.2, 1.3)
    )

    expect_error(
        obj |> fp_extract_labels(Study = labeltext),
        "does not contain source data"
    )
})
