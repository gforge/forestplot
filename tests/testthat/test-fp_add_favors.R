library(testthat)

test_that("fp_set_favors stores settings", {
    obj <- forestplot(
        labeltext = c("A", "B"),
        mean = c(0.4, 0.8),
        lower = c(0.1, 0.5),
        upper = c(0.7, 1.1)
    ) |>
        fp_set_favors(low = "Group 1", high = "Group 2")

    expect_equal(obj$graph_favors$position, "outside")
    expect_true(obj$graph_favors$arrows)
    expect_equal(obj$graph_favors$low, "Group 1")
    expect_equal(obj$graph_favors$high, "Group 2")
})

test_that("fp_set_favors validates arguments", {
    obj <- forestplot(
        labeltext = c("A", "B"),
        mean = c(0.4, 0.8),
        lower = c(0.1, 0.5),
        upper = c(0.7, 1.1)
    )

    expect_error(fp_set_favors(obj, arrows = NA), "arrows")
    expect_error(fp_set_favors(obj, txt_gp = 1), "txt_gp")
    expect_error(fp_set_favors(obj, arrow_gp = 1), "arrow_gp")
    expect_error(fp_set_favors(obj, label_x_nudge = "bad"), "label_x_nudge")
    expect_error(fp_set_favors(obj, label_y_nudge = "bad"), "label_y_nudge")
})

test_that("fp_set_favors accepts and stores label nudges", {
    obj <- forestplot(
        labeltext = c("A", "B"),
        mean = c(0.4, 0.8),
        lower = c(0.1, 0.5),
        upper = c(0.7, 1.1)
    ) |>
        fp_set_favors(
            low = "Group 1",
            high = "Group 2",
            label_x_nudge = unit(4, "mm"),
            label_y_nudge = 1.5
        )

    expect_true(is.unit(obj$graph_favors$label_x_nudge))
    expect_true(is.unit(obj$graph_favors$label_y_nudge))
})

test_that("fp_set_favors renders for inside and outside", {
    outside_obj <- forestplot(
        labeltext = c("A", "B"),
        mean = c(0.4, 0.8),
        lower = c(0.1, 0.5),
        upper = c(0.7, 1.1),
        xlab = "Effect"
    ) |>
        fp_set_favors(low = "Treatment", high = "Control", position = "outside")

    inside_obj <- forestplot(
        labeltext = c("A", "B"),
        mean = c(1.4, 2.2),
        lower = c(1.1, 1.5),
        upper = c(2.0, 3.1),
        xlog = TRUE,
        xlab = "HR"
    ) |>
        fp_set_favors(low = "Mutant", high = "Wild type", arrows = FALSE, position = "inside")

    expect_silent(print(outside_obj, new_page = FALSE))
    expect_silent(print(inside_obj, new_page = FALSE))
})
