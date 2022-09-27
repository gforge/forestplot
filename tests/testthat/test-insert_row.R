library(testthat)

test_that("Check that header row is added", {
  out <- data.frame(labels = LETTERS[1:4],
                    mean = 1:4,
                    lower = 1:4 - 1,
                    upper = 1:4 + 1) |>
    forestplot(labeltext = labels,
               mean = mean,
               lower = lower,
               upper = upper)

  expect_equivalent(out$labels |> unlist(),
                    LETTERS[1:4])

  expect_equivalent(out$estimates[,,1],
                    cbind(mean = 1:4, lower = 1:4 - 1, upper = 1:4 + 1))

  out_with_header <- out |>
    fp_add_header(expression(beta))
  expect_equivalent(out_with_header$labels[[1]][[1]],
                    expression(beta))

  expect_true(all(sapply(out_with_header$estimates[1,,], is.na)))
})

test_that("Check that row is added", {
  out <- data.frame(labels = LETTERS[1:4],
                    mean = 1:4,
                    lower = 1:4 - 1,
                    upper = 1:4 + 1) |>
    forestplot(labeltext = labels,
               mean = mean,
               lower = lower,
               upper = upper)

  out_with_header <- out |>
    fp_insert_row("Data",
                  mean = matrix(c(3, 1, 4), ncol = 3),
                  position = 2)
  expect_equivalent(out_with_header$labels[[1]][[2]],
                    "Data")

  expect_equivalent(out_with_header$estimates[2,,],
                    matrix(c(3, 1, 4), ncol = 3))

  expect_equivalent(nrow(out_with_header$estimates), 5)
})


test_that("Check that row is appended", {
  out <- data.frame(label_1 = LETTERS[1:4],
                    label_2 = LETTERS[1:4 + 1],
                    label_3 = LETTERS[1:4 + 2],
                    mean = 1:4,
                    lower = 1:4 - 1,
                    upper = 1:4 + 1) |>
    forestplot(labeltext = c(label_1, label_2, label_3),
               mean = mean,
               lower = lower,
               upper = upper)

  out_with_header <- out |>
    fp_append_row(label_1 = "AA",
                  label_3 = "BB",
                  mean = matrix(c(3, 1, 4), ncol = 3))
  expect_equivalent(out_with_header$labels[[1]] |> tail(1),
                    list("AA"))

  expect_equivalent(out_with_header$labels[[2]] |> tail(1),
                    list(NA))

  expect_equivalent(out_with_header$labels[[3]] |> tail(1),
                    list("BB"))
})
