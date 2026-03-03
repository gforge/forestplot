library(testthat)

test_that("Check that header row is added", {
  out <- data.frame(
    labels = LETTERS[1:4],
    mean = 1:4,
    lower = 1:4 - 1,
    upper = 1:4 + 1
  ) |>
    forestplot(
      labeltext = labels,
      mean = mean,
      lower = lower,
      upper = upper
    )

  expect_equivalent(
    out$labels |> unlist(),
    LETTERS[1:4]
  )

  expect_equivalent(
    out$estimates[, , 1],
    cbind(mean = 1:4, lower = 1:4 - 1, upper = 1:4 + 1)
  )

  out_with_header <- out |>
    fp_add_header(expression(beta))
  expect_equivalent(
    out_with_header$labels[[1]][[1]],
    expression(beta)
  )

  expect_true(all(sapply(out_with_header$estimates[1, , ], is.na)))
})

test_that("Check that row is added", {
  out <- data.frame(
    labels = LETTERS[1:4],
    mean = 1:4,
    lower = 1:4 - 1,
    upper = 1:4 + 1
  ) |>
    forestplot(
      labeltext = labels,
      mean = mean,
      lower = lower,
      upper = upper
    )

  out_with_header <- out |>
    fp_insert_row("Data",
      mean = matrix(c(3, 1, 4), ncol = 3),
      position = 2
    )
  expect_equivalent(
    out_with_header$labels[[1]][[2]],
    "Data"
  )

  expect_equivalent(
    out_with_header$estimates[2, , ],
    matrix(c(3, 1, 4), ncol = 3)
  )

  expect_equivalent(nrow(out_with_header$estimates), 5)
})


test_that("fp_add_header accepts spanned text", {
  out <- data.frame(
    labels = LETTERS[1:3],
    mean = 1:3,
    lower = 1:3 - 1,
    upper = 1:3 + 1
  ) |>
    forestplot(
      labeltext = labels,
      mean = mean,
      lower = lower,
      upper = upper
    )

  out2 <- out |> fp_add_header(fp_span("Events / N", columns = c(1, 2)))
  # attribute should survive in labels
  expect_equal(attr(out2$labels[[1]][[1]], "span"), c(1L, 2L))
})

test_that("fp_add_header accepts sparse named spanned headers", {
  df <- data.frame(
    Study = c("S1", "S2"),
    E1 = c("1", "2"),
    N1 = c("10", "20"),
    E2 = c("3", "4"),
    N2 = c("11", "21"),
    mean = c(0.8, 1.1),
    lower = c(0.6, 0.9),
    upper = c(1.1, 1.4)
  )

  out <- df |>
    forestplot(
      labeltext = c(Study, E1, N1, E2, N2),
      mean = mean,
      lower = lower,
      upper = upper
    ) |>
    fp_add_header(
      E1 = fp_span("Caffeine", columns = c(2, 3)) |> fp_align_center(),
      E2 = fp_span("Decaf", columns = c(4, 5)) |> fp_align_center()
    )

  expect_equal(as.character(out$labels$E1[[1]]), "Caffeine")
  expect_equal(as.character(out$labels$E2[[1]]), "Decaf")
  expect_equal(attr(out$labels$E1[[1]], "span"), c(2L, 3L))
  expect_equal(attr(out$labels$E2[[1]], "span"), c(4L, 5L))
  expect_true(is.na(out$labels$Study[[1]]))
})

test_that("fp_add_header stops when spanned headers overlap", {
  df <- data.frame(
    E1 = c("1", "2"),
    N1 = c("10", "20"),
    E2 = c("3", "4"),
    mean = c(0.8, 1.1),
    lower = c(0.6, 0.9),
    upper = c(1.1, 1.4)
  )

  obj <- df |>
    forestplot(
      labeltext = c(E1, N1, E2),
      mean = mean,
      lower = lower,
      upper = upper
    )

  expect_error(
    obj |> fp_add_header(
      E1 = fp_span("Left", columns = c(1, 2)),
      E2 = fp_span("Right", columns = c(2, 3))
    ),
    "too close/overlapping; max allowed end"
  )
})

test_that("fp_add_header supports unnamed spans mixed with named cells", {
  df <- data.frame(
    A = c("a1", "a2"),
    B = c("b1", "b2"),
    C = c("c1", "c2"),
    D = c("d1", "d2"),
    E = c("e1", "e2"),
    mean = c(0.8, 1.1),
    lower = c(0.6, 0.9),
    upper = c(1.1, 1.4)
  )

  out <- df |>
    forestplot(
      labeltext = c(A, B, C, D, E),
      mean = mean,
      lower = lower,
      upper = upper
    ) |>
    fp_add_header(
      fp_span("Caffeine", columns = c(2, 3)) |> fp_align_center(),
      fp_span("Decaf", columns = c(4, 5)) |> fp_align_center(),
      A = "AAAA"
    )

  expect_equal(as.character(out$labels$B[[1]]), "Caffeine")
  expect_equal(as.character(out$labels$D[[1]]), "Decaf")
  expect_equal(as.character(out$labels$A[[1]]), "AAAA")
  expect_equal(attr(out$labels$B[[1]], "span"), c(2L, 3L))
  expect_equal(attr(out$labels$D[[1]], "span"), c(4L, 5L))
})

test_that("fp_add_header catches collisions between unnamed span and named cell", {
  df <- data.frame(
    A = c("a1", "a2"),
    B = c("b1", "b2"),
    C = c("c1", "c2"),
    mean = c(0.8, 1.1),
    lower = c(0.6, 0.9),
    upper = c(1.1, 1.4)
  )

  obj <- df |>
    forestplot(
      labeltext = c(A, B, C),
      mean = mean,
      lower = lower,
      upper = upper
    )

  expect_error(
    obj |> fp_add_header(
      fp_span("Span BC", columns = c(2, 3)) |> fp_align_center(),
      C = "Collision"
    ),
    "too close/overlapping; max allowed end"
  )
})


test_that("Check that row is appended", {
  out <- data.frame(
    label_1 = LETTERS[1:4],
    label_2 = LETTERS[1:4 + 1],
    label_3 = LETTERS[1:4 + 2],
    mean = 1:4,
    lower = 1:4 - 1,
    upper = 1:4 + 1
  ) |>
    forestplot(
      labeltext = c(label_1, label_2, label_3),
      mean = mean,
      lower = lower,
      upper = upper
    )

  out_with_header <- out |>
    fp_append_row(
      label_1 = "AA",
      label_3 = "BB",
      mean = matrix(c(3, 1, 4), ncol = 3)
    )
  expect_equivalent(
    out_with_header$labels[[1]] |> tail(1),
    list("AA")
  )

  expect_equivalent(
    out_with_header$labels[[2]] |> tail(1),
    list(NA)
  )

  expect_equivalent(
    out_with_header$labels[[3]] |> tail(1),
    list("BB")
  )
})
