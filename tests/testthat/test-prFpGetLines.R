library(testthat)
context("prFpGetLines")

test_that("No of hrzl_lines for missing argument", {
  is.summary <- c(TRUE, FALSE, FALSE, TRUE)
  hrzl_lines <- prFpGetLines(
    is.summary = is.summary,
    total_columns = 3
  )
  expect_equivalent(
    length(hrzl_lines),
    length(is.summary) + 1
  )
  expect_true(all(sapply(hrzl_lines, is.null)))

  is.summary <- c(TRUE, FALSE)
  hrzl_lines <- prFpGetLines(
    is.summary = is.summary,
    total_columns = 3
  )
  expect_equivalent(
    length(hrzl_lines),
    length(is.summary) + 1
  )
  expect_true(all(sapply(hrzl_lines, is.null)))

  is.summary <- c(TRUE, FALSE)
  hrzl_lines <- prFpGetLines(
    hrzl_lines = list(NULL, NULL),
    is.summary = is.summary,
    total_columns = 3
  )
  expect_equivalent(
    length(hrzl_lines),
    length(is.summary) + 1
  )
  expect_true(all(sapply(hrzl_lines, is.null)))
})

test_that("Check correct hrzl_lines for logical arguments", {
  is.summary <- c(TRUE, FALSE, FALSE, TRUE)
  hrzl_lines <- prFpGetLines(
    hrzl_lines = rep(FALSE, times = length(is.summary) + 1),
    is.summary = is.summary,
    total_columns = 3,
    col = fpColors()
  )
  expect_equivalent(
    length(hrzl_lines),
    length(is.summary) + 1
  )
  expect_true(all(sapply(hrzl_lines, is.null)))

  hrzl_lines <- prFpGetLines(
    hrzl_lines = FALSE,
    is.summary = is.summary,
    total_columns = 3,
    col = fpColors()
  )
  expect_equivalent(
    length(hrzl_lines),
    length(is.summary) + 1
  )
  expect_true(all(sapply(hrzl_lines, is.null)))


  hrzl_lines <- prFpGetLines(
    hrzl_lines = TRUE,
    is.summary = is.summary,
    total_columns = 3,
    col = fpColors()
  )
  expect_equivalent(
    length(hrzl_lines),
    length(is.summary) + 1
  )
  expect_null(hrzl_lines[[1]])
  expect_true(inherits(hrzl_lines[[2]], "gpar"))
  expect_null(hrzl_lines[[3]])
  expect_true(inherits(hrzl_lines[[4]], "gpar"))
  expect_true(inherits(hrzl_lines[[5]], "gpar"))

  is.summary <- c(TRUE, FALSE, FALSE, FALSE)
  hrzl_lines <- prFpGetLines(
    hrzl_lines = TRUE,
    is.summary = is.summary,
    total_columns = 3,
    col = fpColors()
  )
  expect_equivalent(
    length(hrzl_lines),
    length(is.summary) + 1
  )
  expect_null(hrzl_lines[[1]])
  expect_true(inherits(hrzl_lines[[2]], "gpar"))
  expect_null(hrzl_lines[[3]])
  expect_null(hrzl_lines[[4]])
  expect_null(hrzl_lines[[5]])

  is.summary <- c(FALSE, FALSE, FALSE, FALSE)
  hrzl_lines <- prFpGetLines(
    hrzl_lines = TRUE,
    is.summary = is.summary,
    total_columns = 3,
    col = fpColors()
  )
  expect_equivalent(
    length(hrzl_lines),
    length(is.summary) + 1
  )
  expect_true(all(sapply(hrzl_lines, is.null)))
})



test_that("Check correct hrzl_lines for list arguments", {
  is.summary <- c(TRUE, FALSE, FALSE, TRUE)
  hrzl_lines <- prFpGetLines(
    hrzl_lines = list("2" = gpar(col = "red")),
    is.summary = is.summary,
    total_columns = 3,
    col = fpColors()
  )
  expect_equivalent(
    length(hrzl_lines),
    length(is.summary) + 1
  )
  expect_null(hrzl_lines[[1]])
  expect_true(inherits(hrzl_lines[[2]], "gpar"))
  expect_null(hrzl_lines[[3]])
  expect_null(hrzl_lines[[4]])
  expect_null(hrzl_lines[[5]])


  is.summary <- c(TRUE, FALSE, FALSE, TRUE)
  hrzl_lines <- list()
  hrzl_lines[[4]] <- gpar(col = "red")
  hrzl_lines[[5]] <- gpar(col = "red")

  hrzl_lines <- prFpGetLines(
    hrzl_lines = hrzl_lines,
    is.summary = is.summary,
    total_columns = 3,
    col = fpColors()
  )
  expect_equivalent(
    length(hrzl_lines),
    length(is.summary) + 1
  )
  expect_null(hrzl_lines[[1]])
  expect_null(hrzl_lines[[2]])
  expect_null(hrzl_lines[[3]])
  expect_true(inherits(hrzl_lines[[4]], "gpar"))
  expect_true(inherits(hrzl_lines[[5]], "gpar"))
  expect_equivalent(
    hrzl_lines[[4]]$col,
    "red"
  )
  expect_equivalent(
    hrzl_lines[[5]]$col,
    "red"
  )
})
