library(testthat)

test_that("No of lines for missing argument", {
  is.summary <- c(TRUE, FALSE, FALSE, TRUE)
  number_of_columns <- 3
  lines <- prepLines(
    lines = list(),
    is.summary = is.summary,
    number_of_columns = number_of_columns,
    number_of_rows = length(is.summary)
  )
  expect_equivalent(
    length(lines$horizontal),
    length(is.summary) + 1
  )
  expect_equivalent(
    length(lines$vertical),
    number_of_columns + 1
  )
  expect_true(all(sapply(lines$horizontal, is.null)))
  expect_true(all(sapply(lines$vertical, is.null)))

  is.summary <- c(TRUE, FALSE)
  lines <- prepLines(
    lines = NULL,
    is.summary = is.summary,
    number_of_columns = number_of_columns,
    number_of_rows = length(is.summary)
  )
  expect_equivalent(
    length(lines$horizontal),
    length(is.summary) + 1
  )
  expect_true(all(sapply(lines$horizontal, is.null)))
  expect_true(all(sapply(lines$vertical, is.null)))

  is.summary <- c(TRUE, FALSE)
  lines <- prepLines(
    lines = list(horizontal = list(NULL, NULL)),
    is.summary = is.summary,
    number_of_columns = number_of_columns,
    number_of_rows = length(is.summary)
  )
  expect_equivalent(
    length(lines$horizontal),
    length(is.summary) + 1
  )
  expect_true(all(sapply(lines$horizontal, is.null)))
})

test_that("Check correct lines for logical arguments", {
  is.summary <- c(TRUE, FALSE, FALSE, TRUE)
  number_of_columns <- 3

  lines <- prepLines(
    lines = list(horizontal = rep(FALSE, times = length(is.summary) + 1)),
    is.summary = is.summary,
    number_of_rows = length(is.summary),
    number_of_columns = number_of_columns,
    col = fpColors()
  )
  expect_equivalent(
    length(lines$horizontal),
    length(is.summary) + 1
  )
  expect_true(all(sapply(lines$horizontal, is.null)))

  lines <- prepLines(
    lines = list(horizontal = TRUE),
    is.summary = is.summary,
    number_of_rows = length(is.summary),
    number_of_columns = number_of_columns,
    col = fpColors(),
    shapes_gp = fpShapesGp()
  )
  expect_equivalent(
    length(lines$horizontal),
    length(is.summary) + 1
  )
  expect_null(lines$horizontal[[1]])
  expect_true(inherits(lines$horizontal[[2]], "gpar"))
  expect_null(lines$horizontal[[3]])
  expect_true(inherits(lines$horizontal[[4]], "gpar"))
  expect_true(inherits(lines$horizontal[[5]], "gpar"))

  is.summary <- c(TRUE, FALSE, FALSE, FALSE)
  lines <- prepLines(
    lines = list(horizontal = TRUE),
    is.summary = is.summary,
    number_of_rows = length(is.summary),
    number_of_columns = number_of_columns,
    col = fpColors(),
    shapes_gp = fpShapesGp()
  )
  expect_equivalent(
    length(lines$horizontal),
    length(is.summary) + 1
  )
  expect_null(lines$horizontal[[1]])
  expect_true(inherits(lines$horizontal[[2]], "gpar"))
  expect_null(lines$horizontal[[3]])
  expect_null(lines$horizontal[[4]])
  expect_null(lines$horizontal[[5]])

  is.summary <- c(FALSE, FALSE, FALSE, FALSE)
  lines <- prepLines(
    lines = list(horizontal = TRUE),
    is.summary = is.summary,
    number_of_rows = length(is.summary),
    number_of_columns = number_of_columns,
    col = fpColors(),
    shapes_gp = fpShapesGp()
  )
  expect_equivalent(
    length(lines$horizontal),
    length(is.summary) + 1
  )
  expect_true(all(sapply(lines$horizontal, is.null)))
})



test_that("Check correct lines for list arguments", {
  is.summary <- c(TRUE, FALSE, FALSE, TRUE)
  number_of_columns = 4

  lines <- prepLines(
    lines = list(horizontal = list("2" = gpar(col = "red"))),
    is.summary = is.summary,
    number_of_rows = length(is.summary),
    number_of_columns = number_of_columns,
    col = fpColors(),
    shapes_gp = fpShapesGp()
  )
  expect_equivalent(
    length(lines$horizontal),
    length(is.summary) + 1
  )
  expect_null(lines$horizontal[[1]])
  expect_true(inherits(lines$horizontal[[2]], "gpar"))
  expect_null(lines$horizontal[[3]])
  expect_null(lines$horizontal[[4]])
  expect_null(lines$horizontal[[5]])


  is.summary <- c(TRUE, FALSE, FALSE, TRUE)
  lines <- list()
  lines[[4]] <- gpar(col = "red")
  lines[[5]] <- gpar(col = "red")

  lines <- prepLines(
    lines = list(horizontal = lines),
    is.summary = is.summary,
    number_of_rows = length(is.summary),
    number_of_columns = number_of_columns,
    col = fpColors(),
    shapes_gp = fpShapesGp()
  )
  expect_equivalent(
    length(lines$horizontal),
    length(is.summary) + 1
  )
  expect_null(lines$horizontal[[1]])
  expect_null(lines$horizontal[[2]])
  expect_null(lines$horizontal[[3]])
  expect_true(inherits(lines$horizontal[[4]], "gpar"))
  expect_true(inherits(lines$horizontal[[5]], "gpar"))
  expect_equivalent(
    lines$horizontal[[4]]$col,
    "red"
  )
  expect_equivalent(
    lines$horizontal[[5]]$col,
    "red"
  )
})
