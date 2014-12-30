library(testthat)
context("prFpGetLines")

test_that("No of lines for missing argument", {
  is.summary <- c(TRUE, FALSE, FALSE, TRUE)
  lines <- prFpGetLines(is.summary = is.summary,
                        nc = 3)
  expect_equivalent(length(lines),
                    length(is.summary) + 1)
  expect_true(all(sapply(lines, is.null)))

  is.summary <- c(TRUE, FALSE)
  lines <- prFpGetLines(is.summary = is.summary,
                        nc = 3)
  expect_equivalent(length(lines),
                    length(is.summary) + 1)
  expect_true(all(sapply(lines, is.null)))

  is.summary <- c(TRUE, FALSE)
  lines <- prFpGetLines(lines = list(NULL, NULL),
                        is.summary = is.summary,
                        nc = 3)
  expect_equivalent(length(lines),
                    length(is.summary) + 1)
  expect_true(all(sapply(lines, is.null)))

})

test_that("Check correct lines for logical arguments",{
  is.summary <- c(TRUE, FALSE, FALSE, TRUE)
  lines <- prFpGetLines(lines = rep(FALSE, times = length(is.summary) + 1),
                        is.summary = is.summary,
                        nc = 3)
  expect_equivalent(length(lines),
                    length(is.summary) + 1)
  expect_true(all(sapply(lines, is.null)))

  lines <- prFpGetLines(lines = FALSE,
                        is.summary = is.summary,
                        nc = 3)
  expect_equivalent(length(lines),
                    length(is.summary) + 1)
  expect_true(all(sapply(lines, is.null)))


  lines <- prFpGetLines(lines = TRUE,
                        is.summary = is.summary,
                        nc = 3)
  expect_equivalent(length(lines),
                    length(is.summary) + 1)
  expect_null(lines[[1]])
  expect_true(inherits(lines[[2]], "gpar"))
  expect_null(lines[[3]])
  expect_true(inherits(lines[[4]], "gpar"))
  expect_null(lines[[5]])

  is.summary <- c(TRUE, FALSE, FALSE, FALSE)
  lines <- prFpGetLines(lines = TRUE,
                        is.summary = is.summary,
                        nc = 3)
  expect_equivalent(length(lines),
                    length(is.summary) + 1)
  expect_null(lines[[1]])
  expect_true(inherits(lines[[2]], "gpar"))
  expect_null(lines[[3]])
  expect_null(lines[[4]])
  expect_null(lines[[5]])

  is.summary <- c(FALSE, FALSE, FALSE, FALSE)
  lines <- prFpGetLines(lines = TRUE,
                        is.summary = is.summary,
                        nc = 3)
  expect_equivalent(length(lines),
                    length(is.summary) + 1)
  expect_true(all(sapply(lines, is.null)))
})



test_that("Check correct lines for list arguments",{
  is.summary <- c(TRUE, FALSE, FALSE, TRUE)
  lines <- prFpGetLines(lines = list("2" = gpar(col="red")),
                        is.summary = is.summary,
                        nc = 3)
  expect_equivalent(length(lines),
                    length(is.summary) + 1)
  expect_null(lines[[1]])
  expect_true(inherits(lines[[2]], "gpar"))
  expect_null(lines[[3]])
  expect_null(lines[[4]])
  expect_null(lines[[5]])


  is.summary <- c(TRUE, FALSE, FALSE, TRUE)
  lines <- list()
  lines[[4]] <- gpar(col = "red")
  lines[[5]] <- gpar(col = "red")

  lines <- prFpGetLines(lines = lines,
                        is.summary = is.summary,
                        nc = 3)
  expect_equivalent(length(lines),
                    length(is.summary) + 1)
  expect_null(lines[[1]])
  expect_true(inherits(lines[[2]], "gpar"))
  expect_null(lines[[3]])
  expect_null(lines[[4]])
  expect_null(lines[[5]])
}
