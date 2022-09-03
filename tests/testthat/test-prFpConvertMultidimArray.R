library(testthat)

context("Test prFpConvertMultidimArray")

test_that("Basic identification of mean, lower, upper", {
  out <- prFpConvertMultidimArray(matrix(
    data = 1:3,
    ncol = 3
  ))
  expect_equal(out, list(mean = 2, lower = 1, upper = 3))

  out <- prFpConvertMultidimArray(matrix(
    data = 3:1,
    ncol = 3
  ))
  expect_equal(out, list(mean = 2, lower = 1, upper = 3))
})

test_that("Basic multirow identification of mean, lower, upper", {
  out <- prFpConvertMultidimArray(matrix(
    data = 1:6,
    ncol = 3
  ))
  expect_set_equal(out$lower, 1:2)
  expect_set_equal(out$mean, 3:4)
  expect_set_equal(out$upper, 5:6)

  out <- prFpConvertMultidimArray(matrix(
    data = 6:1,
    ncol = 3
  ))
  expect_set_equal(out$lower, 1:2)
  expect_set_equal(out$mean, 3:4)
  expect_set_equal(out$upper, 5:6)
})

test_that("Check identification of mean, lower, upper with NA", {
  data <- matrix(data = 1:6, ncol = 3)
  data <- rbind(
    rep(NA, 3),
    data,
    rep(NA, 3)
  )
  out <- prFpConvertMultidimArray(data)
  expect_set_equal(out$lower, c(NA, 1:2))
  expect_set_equal(out$mean, c(NA, 3:4))
  expect_set_equal(out$upper, c(NA, 5:6))
})

test_that("Check failures", {
  data <- matrix(data = rep(1, 6), ncol = 3)
  expect_error(prFpConvertMultidimArray(data))

  data <- matrix(data = rep(1, 6), ncol = 3)
  data[, 2] <- rep(2, 2)
  expect_error(prFpConvertMultidimArray(data))
})
