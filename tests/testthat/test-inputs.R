library(testthat)
library(abind)
context("Tests for foresplot inputs")

test_that("Check different input formats",{
  basic_data <- cbind(0:2,1:3,2:4)
  rownames(basic_data) <- LETTERS[1:3]
  expect_silent(
    abind(basic_data,
          basic_data + 1,
          along = 3) %>%
    forestplot(labeltext = 1:3))

  expect_silent(
    abind(basic_data,
          basic_data + 1,
          along = 3) %>%
      forestplot())

  expect_silent(forestplot(
    cbind(
      0:2,
      1:3,
      2:4
    ),
    labeltext = 1:3))

  expect_error(forestplot(
      cbind(
        0:2,
        3:1,
        2:4
      ),
      labeltext = 1:3))

  expect_error(
    abind(basic_data,
          cbind(0:2,3:1,2:4),
          along = 3) %>%
      forestplot())
})
