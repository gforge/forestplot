library(testthat)
library(abind)

test_that("Check different input formats", {
  basic_data <- cbind(0:2, 1:3, 2:4)
  rownames(basic_data) <- LETTERS[1:3]
  expect_silent(
    abind(basic_data,
      basic_data + 1,
      along = 3
    ) |>
      forestplot(labeltext = 1:3)
  )

  expect_silent(
    abind(basic_data,
      basic_data + 1,
      along = 3
    ) |>
      forestplot()
  )

  expect_silent(forestplot(
    cbind(
      0:2,
      1:3,
      2:4
    ),
    labeltext = 1:3
  ))

  expect_silent(forestplot(
    cbind(
      c(NA, 1:2),
      c(NA, 2:3),
      c(NA, 3:4)
    ),
    labeltext = 1:3
  ))

  expect_error(forestplot(
    cbind(
      0:2,
      3:1,
      2:4
    ),
    labeltext = 1:3
  ))

  expect_error(
    abind(basic_data,
      cbind(0:2, 3:1, 2:4),
      along = 3
    ) |>
      forestplot()
  )

  # new validation cases ------------------------------------------------------
  # mean outside CI (deliberately outside bounds)
  df <- data.frame(coef = 1, low = -1, high = 0.5)
  expect_error(
    forestplot(df, labeltext = 1, mean = coef, lower = low, upper = high),
    "Estimate outside confidence interval"
  )

  # lower > upper (negative values included)
  df2 <- data.frame(coef = -2, low = -1, high = -3)
  expect_error(
    forestplot(df2, labeltext = 1, mean = coef, lower = low, upper = high),
    "lower bound exceeds upper bound"
  )

  # valid negative scenario should be silent
  df3 <- data.frame(coef = c(-2, -4), low = c(-2.5, -5), high = c(-1.5, -3))
  expect_silent(
    forestplot(df3, labeltext = 1:2, mean = coef, lower = low, upper = high)
  )
})
