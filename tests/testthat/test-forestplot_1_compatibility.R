library(testthat)

test_that("Feeding a data.frame",{
  df <- data.frame(est = 1:3,
                   lb = 0:2,
                   ub = 2:4,
                   labels = LETTERS[1:3])

  obj <- forestplot(df %>% dplyr::select("labels"),
                    mean = df$est,
                    lower = df$lb,
                    upper = df$ub)
  expect_class(obj, "gforge_forestplot")
  expect_equal(obj$labels %>% length(), 1)
  expect_equal(obj$labels[[1]] %>% length(), 3)
})
