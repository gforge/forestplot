library(testthat)

context("Tests for fpTxtGp")

test_that("Check multilabel structure", {
  expect_equal(
    fpTxtGp(label = gpar(cex = 1.2))$label$cex,
    1.2
  )

  tmp <- fpTxtGp(label = list(
    gpar(cex = 1.2),
    gpar(cex = 1.4)
  ))
  expect_equal(
    length(tmp$label),
    2
  )

  expect_equal(
    tmp$label[[1]]$cex,
    1.2
  )
  expect_equal(
    tmp$legend$cex,
    tmp$label[[1]]$cex * 0.8
  )

  expect_equal(
    attr(tmp$label, "txt_dim"),
    1
  )
  expect_equal(
    attr(tmp$summary, "txt_dim"),
    0
  )

  tmp <- fpTxtGp(
    label = gpar(cex = 1.2),
    summary = gpar(cex = .6)
  )
  expect_equal(
    attr(tmp$label, "txt_dim"),
    0
  )
  expect_equal(
    attr(tmp$summary, "txt_dim"),
    0
  )
  expect_equal(
    tmp$label$cex,
    1.2
  )
  expect_equal(
    tmp$summary$cex,
    .6
  )
})
