library(testthat)

context("Test prGparMerge")

test_that("Test that elements are correctly overwritten by the second parameter", {
  expect_equivalent(
    prGparMerge(gpar(col = "red"), gpar(col = "black"))$col,
    "black"
  )

  expect_equivalent(
    prGparMerge(gpar(fill = "red"), gpar(col = "black"))$col,
    "black"
  )

  expect_equivalent(
    prGparMerge(gpar(fill = "red"), gpar(col = "black"))$fill,
    "red"
  )

  expect_equivalent(
    prGparMerge(gpar(fill = "red", col = "red"), gpar(col = "black"))$col,
    "black"
  )

  merged <- prGparMerge(gpar(fill = "red", col = "red", fontface = "bold"), gpar(col = "black", fontface = "italic"))
  expect_equivalent(merged[["fontface"]], "italic")
  expect_null(merged[["font"]])
})
