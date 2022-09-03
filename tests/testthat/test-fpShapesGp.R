library("testthat")
context("fpShapesGp")

test_that("Check fpShapesGp can be used as shapes_gp parameter", {
  expect_silent(
    forestplot(
      labeltext = cbind(Author = c("Smith et al", "Smooth et al", "al et al")),
      mean = cbind(1:3, 1.5:3.5), lower = cbind(0:2, 0.5:2.5), upper = cbind(4:6, 5.5:7.5),
      is.summary = c(FALSE, TRUE, FALSE), grid = TRUE, new_page = TRUE,
      xticks = c(1, 2, 3, 4, 5),
      col = fpColors(
        box = "blue", lines = "pink", summary = "orange",
        zero = "yellow",
        text = "gray",
        axes = "green", hrz_lines = "violet"
      ),
      hrzl_lines = list(gpar(col = "blue", lwd = 2), gpar(col = "black", lwd = 2), gpar(col = "blue", lwd = 2), gpar(col = "black", lwd = 2)),
      shapes_gp = fpShapesGp(
        default = gpar(lineend = "square", linejoin = "mitre", lwd = 3),
        lines = list(
          gpar(lineend = "square", linejoin = "mitre", lwd = 10, col = rgb(0, 0.7, 0), lty = "dotted"),
          gpar(lineend = "square", linejoin = "mitre", lwd = 5, col = rgb(0, 0.9, 0.9), lty = "dotted"),
          gpar(lwd = 8), gpar(lwd = 7),
          gpar(lwd = 6), gpar(lwd = 1)
        ),
        vertices = gpar(lty = "dotted"),
        box = list(gpar(fill = "orange", col = "red"), gpar(fill = "red", col = "orange")),
        summary = list(gpar(fill = "violet", col = "gray", lwd = 10), gpar(fill = "orange", col = "gray", lwd = 10)),
        axes = gpar(col = "yellow", lwd = 10),
        hrz_lines = gpar(col = "red", lwd = 10, lty = "dashed"),
        zero = gpar(col = "violet", lwd = 10, lty = "dashed"),
        grid = list(gpar(col = "blue", lty = "dotted", lwd = 7), gpar(col = "red", lty = "dotted", lwd = 5), gpar(col = "orange", lty = "dotted", lwd = 3), gpar(col = "orange", lty = "dotted", lwd = 2), gpar(col = "orange", lty = "dotted", lwd = 1))
      ),
      fn.ci_sum = fpDrawBarCI,
      fn.ci_norm = fpDrawPointCI,
      vertices = TRUE
    )
  )
})
