ask <- par(ask = TRUE)

test_data <- data.frame(
  coef1 = c(1, 1.59, 1.3, 1.24),
  coef2 = c(1, 1.7, 1.4, 1.04),
  low1 = c(1, 1.3, 1.1, 0.99),
  low2 = c(1, 1.6, 1.2, 0.7),
  high1 = c(1, 1.94, 1.6, 1.55),
  high2 = c(1, 1.8, 1.55, 1.33)
)

coef <- with(test_data, cbind(coef1, coef2))
low <- with(test_data, cbind(low1, low2))
high <- with(test_data, cbind(high1, high2))
forestplot(list("Category 1", "Category 2", "Category 3", expression(Category >= 4)),
  coef, low, high,
  xlog = TRUE,
  title = "Cool study",
  boxsize = 0.25,
  col = fpColors(
    box = c("royalblue", "gold"),
    line = c("darkblue", "orange"),
    summary = c("darkblue", "red")
  ),
  xlab = "The estimates",
  new_page = TRUE,
  legend = c("Treatment", "Placebo"),
  legend_args = fpLegend(
    pos = list("topright"),
    title = "Group",
    r = unit(.1, "snpc"),
    gp = gpar(col = "#CCCCCC", lwd = 1.5)
  )
)

par(ask = ask)