ask <- par(ask = TRUE)

# An example of how the exponential works
test_data <- data.frame(
  coef = c(2.45, 0.43),
  low = c(1.5, 0.25),
  high = c(4, 0.75),
  boxsize = c(0.5, 0.5)
)
row_names <- cbind(
  c("Name", "Variable A", "Variable B"),
  c("HR", test_data$coef)
)
test_data <- rbind(rep(NA, 3), test_data)

forestplot(
  labeltext = row_names,
  test_data[, c("coef", "low", "high")],
  is.summary = c(TRUE, FALSE, FALSE),
  boxsize = test_data$boxsize,
  zero = 1,
  xlog = TRUE,
  col = fpColors(lines = "#990000", box = "#660000", zero = "darkblue"),
  new_page = TRUE
)

par(ask = ask)