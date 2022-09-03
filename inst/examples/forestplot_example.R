#############################################
# Simple examples of how to do a forestplot #
#############################################

ask <- par(ask = TRUE)

# A basic example, create some fake data
row_names <- list(list("test = 1", expression(test >= 2)))
test_data <- data.frame(
  coef = c(1.59, 1.24),
  low = c(1.4, 0.78),
  high = c(1.8, 1.55)
)
test_data |>
  forestplot(labeltext = row_names,
             mean = coef,
             lower = low,
             upper = high,
             zero = 1,
             cex  = 2,
             lineheight = "auto",
             xlab = "Lab axis txt")

# Print two plots side by side using the grid
# package's layout option for viewports
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 2)))
pushViewport(viewport(layout.pos.col = 1))
test_data |>
  forestplot(labeltext = row_names,
             mean = coef,
             lower = low,
             upper = high,
             zero = 1,
             cex  = 2,
             lineheight = "auto",
             xlab = "Lab axis txt",
             new_page = FALSE)
popViewport()
pushViewport(viewport(layout.pos.col = 2))
test_data |>
  forestplot(labeltext = row_names,
             mean = coef,
             lower = low,
             upper = high,
             zero = 1,
             cex  = 2,
             lineheight = "auto",
             xlab = "Lab axis txt",
             new_page = FALSE)
popViewport(2)


# An advanced test
library(dplyr)
library(tidyr)
test_data <- data.frame(id = 1:4,
                        coef1 = c(1, 1.59, 1.3, 1.24),
                        coef2 = c(1, 1.7, 1.4, 1.04),
                        low1 = c(1, 1.3, 1.1, 0.99),
                        low2 = c(1, 1.6, 1.2, 0.7),
                        high1 = c(1, 1.94, 1.6, 1.55),
                        high2 = c(1, 1.8, 1.55, 1.33))

# Convert into dplyr formatted data
out_data <- test_data |>
  pivot_longer(cols = everything() & -id) |>
  mutate(group = gsub("(.+)([12])$", "\\2", name),
         name = gsub("(.+)([12])$", "\\1", name)) |>
  pivot_wider() |>
  group_by(id) |>
  mutate(col1 = lapply(id, \(x) ifelse(x < 4,
                                       paste("Category", id),
                                       expression(Category >= 4))),
         col2 = lapply(1:n(), \(i) substitute(expression(bar(x) == val),
                                              list(val = mean(coef) |> round(2)))),
         col2 = if_else(id == 1,
                        rep("ref", n()) |> as.list(),
                        col2)) |>
  group_by(group)

out_data |>
  forestplot(mean = coef,
             lower = low,
             upper = high,
             labeltext = c(col1, col2),
             title = "Cool study",
             zero = c(0.98, 1.02),
             grid = structure(c(2^-.5, 2^.5),
                              gp = gpar(col = "steelblue", lty = 2)
             ),
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
             ))

# An example of how the exponential works
test_data <- data.frame(coef = c(2.45, 0.43),
                        low = c(1.5, 0.25),
                        high = c(4, 0.75),
                        boxsize = c(0.25, 0.25))
row_names <- cbind(
  c("Name", "Variable A", "Variable B"),
  c("HR", test_data$coef)
)
test_data <- rbind(rep(NA, ncol(test_data)), test_data)

forestplot(
  labeltext = row_names,
  test_data[, c("coef", "low", "high")],
  is.summary = c(TRUE, FALSE, FALSE),
  boxsize = test_data$boxsize,
  zero = 1,
  xlog = TRUE,
  col = fpColors(lines = "red", box = "darkred")
)

# An example using shapes_gp
forestplot(
  labeltext = cbind(Author = c("Smith et al", "Smooth et al", "Al et al")),
  mean = cbind(1:3, 1.5:3.5),
  lower = cbind(0:2, 0.5:2.5),
  upper = cbind(4:6, 5.5:7.5),
  is.summary = c(FALSE, FALSE, TRUE),
  shapes_gp = fpShapesGp(
    default = gpar(lineend = "square", linejoin = "mitre", lwd = 3, col = "pink"),
    box = gpar(fill = "black", col = "red"), # only one parameter
    lines = list( # as many parameters as CI
      gpar(lwd = 10), gpar(lwd = 5),
      gpar(), gpar(),
      gpar(lwd = 2), gpar(lwd = 1)
    ),
    summary = list( # as many parameters as band per label
      gpar(fill = "violet", col = "gray", lwd = 10),
      gpar(fill = "orange", col = "gray", lwd = 10)
    )
  ),
  vertices = TRUE
)

par(ask = ask)
# See vignette for a more detailed description
# vignette("forestplot",  package="forestplot")
