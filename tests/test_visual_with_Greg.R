library(Greg)
library(tidyverse)
# simulated data to test
set.seed(102)
cov <- tibble(ftime = rexp(200)) |>
  mutate(x1 = runif(n()),
         x2 = runif(n()),
         x3 = runif(n()),
         fstatus1 = if_else(x1 * 1 +
                              x2 * 0.2 +
                              x3 * 0.5 +
                              runif(n()) * 0.5 > 1,
                            1, 0),
         fstatus2 = if_else(x1 * 0.2 +
                              x2 * 0.5 +
                              x3 * 0.1 +
                              runif(n()) * 2 > 1,
                            1, 0)) |>
  # Add some column labels
  Gmisc::set_column_labels(x1 = "First variable",
                           x2 = "Second variable")

library(rms)
dd <- datadist(cov)
options(datadist = "dd")

fit1 <- cph(Surv(ftime, fstatus1 == 1) ~ x1 + x2 + x3, data = cov)
fit2 <- update(fit1, Surv(ftime, fstatus2 == 1) ~ .)
list("Frist model" = fit1, "Second model"  = fit2) |>
  forestplotRegrObj(legend_args = fpLegend(title = "Type of regression"),
                    postprocess_estimates.fn = \(x) filter(x, str_detect(column_term, "(x2|x3)")),
                    col = fpColors(box = c("darkblue", "darkred"))) |>
  forestplot() |>
  fp_set_zebra_style("red")
