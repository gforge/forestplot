library(forestplot)
options(forestplot_new_page = TRUE)
# Cochrane data from the 'rmeta'-package
cochrane_from_rmeta <-
  structure(list(
    mean  = c(NA, NA, 0.578, 0.165, 0.246, 0.700, 0.348, 0.139, 1.017, NA, 0.531),
    lower = c(NA, NA, 0.372, 0.018, 0.072, 0.333, 0.083, 0.016, 0.365, NA, 0.386),
    upper = c(NA, NA, 0.898, 1.517, 0.833, 1.474, 1.455, 1.209, 2.831, NA, 0.731)
  ),
  .Names = c("mean", "lower", "upper"),
  row.names = c(NA, -11L),
  class = "data.frame"
  )

tabletext <- cbind(
  c(
    "", "Study", "Auckland", "Block",
    "Doran", "Gamsu", "Morrison", "Papageorgiou",
    "Tauesch", NA, "Summary"
  ),
  c(
    "Deaths", "(steroid)", "36", "1",
    "4", "14", "3", "1",
    "8", NA, NA
  ),
  c(
    "Deaths", "(placebo)", "60", "5",
    "11", "20", "7", "7",
    "10", NA, NA
  ),
  c(
    "", "OR", "0.58", "0.16",
    "0.25", "0.70", "0.35", "0.14",
    "1.02", NA, "0.53"
  )
)

# Test summary
forestplot(tabletext,
  cochrane_from_rmeta,
  new_page = TRUE,
  is.summary = c(TRUE, TRUE, rep(FALSE, 8), TRUE),
  clip = c(0.1, 2.5),
  xlog = TRUE,
  col = fpColors(box = "royalblue", line = "darkblue", summary = "royalblue")
)

# Test lines
forestplot(tabletext,
  hrzl_lines = gpar(col = "#444444"),
  cochrane_from_rmeta, new_page = TRUE,
  is.summary = c(TRUE, TRUE, rep(FALSE, 8), TRUE),
  clip = c(0.1, 2.5),
  xlog = TRUE,
  col = fpColors(box = "royalblue", line = "darkblue", summary = "royalblue")
)

# Test line options
forestplot(tabletext,
  hrzl_lines = list(
    "3" = gpar(lty = 2),
    "11" = gpar(lwd = 1, columns = 1:4, col = "#000044")
  ),
  cochrane_from_rmeta, new_page = TRUE,
  is.summary = c(TRUE, TRUE, rep(FALSE, 8), TRUE),
  clip = c(0.1, 2.5),
  xlog = TRUE,
  col = fpColors(box = "royalblue", line = "darkblue", summary = "royalblue", hrz_lines = "#444444")
)

# Test colors
forestplot(tabletext,
  hrzl_lines = list(
    "3" = gpar(lty = 2),
    "11" = gpar(lwd = 1, columns = 1:4, col = "blue")
  ),
  cochrane_from_rmeta, new_page = TRUE,
  is.summary = c(TRUE, TRUE, rep(FALSE, 8), TRUE),
  clip = c(0.1, 2.5),
  xlog = TRUE,
  col = fpColors(box = "red", line = "green", summary = "purple", hrz_lines = "orange"),
  vertices = TRUE
)

# Test positioning of text
forestplot(tabletext,
  graph.pos = 4,
  hrzl_lines = list(
    "3" = gpar(lty = 2),
    "11" = gpar(lwd = 1, columns = c(1:3, 5), col = "#000044"),
    "12" = gpar(lwd = 1, lty = 2, columns = c(1:3, 5), col = "#000044")
  ),
  cochrane_from_rmeta, new_page = TRUE,
  is.summary = c(TRUE, TRUE, rep(FALSE, 8), TRUE),
  clip = c(0.1, 2.5),
  xlog = TRUE,
  col = fpColors(box = "royalblue", line = "darkblue", summary = "royalblue", hrz_lines = "#444444")
)

# Test expression
data(HRQoL)
clrs <- fpColors(box = "royalblue", line = "darkblue", summary = "royalblue")
tabletext <-
  list(
    c(NA, rownames(HRQoL$Sweden)),
    append(list(expression(beta)), sprintf("%.2f", HRQoL$Sweden[, "coef"]))
  )
forestplot(tabletext,
  rbind(
    rep(NA, 3),
    HRQoL$Sweden
  ),
  col = clrs,
  xlab = "EQ-5D index"
)

# Test alternative text
tabletext <- cbind(
  rownames(HRQoL$Sweden),
  sprintf("%.2f", HRQoL$Sweden[, "coef"])
)
forestplot(tabletext,
  txt_gp = fpTxtGp(label = gpar(fontfamily = "HersheyScript")),
  rbind(HRQoL$Sweden),
  col = clrs,
  xlab = "EQ-5D index"
)

# More color tests
forestplot(tabletext,
  txt_gp = fpTxtGp(
    label = list(
      gpar(fontfamily = "HersheyScript"),
      gpar(
        fontfamily = "",
        col = "#660000"
      )
    ),
    ticks = gpar(fontfamily = "", cex = 1),
    xlab = gpar(fontfamily = "HersheySerif", cex = 1.5)
  ),
  rbind(HRQoL$Sweden),
  col = clrs,
  xlab = "EQ-5D index"
)

# Test clipping
forestplot(tabletext,
  rbind(HRQoL$Sweden),
  clip = c(-.1, Inf),
  col = clrs,
  xlab = "EQ-5D index"
)

# test two lines
tabletext <- tabletext[, 1]
forestplot(tabletext,
  mean = cbind(HRQoL$Sweden[, "coef"], HRQoL$Denmark[, "coef"]),
  lower = cbind(HRQoL$Sweden[, "lower"], HRQoL$Denmark[, "lower"]),
  upper = cbind(HRQoL$Sweden[, "upper"], HRQoL$Denmark[, "upper"]),
  clip = c(-.1, 0.075),
  col = fpColors(box = c("blue", "darkred")),
  xlab = "EQ-5D index"
)


## ------------------------------------------------------------------------
forestplot(tabletext,
  fn.ci_norm = c(fpDrawNormalCI, fpDrawCircleCI),
  boxsize = .25, # We set the box size to better visualize the type
  line.margin = .1, # We need to add this to avoid crowding
  mean = cbind(HRQoL$Sweden[, "coef"], HRQoL$Denmark[, "coef"]),
  lower = cbind(HRQoL$Sweden[, "lower"], HRQoL$Denmark[, "lower"]),
  upper = cbind(HRQoL$Sweden[, "upper"], HRQoL$Denmark[, "upper"]),
  clip = c(-.125, 0.075),
  col = fpColors(box = c("blue", "darkred")),
  xlab = "EQ-5D index"
)

## ------------------------------------------------------------------------
forestplot(tabletext,
  fn.ci_norm = c(fpDrawNormalCI, fpDrawCircleCI),
  boxsize = .25, # We set the box size to better visualize the type
  line.margin = .1, # We need to add this to avoid crowding
  mean = cbind(HRQoL$Sweden[, "coef"], HRQoL$Denmark[, "coef"]),
  lower = cbind(HRQoL$Sweden[, "lower"], HRQoL$Denmark[, "lower"]),
  upper = cbind(HRQoL$Sweden[, "upper"], HRQoL$Denmark[, "upper"]),
  clip = c(-.125, 0.075),
  lty.ci = c(1, 2),
  col = fpColors(box = c("blue", "darkred")),
  xlab = "EQ-5D index"
)

## ------------------------------------------------------------------------
forestplot(tabletext,
  legend = c("Sweden", "Denmark"),
  fn.ci_norm = c(fpDrawNormalCI, fpDrawCircleCI),
  boxsize = .25, # We set the box size to better visualize the type
  line.margin = .1, # We need to add this to avoid crowding
  mean = cbind(HRQoL$Sweden[, "coef"], HRQoL$Denmark[, "coef"]),
  lower = cbind(HRQoL$Sweden[, "lower"], HRQoL$Denmark[, "lower"]),
  upper = cbind(HRQoL$Sweden[, "upper"], HRQoL$Denmark[, "upper"]),
  clip = c(-.125, 0.075),
  col = fpColors(box = c("blue", "darkred")),
  xlab = "EQ-5D index"
)

## ------------------------------------------------------------------------
forestplot(tabletext,
  legend_args = fpLegend(
    pos = list(x = .85, y = 0.25),
    gp = gpar(col = "#CCCCCC", fill = "#F9F9F9")
  ),
  legend = c("Sweden", "Denmark"),
  fn.ci_norm = c(fpDrawNormalCI, fpDrawCircleCI),
  boxsize = .25, # We set the box size to better visualize the type
  line.margin = .1, # We need to add this to avoid crowding
  mean = cbind(HRQoL$Sweden[, "coef"], HRQoL$Denmark[, "coef"]),
  lower = cbind(HRQoL$Sweden[, "lower"], HRQoL$Denmark[, "lower"]),
  upper = cbind(HRQoL$Sweden[, "upper"], HRQoL$Denmark[, "upper"]),
  clip = c(-.125, 0.075),
  col = fpColors(box = c("blue", "darkred")),
  xlab = "EQ-5D index"
)

## ------------------------------------------------------------------------
forestplot(tabletext,
  legend = c("Sweden", "Denmark"),
  fn.ci_norm = c(fpDrawNormalCI, fpDrawCircleCI),
  boxsize = .25, # We set the box size to better visualize the type
  line.margin = .1, # We need to add this to avoid crowding
  mean = cbind(HRQoL$Sweden[, "coef"], HRQoL$Denmark[, "coef"]),
  lower = cbind(HRQoL$Sweden[, "lower"], HRQoL$Denmark[, "lower"]),
  upper = cbind(HRQoL$Sweden[, "upper"], HRQoL$Denmark[, "upper"]),
  clip = c(-.125, 0.075),
  col = fpColors(box = c("blue", "darkred")),
  xticks = c(-.1, -0.05, 0, .05),
  xlab = "EQ-5D index"
)

## ------------------------------------------------------------------------
xticks <- seq(from = -.1, to = .05, by = 0.025)
xtlab <- rep(c(TRUE, FALSE), length.out = length(xticks))
attr(xticks, "labels") <- xtlab
forestplot(tabletext,
           legend = c("Sweden", "Denmark"),
           fn.ci_norm = c(fpDrawNormalCI, fpDrawCircleCI),
           boxsize = .25, # We set the box size to better visualize the type
           line.margin = .1, # We need to add this to avoid crowding
           mean = cbind(HRQoL$Sweden[, "coef"], HRQoL$Denmark[, "coef"]),
           lower = cbind(HRQoL$Sweden[, "lower"], HRQoL$Denmark[, "lower"]),
           upper = cbind(HRQoL$Sweden[, "upper"], HRQoL$Denmark[, "upper"]),
           clip = c(-.125, 0.075),
           col = fpColors(box = c("blue", "darkred")),
           xticks = xticks,
           xlab = "EQ-5D index"
)

## ------------------------------------------------------------------------
forestplot(tabletext,
           legend = c("Sweden", "Denmark"),
           fn.ci_norm = c(fpDrawNormalCI, fpDrawCircleCI),
           boxsize = .25, # We set the box size to better visualize the type
           line.margin = .1, # We need to add this to avoid crowding
           mean = cbind(HRQoL$Sweden[, "coef"], HRQoL$Denmark[, "coef"]),
           lower = cbind(HRQoL$Sweden[, "lower"], HRQoL$Denmark[, "lower"]),
           upper = cbind(HRQoL$Sweden[, "upper"], HRQoL$Denmark[, "upper"]),
           clip = c(-.125, 0.075),
           col = fpColors(box = c("blue", "darkred")),
           grid = TRUE,
           xticks = c(-.1, -0.05, 0, .05),
           xlab = "EQ-5D index"
)

## ------------------------------------------------------------------------
forestplot(tabletext,
           legend = c("Sweden", "Denmark"),
           fn.ci_norm = c(fpDrawNormalCI, fpDrawCircleCI),
           boxsize = .25, # We set the box size to better visualize the type
           line.margin = .1, # We need to add this to avoid crowding
           mean = cbind(HRQoL$Sweden[, "coef"], HRQoL$Denmark[, "coef"]),
           lower = cbind(HRQoL$Sweden[, "lower"], HRQoL$Denmark[, "lower"]),
           upper = cbind(HRQoL$Sweden[, "upper"], HRQoL$Denmark[, "upper"]),
           clip = c(-.125, 0.075),
           col = fpColors(box = c("blue", "darkred")),
           grid = structure(c(-.1, -.05, .05),
                            gp = gpar(lty = 2, col = "#CCCCFF")
           ),
           xlab = "EQ-5D index"
)

##----Test group_by with tidy-syntax--------------------------------
HRQoL |>
  sapply(\(x) data.frame(x) |> tibble::rownames_to_column(),
         simplify = FALSE) |>
  dplyr::bind_rows(.id = "Country") |>
  dplyr::group_by(Country) |>
  forestplot(mean = coef,
             lower = lower,
             upper = upper,
             labeltext = rowname,
             legend = c("Sweden", "Denmark"),
             fn.ci_norm = c(fpDrawNormalCI, fpDrawCircleCI),
             boxsize = .25, # We set the box size to better visualize the type
             line.margin = .1, # We need to add this to avoid crowding
             clip = c(-.125, 0.075),
             col = fpColors(box = c("blue", "darkred")),
             xticks = c(-.1, -0.05, 0, .05),
             xlab = "EQ-5D index"
  )

##----How to handle missing rows when group_by have ----------------
##----different names--------------------------------
HRQoL |>
  sapply(\(x) data.frame(x) |> tibble::rownames_to_column(),
         simplify = FALSE) |>
  dplyr::bind_rows(.id = "Country") |>
  dplyr::filter(Country == "Sweden" | rowname != "Males vs Female") |>
  dplyr::group_by(Country) |>
  forestplot(mean = coef,
             lower = lower,
             upper = upper,
             labeltext = rowname,
             legend = c("Sweden", "Denmark"),
             fn.ci_norm = c(fpDrawNormalCI, fpDrawCircleCI),
             boxsize = .25, # We set the box size to better visualize the type
             line.margin = .1, # We need to add this to avoid crowding
             clip = c(-.125, 0.075),
             col = fpColors(box = c("blue", "darkred")),
             xticks = c(-.1, -0.05, 0, .05),
             xlab = "EQ-5D index"
  )

## ---- eval=FALSE, echo=TRUE----------------------------------------------
#  grid_arg <- c(-.1, -.05, .05)
#  attr(grid_arg, "gp") <- gpar(lty = 2, col = "#CCCCFF")
#
#  identical(grid_arg,
#            structure(c(-.1, -.05, .05),
#                      gp = gpar(lty = 2, col = "#CCCCFF")))
#  # Returns TRUE
