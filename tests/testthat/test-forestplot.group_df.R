library("testthat")

data("HRQoL")

test_that("Basic", {
  out <- HRQoL |>
    sapply(\(x) data.frame(x) |> tibble::rownames_to_column(),
           simplify = FALSE) |>
    dplyr::bind_rows(.id = "Country") |>
    dplyr::group_by(Country) |>
    forestplot(mean = coef,
               lower = lower,
               upper = upper,
               labeltext = rowname,
               fn.ci_norm = c(fpDrawNormalCI, fpDrawCircleCI),
               boxsize = .25, # We set the box size to better visualize the type
               line.margin = .1, # We need to add this to avoid crowding
               clip = c(-.125, 0.075),
               col = fpColors(box = c("blue", "darkred")),
               xticks = c(-.1, -0.05, 0, .05),
               xlab = "EQ-5D index"
    )

  expect_equivalent(out$estimates[,1,],
                    lapply(HRQoL, \(x) x[,"coef"]) |> do.call(cbind, args = _))
})


test_that("How to handle missing rows when group_by have different names", {
  out <- HRQoL |>
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
  expect_equivalent(out$estimates[,1,1],
                    HRQoL[[1]][,"coef"])
  expect_scalar_na(out$estimates[1,1,2])
  expect_equivalent(out$estimates[2:4,1,2],
                    HRQoL[[2]][2:4,"coef"])
})
