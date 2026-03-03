data(dfHRQoL)

sweden <- dfHRQoL[dfHRQoL$group == "Sweden", ]
sweden$est <- sprintf("%.2f", sweden$mean)

sweden |>
    forestplot(
        labeltext = c(labeltext, est),
        mean = mean,
        lower = lower,
        upper = upper,
        clip = c(-.1, Inf),
        xlab = "EQ-5D index"
    ) |>
    fp_set_favors(
        low = "worse",
        high = "better",
        txt_gp = gpar(cex = 0.8)
    ) |>
    fp_set_style(
        box = "royalblue",
        line = "darkblue",
        summary = "royalblue"
    )
