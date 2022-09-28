base_data <- tibble::tibble(mean  = c(0.578, 0.165, 0.246, 0.700, 0.348, 0.139, 1.017),
                            lower = c(0.372, 0.018, 0.072, 0.333, 0.083, 0.016, 0.365),
                            upper = c(0.898, 1.517, 0.833, 1.474, 1.455, 1.209, 2.831),
                            study = c("Auckland", "Block", "Doran", "Gamsu",
                                      "Morrison", "Papageorgiou", "Tauesch"),
                            deaths_steroid = c("36", "1", "4", "14", "3", "1", "8"),
                            deaths_placebo = c("60", "5", "11", "20", "7", "7", "10"),
                            OR = c("0.58", "0.16", "0.25", "0.70", "0.35", "0.14", "1.02"))

base_data |>
  forestplot(labeltext = c(study, deaths_steroid, deaths_placebo, OR),
             clip = c(0.1, 2.5),
             xlog = TRUE) |>
  fp_add_header(study = c("", "Study"),
                deaths_steroid = c("Deaths", "(steroid)"),
                deaths_placebo = c("Deaths", "(placebo)"),
                OR = c("", "OR")) |>
  fp_set_style(box = "royalblue",
               line = "darkblue",
               summary = gpar(fill = "royalblue", clr = "black"),
               txt_gp = fpTxtGp(label = gpar(fontfamily = "mono"))) |>
  fp_decorate_graph(box = "lightgray",
                    right_bottom_txt = fp_txt_gp("RB", gp = gpar(cex = .5)),
                    left_bottom_txt = fp_txt_gp("LB", gp = gpar(cex = .5)),
                    right_top_txt = "RT",
                    left_top_txt = "LT")
