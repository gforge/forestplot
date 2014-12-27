## ----, echo=FALSE--------------------------------------------------------
knitr::opts_chunk$set(fig.width = 7, fig.height=3, dev='svg')

## ----, fig.height=4, fig.width=8, message=FALSE--------------------------
library(forestplot)
# Cochrane data from the rmeta-package
cochrane_from_rmeta <- 
  structure(list(
    mean  = c(NA, NA, 0.578, 0.165, 0.246, 0.700, 0.348, 0.139, 1.017, NA, 0.531), 
    lower = c(NA, NA, 0.372, 0.018, 0.072, 0.333, 0.083, 0.016, 0.365, NA, 0.386),
    upper = c(NA, NA, 0.898, 1.517, 0.833, 1.474, 1.455, 1.209, 2.831, NA, 0.731)),
    .Names = c("mean", "lower", "upper"), 
    row.names = c(NA, -11L), 
    class = "data.frame")

tabletext<-cbind(
  c("", "Study", "Auckland", "Block", 
    "Doran", "Gamsu", "Morrison", "Papageorgiou", 
    "Tauesch", NA, "Summary"),
  c("Deaths", "(steroid)", "36", "1", 
    "4", "14", "3", "1", 
    "8", NA, NA),
  c("Deaths", "(placebo)", "60", "5", 
    "11", "20", "7", "7", 
    "10", NA, NA),
  c("", "OR", "0.58", "0.16", 
    "0.25", "0.70", "0.35", "0.14", 
    "1.02", NA, "0.53"))

forestplot(tabletext, 
           cochrane_from_rmeta,new_page = TRUE,
           is.summary=c(TRUE,TRUE,rep(FALSE,8),TRUE),
           clip=c(0.1,2.5), 
           xlog=TRUE,
           col=fpColors(box="royalblue",line="darkblue", summary="royalblue"))

## ------------------------------------------------------------------------
data(HRQoL)
clrs <- fpColors(box="royalblue",line="darkblue", summary="royalblue")
tabletext <- 
  list(c(NA, rownames(HRQoL$Sweden)),
       append(list(expression(beta)), sprintf("%.2f", HRQoL$Sweden[,"coef"])))
forestplot(tabletext, new_page = TRUE,
           rbind(rep(NA, 3), 
                 HRQoL$Sweden),
           col=clrs,
           xlab="EQ-5D index")

## ------------------------------------------------------------------------
tabletext <- cbind(rownames(HRQoL$Sweden),
                   sprintf("%.2f", HRQoL$Sweden[,"coef"]))
forestplot(tabletext, new_page = TRUE, 
           txt_gp = fpTxtGp(label = gpar(fontfamily = "HersheyScript")),
           rbind(HRQoL$Sweden),
           col=clrs,
           xlab="EQ-5D index")

## ------------------------------------------------------------------------
forestplot(tabletext, new_page = TRUE, 
           txt_gp = fpTxtGp(label = list(gpar(fontfamily = "HersheyScript"),
                                         gpar(fontfamily = "",
                                              col = "#660000")),
                            ticks = gpar(fontfamily = "", cex=1),
                            xlab  = gpar(fontfamily = "HersheySerif", cex = 1.5)),
           rbind(HRQoL$Sweden),
           col=clrs,
           xlab="EQ-5D index")

## ------------------------------------------------------------------------
forestplot(tabletext, new_page = TRUE, 
           rbind(HRQoL$Sweden),
           clip =c(-.1, Inf),
           col=clrs,
           xlab="EQ-5D index")

## ------------------------------------------------------------------------
tabletext <- tabletext[,1]
forestplot(tabletext, new_page = TRUE, 
            mean = cbind(HRQoL$Sweden[, "coef"], HRQoL$Denmark[, "coef"]),
            lower = cbind(HRQoL$Sweden[, "lower"], HRQoL$Denmark[, "lower"]),
            upper = cbind(HRQoL$Sweden[, "upper"], HRQoL$Denmark[, "upper"]),
            clip =c(-.1, 0.075),
            col=fpColors(box=c("blue", "darkred")),
            xlab="EQ-5D index")


## ------------------------------------------------------------------------
forestplot(tabletext, new_page = TRUE, 
            fn.ci_norm = c(fpDrawNormalCI, fpDrawCircleCI),
            boxsize = .25, # We set the box size to better visualize the type
            line.margin = .1, # We need to add this to avoid crowding
            mean = cbind(HRQoL$Sweden[, "coef"], HRQoL$Denmark[, "coef"]),
            lower = cbind(HRQoL$Sweden[, "lower"], HRQoL$Denmark[, "lower"]),
            upper = cbind(HRQoL$Sweden[, "upper"], HRQoL$Denmark[, "upper"]),
            clip =c(-.125, 0.075),
            col=fpColors(box=c("blue", "darkred")),
            xlab="EQ-5D index")

## ------------------------------------------------------------------------
forestplot(tabletext, new_page = TRUE, 
            legend = c("Sweden", "Denmark"),
            fn.ci_norm = c(fpDrawNormalCI, fpDrawCircleCI),
            boxsize = .25, # We set the box size to better visualize the type
            line.margin = .1, # We need to add this to avoid crowding
            mean = cbind(HRQoL$Sweden[, "coef"], HRQoL$Denmark[, "coef"]),
            lower = cbind(HRQoL$Sweden[, "lower"], HRQoL$Denmark[, "lower"]),
            upper = cbind(HRQoL$Sweden[, "upper"], HRQoL$Denmark[, "upper"]),
            clip =c(-.125, 0.075),
            col=fpColors(box=c("blue", "darkred")),
            xlab="EQ-5D index")

## ------------------------------------------------------------------------
forestplot(tabletext, new_page = TRUE, 
            legend_args = fpLegend(pos = list(x=.85, y=0.25), 
                                   gp=gpar(col="#CCCCCC", fill="#F9F9F9")),
            legend = c("Sweden", "Denmark"),
            fn.ci_norm = c(fpDrawNormalCI, fpDrawCircleCI),
            boxsize = .25, # We set the box size to better visualize the type
            line.margin = .1, # We need to add this to avoid crowding
            mean = cbind(HRQoL$Sweden[, "coef"], HRQoL$Denmark[, "coef"]),
            lower = cbind(HRQoL$Sweden[, "lower"], HRQoL$Denmark[, "lower"]),
            upper = cbind(HRQoL$Sweden[, "upper"], HRQoL$Denmark[, "upper"]),
            clip =c(-.125, 0.075),
            col=fpColors(box=c("blue", "darkred")),
            xlab="EQ-5D index")

