library(forestplot)

# Cochrane data from the 'rmeta'-package
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

# add extra "summary" line:
cochrane_from_rmeta <- rbind(cochrane_from_rmeta,
                             cochrane_from_rmeta[nrow(cochrane_from_rmeta), ])

tabletext <- rbind(tabletext,
                   c("95% prediction interval",
                     rep(NA, ncol(tabletext)-1)))

# add second set of outcomes:
cochrane_from_rmeta <- cbind(cochrane_from_rmeta,
                             "mean2"= c(NA,NA,exp(rnorm(7)),NA,NA,NA))
cochrane_from_rmeta <- cbind(cochrane_from_rmeta,
                             "lower2"= cochrane_from_rmeta$mean2/2,
                             "upper2"= cochrane_from_rmeta$mean2*2)


fpDrawBarCI <- function (lower_limit, estimate, upper_limit, size, col, y.offset = 0.5, ...)
{
  size <- ifelse(is.unit(size), convertUnit(size, unitTo = "npc", valueOnly = TRUE), size) * 0.9
  grid.polygon(x = unit(c(lower_limit, upper_limit, upper_limit, lower_limit), "native"),
               y = unit(y.offset + 0.5*c(1, 1, -1, -1)* size, "npc"),
               gp = gpar(fill = col, col = col))
}


###############################################
# (1)  forest plot for single set of outcomes:

forestplot(tabletext,
           fn.ci_sum = c(
             as.list(rep("fpDrawSummaryCI",
                         nrow(cochrane_from_rmeta) - 2)),
             fpDrawSummaryCI,
             fpDrawBarCI),
           mean=cochrane_from_rmeta$mean,
           lower=cochrane_from_rmeta$lower,
           upper=cochrane_from_rmeta$upper,
           new_page = TRUE,
           is.summary=c(TRUE,TRUE,rep(FALSE,8),TRUE, TRUE),
           clip=c(0.1,2.5),
           xlog=TRUE,
           col=fpColors(box="royalblue",line="darkblue", summary="royalblue"))

###############################################
# (2)  forest plot for two sets of outcomes:

norm.arg <- list(NULL)
for (i in 1:12)
  norm.arg[[i]] <- list(function(...) {fpDrawPointCI(pch=15,...)}, function(...) {fpDrawPointCI(pch=18,...)})

sum.arg <- c(as.list(rep("fpDrawSummaryCI", nrow(cochrane_from_rmeta) - 2)),
             fpDrawSummaryCI,
             fpDrawBarCI)

forestplot(tabletext,
           mean  = cochrane_from_rmeta[,c("mean","mean2")],
           lower = cochrane_from_rmeta[,c("lower","lower2")],
           upper = cochrane_from_rmeta[,c("upper","upper2")],
           is.summary=c(TRUE,TRUE,rep(FALSE,8),TRUE, TRUE),
           fn.ci_norm = norm.arg,
           fn.ci_sum  = sum.arg,
           col =fpColors(box    =c("black", "grey45"),
                         lines  =c("black","grey45"),
                         summary="grey30"),
           xlog=TRUE,
           boxsize=c(rep(0.25,11),0.125))
