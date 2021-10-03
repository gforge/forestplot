#' @rdname forestplot
#' @method forestplot data.frame
#' @param x The data frame with or without grouping
#' @export
forestplot.data.frame <- function(x, mean, lower, upper, labeltext, is.summary, ...) {
  safeLoadPackage("dplyr")
  safeLoadPackage("tidyr")
  safeLoadPackage("rlang")

  estimates <- list(
    mean = assertAndRetrieveTidyValue(x, mean),
    lower = assertAndRetrieveTidyValue(x, lower),
    upper = assertAndRetrieveTidyValue(x, upper)
  )

  if (!any(sapply(estimates, attr, "tidyFormat"))) {
    labeltext <- x
  } else {
    labeltext <- assertAndRetrieveTidyValue(x, labeltext)
  }

  if (!missing(is.summary)) {
    sumid <- substitute(is.summary)
    is.summary <- tryCatch(x %>% dplyr::pull({{ sumid }}) %>% sapply(function(x) ifelse(is.na(x), FALSE, x)),
      error = function(e) is.summary
    )
  } else {
    is.summary <- FALSE
  }

  forestplot.default(labeltext = labeltext, mean = estimates$mean, lower = estimates$lower, upper = estimates$upper, is.summary = is.summary, ...)
}
