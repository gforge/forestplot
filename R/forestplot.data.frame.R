#' @rdname forestplot
#' @method forestplot data.frame
#' @importFrom rlang as_name
#' @param x The data frame with or without grouping
#' @export
forestplot.data.frame <- function(x, mean, lower, upper, labeltext, ...) {
  safeLoadPackage("dplyr")
  safeLoadPackage("rlang")
  if (missing(mean)) {
    mean <- rlang::as_name("mean")
  } else {
    mean <- substitute(mean)
  }

  if (missing(lower)) {
    lower <- rlang::as_name("lower")
  } else {
    lower <- substitute(lower)
  }

  if (missing(upper)) {
    upper <- rlang::as_name("upper")
  } else {
    upper <- substitute(upper)
  }

  if (missing(labeltext)) {
    lblid <- rlang::as_name("labeltext")
  } else {
    lblid <- substitute(labeltext)
  }
  labeltext <- tryCatch(x %>% dplyr::pull({{ lblid }}), error = function(e) labeltext)


  estimates <- list(mean = x %>% dplyr::pull({{ mean }}),
                    lower = x %>% dplyr::pull({{ lower }}),
                    upper = x %>% dplyr::pull({{ upper }}))

  forestplot(labeltext = labeltext, mean = estimates$mean, lower = estimates$lower, upper = estimates$upper, ...)
}
