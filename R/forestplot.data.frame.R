#' @rdname forestplot
#' @method forestplot data.frame
#' @param x The data frame with or without grouping
#' @export
forestplot.data.frame <- function(x, mean, lower, upper, labeltext, is.summary, ...) {
  safeLoadPackage("dplyr")
  safeLoadPackage("tidyr")
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
  ret <- tryCatch(x %>% dplyr::select({{ lblid }}),
                  error = function(e) e)
  if (inherits(ret, "error")) {
    # Note, we re-throw the original error if it fails
    ret <- tryCatch(labeltext, error = function(e) stop(ret))
  }
  labeltext <- ret

  estimates <- list(mean = x %>% dplyr::pull({{ mean }}),
                    lower = x %>% dplyr::pull({{ lower }}),
                    upper = x %>% dplyr::pull({{ upper }}))

  if (!missing(is.summary)) {
    sumid <- substitute(is.summary)
    is.summary <- tryCatch(x %>% dplyr::pull({{ sumid }}) %>% sapply(function(x) ifelse(is.na(x), FALSE, x)),
                           error = function(e) is.summary)
  } else {
    is.summary = FALSE
  }

  forestplot.default(labeltext = labeltext, mean = estimates$mean, lower = estimates$lower, upper = estimates$upper, is.summary = is.summary, ...)
}
