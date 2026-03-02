#' @rdname forestplot
#' @method forestplot data.frame
#' @param x The data frame with or without grouping
#' @export
forestplot.data.frame <- function(x, mean, lower, upper, labeltext, is.summary, boxsize, ...) {
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

  if (!missing(boxsize)) {
    boxid <- substitute(boxsize)
    boxsize <- tryCatch(x |> dplyr::pull({{ boxid }}) |> sapply(function(x) ifelse(is.na(x), NA, x)),
      error = function(e) boxsize
    )
  } else {
    boxsize <- NULL
  }

  if (!missing(is.summary)) {
    sumid <- substitute(is.summary)
    is.summary <- tryCatch(x |> dplyr::pull({{ sumid }}) |> sapply(function(x) ifelse(is.na(x), FALSE, x)),
      error = function(e) is.summary
    )
    if (is.function(is.summary)) {
      stop("Invalid summary input, does column, '", sumid, "', actually exist?")
    }
  } else {
    is.summary <- FALSE
  }

  get_tidy_name <- function(value) {
    if (!isTRUE(attr(value, "tidyFormat"))) {
      return(NULL)
    }

    if (is.data.frame(value) && ncol(value) == 1) {
      return(colnames(value)[1])
    }

    NULL
  }

  out <- forestplot.default(
    labeltext = labeltext,
    mean = estimates$mean,
    lower = estimates$lower,
    upper = estimates$upper,
    is.summary = is.summary,
    boxsize = boxsize,
    ...
  )

  out$extra_arguments$.fp_data <- x
  out$extra_arguments$.fp_estimate_names <- list(
    mean = get_tidy_name(estimates$mean),
    lower = get_tidy_name(estimates$lower),
    upper = get_tidy_name(estimates$upper)
  )

  out
}
