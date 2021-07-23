#' @rdname forestplot
#' @method forestplot grouped_df
#' @importFrom rlang as_name
#' @export
forestplot.grouped_df <- function(x, labeltext, mean, lower, upper, legend, ...) {
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
    labeltext <- rlang::as_name("labeltext")
  } else {
    labeltext <- substitute(labeltext)
  }

  groups <- attr(x, "groups")
  estimates <- list(mean = x %>% dplyr::pull({{ mean }}),
                    lower = x %>% dplyr::pull({{ lower }}),
                    upper = x %>% dplyr::pull({{ upper }}))
  estimates <- sapply(estimates,
                      function(est) {
                        suppressMessages(groups$.rows %>%
                                           lapply(function(row_numbers) est[row_numbers]) %>%
                                           bind_cols() %>%
                                           as.matrix())
                      },
                      simplify = FALSE)

  labeltext <- x %>% dplyr::pull({{ labeltext }}) %>% extract(groups$.rows[[1]])

  if (missing(legend)) {
    legend <- groups$group
  }

  forestplot(labeltext = labeltext, mean = estimates$mean, lower = estimates$lower, upper = estimates$upper, legend = legend, ...)
}
