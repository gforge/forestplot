#' @rdname forestplot
#' @method forestplot grouped_df
#' @export
forestplot.grouped_df <- function(x, labeltext, mean, lower, upper, legend, is.summary, ...) {
  safeLoadPackage("dplyr")
  safeLoadPackage("rlang")
  groups <- attr(x, "groups")

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
  ret <- tryCatch(suppressMessages(x %>% dplyr::select({{ lblid }})),
    error = function(e) e
  )
  if (inherits(ret, "error")) {
    # Note, we re-throw the original error if it fails
    ret <- tryCatch(labeltext, error = function(e) stop(ret))
  } else {
    # Remove the group variable
    ret <- ret %>%
      tidyr::nest() %>%
      dplyr::pull(data) %>%
      Reduce(function(x, y) {
        if (nrow(x) != nrow(y)) {
          stop("The groups must be identical in the number of rows, check your")
        }
        for (col_no in 1:ncol(x)) {
          x[[col_no]] <- apply(cbind(x[[col_no]], y[[col_no]]), MARGIN = 1, unique) %>%
            sapply(paste, collapse = ", ")
        }
        x
      }, .)
  }
  labeltext <- ret


  estimates <- list(
    mean = x %>% dplyr::pull({{ mean }}),
    lower = x %>% dplyr::pull({{ lower }}),
    upper = x %>% dplyr::pull({{ upper }})
  )
  estimates <- sapply(estimates,
    function(est) {
      suppressMessages(groups$.rows %>%
        lapply(function(row_numbers) est[row_numbers]) %>%
        dplyr::bind_cols() %>%
        as.matrix())
    },
    simplify = FALSE
  )


  if (missing(legend)) {
    legend <- groups %>%
      dplyr::select(-.rows) %>%
      apply(MARGIN = 1, function(x) paste(x, collapse = ", "))
  }

  if (!missing(is.summary)) {
    sumid <- substitute(is.summary)
    is.summary <- tryCatch(x %>% dplyr::pull({{ sumid }}) %>% sapply(function(x) ifelse(is.na(x), FALSE, x)),
      error = function(e) is.summary
    )
  } else {
    is.summary <- FALSE
  }

  forestplot.default(
    labeltext = labeltext, mean = estimates$mean, lower = estimates$lower, upper = estimates$upper, legend = legend,
    is.summary = is.summary, ...
  )
}

globalVariables(c("data", ".", ".rows"))
