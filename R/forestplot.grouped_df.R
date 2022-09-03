#' @rdname forestplot
#' @method forestplot grouped_df
#' @export
forestplot.grouped_df <- function(x, labeltext, mean, lower, upper, legend, is.summary, ...) {
  safeLoadPackage("dplyr")
  safeLoadPackage("tidyr")
  safeLoadPackage("rlang")
  safeLoadPackage("tidyselect")
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

  if (!missing(is.summary)) {
    sumid <- substitute(is.summary)
    is.summary <- tryCatch(x |> dplyr::pull({{ sumid }}) |> sapply(function(x) ifelse(is.na(x), FALSE, x)),
                           error = function(e) is.summary)
  } else {
    is.summary <- FALSE
  }

  groups <- attr(x, "groups") |>
    dplyr::select(-.rows & where(\(col) length(unique(col)) > 1)) |>
    colnames()

  # Convert into a clean dataset
  core_data <- x |>
    dplyr::ungroup() |>
    dplyr::select({{ lblid }},
                  mean = {{ mean }},
                  lower = {{ lower }},
                  upper = {{ upper }}) |>
    dplyr::bind_cols(x |>
                       tidyr::unite(".fp_groups", dplyr::all_of(groups), sep = " > ") |>
                       tidyr::unite(".fp_labels", {{lblid}}, sep = " > ") |>
                       dplyr::select(dplyr::starts_with(".fp"))) |>
    dplyr::group_by(.fp_groups)

  if (length(is.summary) %in% c(1, nrow(core_data))) {
    core_data <- dplyr::mutate(core_data, is.summary = is.summary)
    is.summary <- NULL
  }

  all_labels <- core_data |>
    tidyr::nest() |>
    dplyr::pull(data) |>
    lapply(\(x) x$.fp_labels) |>
    unlist() |>
    unique()

  # Check for bad data assumptions
  bad_rows <- core_data |>
    dplyr::mutate(level = sapply(.fp_labels, \(lbl) which(all_labels == lbl)[[1]])) |>
    dplyr::filter(level > dplyr::lead(level))
  if (nrow(bad_rows) > 0) {
    stop("There are seem be invalid the labels: ", bad_rows$.fp_labels |> paste(collapse = ", "),
         "\n appear in the wrong position.")
  }

  bad_rows <- core_data |>
    dplyr::group_by(.fp_groups, .fp_labels) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
    dplyr::filter(n > 1)
  if (nrow(bad_rows) > 0) {
    stop("There are seem be non-unique labels: ", bad_rows$.fp_labels |> paste(collapse = ", "))
  }

  # Add missing rows to those groups that don't have the given category
  fixed_data <- core_data |>
    tidyr::nest() |>
    dplyr::mutate(data = lapply(data, function(df) {
      for (i in 1:length(all_labels)) {
        if (df$.fp_labels[i] != all_labels[i]) {
          new_row <- core_data |>
            dplyr::ungroup() |>
            dplyr::select({{lblid}}, .fp_labels) |>
            dplyr::filter(.fp_labels == all_labels[i]) |>
            dplyr::distinct(.fp_labels, .keep_all = TRUE)

          df <- tibble::add_row(df,
                                new_row,
                                .before = i)
        }
      }
      return(df)
    })) |>
    tidyr::unnest(cols = data)

  if (!is.null(is.summary)) {
    fixed_is.summary <- rep(is.summary, times = nrow(attr(fixed_data, "groups")))
    if (length(fixed_is.summary) != nrow(fixed_data)) {
      stop("Expected is.summary to have the length ", fixed_data |> dplyr::filter(.fp_groups == .fp_groups[1]) |> nrow(),
           " but got instead length of ", length(is.summary),
           ". Note that you may also provide length of 1 or the entire initial data size.")
    }
    fixed_data$is.summary <- fixed_is.summary
  }

  if (missing(legend)) {
    legend <- attr(fixed_data, "groups") |>
      dplyr::select(-.rows) |>
      tidyr::unite(col = "legend", dplyr::everything(), sep = " > ") |>
      purrr::pluck("legend")
  }

  # Retrieve the final data for the forestplot.default
  labeltext <- fixed_data |>
    dplyr::ungroup() |>
    dplyr::filter(.fp_groups == .fp_groups[1]) |>
    dplyr::select({{lblid}}) |>
    # The list is important as the labeltext can contain expressions
    # see forestplot example
    as.list()

  is.summary <- fixed_data |>
    dplyr::ungroup() |>
    dplyr::filter(.fp_groups == .fp_groups[1]) |>
    purrr::pluck("is.summary")

  # Convert estimates to two-dimensional matrices
  estimates <- sapply(c("mean", "lower", "upper"),
                      \(n) fixed_data[[n]] |>
                        (\(raw_data)
                         lapply(attr(fixed_data, "groups")$.rows,
                                \(row_numbers) raw_data[row_numbers]))() |>
                        (\(cols) {
                          names(cols) <- attr(fixed_data, "groups")$.fp_groups
                          suppressMessages(dplyr::bind_cols(cols))
                        })() |>
                        as.matrix(),
                      simplify = FALSE)

  forestplot.default(
    labeltext = labeltext,
    mean = estimates$mean,
    lower = estimates$lower,
    upper = estimates$upper,
    legend = legend,
    is.summary = is.summary,
    ...
  )
}

globalVariables(c("data", ".rows", ".fp_labels", ".fp_groups", "n", "level", "where"))
