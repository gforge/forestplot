#' @rdname forestplot
#' @method forestplot grouped_df
#' @export
forestplot.grouped_df <- function(x, labeltext, mean, lower, upper, legend, is.summary, boxsize, ...) {
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
    if (is.function(is.summary)) {
      stop("Invalid summary input, does column, '", sumid, "', actually exist?")
    }
  } else {
    is.summary <- FALSE
  }

  if (!missing(boxsize)) {
    boxid <- substitute(boxsize)
    boxsize <- tryCatch(x |> dplyr::pull({{ boxid }}) |> sapply(function(x) ifelse(is.na(x), NA, x)),
                        error = function(e) boxsize)
  } else {
    boxsize <- NULL
  }

  groups <- attr(x, "groups") |>
    dplyr::select(-.rows & where(\(col) length(unique(col)) > 1)) |>
    colnames()

  if (length(groups) == 0) {
    groups <- attr(x, "groups") |>
      dplyr::select(-.rows) |>
      colnames()
    stop("You are using groups but there is only one group (call dplyr::ungroup() to undo the groups first), the grouped variable(s): ",
            paste(groups, collapse = ", "))
  }

  raw_x <- x |> dplyr::ungroup()

  # Convert into a clean dataset
  fp_label_core_values <- raw_x |>
    dplyr::select({{ lblid }},
                  mean = {{ mean }},
                  lower = {{ lower }},
                  upper = {{ upper }})

  fp_group_and_label_cols <- raw_x |>
    dplyr::mutate(dplyr::across({{ lblid }}, \(x) dplyr::if_else(x == "", NA_character_, x))) |>
    tidyr::unite(".fp_groups", dplyr::all_of(groups), sep = " > ", remove = FALSE) |>
    tidyr::unite(".fp_labels", {{ lblid }}, sep = " > ", na.rm = TRUE) |>
    dplyr::select(dplyr::starts_with(".fp"), dplyr::all_of(groups))

  core_data <- dplyr::bind_cols(fp_label_core_values,
                                fp_group_and_label_cols) |>
    dplyr::group_by(.fp_groups)

  if (length(is.summary) %in% c(1, nrow(core_data))) {
    core_data <- core_data |>
      dplyr::ungroup() |>
      dplyr::mutate(is.summary = is.summary) |>
      dplyr::group_by(.fp_groups)
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
    dplyr::select(.fp_labels, .fp_groups) |>
    dplyr::mutate(level = sapply(.fp_labels, \(lbl) which(all_labels == lbl)[[1]]),
                  lead_level = dplyr::lead(level)) |>
    dplyr::filter(!is.na(lead_level) & level > lead_level)
  if (nrow(bad_rows) > 0) {
    stop("There are invalid labels: ", bad_rows$.fp_labels |> paste(collapse = ", "),
         "\n appear in the wrong position.")
  }

  bad_rows <- core_data |>
    dplyr::filter(!is.na(mean)) |>
    dplyr::group_by(.fp_groups, .fp_labels) |>
    dplyr::count() |>
    dplyr::filter(n > 1)
  if (nrow(bad_rows) > 0) {
    stop("There are seem be non-unique labels: ", bad_rows$.fp_labels |> paste(collapse = ", "))
  }

  # Add missing rows to those groups that don't have the given category
  fixed_data <- core_data |>
    tidyr::nest() |>
    dplyr::mutate(data = lapply(data, function(df) {
      for (i in 1:length(all_labels)) {

        if (i > nrow(df) ||
            df$.fp_labels[i] != all_labels[i]) {
          new_row <- core_data |>
            dplyr::ungroup() |>
            dplyr::select({{lblid}}, .fp_labels, dplyr::all_of(groups)) |>
            dplyr::filter(.fp_labels == all_labels[i]) |>
            dplyr::distinct(.fp_labels, .keep_all = TRUE)

          if (nrow(new_row) == 0) {
            stop("There is no data for the label: ", all_labels[i])
          }
          if (i > nrow(df)) {
            df <- tibble::add_row(df,
                                  new_row)
          } else {
            df <- tibble::add_row(df,
                                  new_row,
                                  .before = i)
          }
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
    grouped_columns <- attr(x, "groups") |>
      dplyr::select(-.rows) |>
      colnames()
    legend <- fixed_data |>
      dplyr::ungroup() |>
      dplyr::select({{grouped_columns}}) |>
      dplyr::distinct() |>
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
                      \(value_col) fixed_data |>
                        dplyr::select(.fp_labels,
                                      .fp_groups,
                                      {{value_col}}) |>
                        tidyr::pivot_wider(names_from = .fp_groups,
                                           values_from = {{value_col}},
                                           names_prefix = "@estimates@") |>
                        dplyr::select(dplyr::starts_with("@estimates@")) |>
                        dplyr::rename_with(\(x) sub(pattern = "^@estimates@",
                                                    replacement = "",
                                                    x = x)) |>
                        as.matrix(),
                      simplify = FALSE)

  forestplot.default(
    labeltext = labeltext,
    mean = estimates$mean,
    lower = estimates$lower,
    upper = estimates$upper,
    legend = legend,
    is.summary = is.summary,
    boxsize = boxsize,
    ...
  )
}

globalVariables(c("data", ".rows", ".fp_labels", ".fp_groups", "n", "level", "where"))
