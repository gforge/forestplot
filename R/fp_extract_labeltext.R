#' Extract label text columns
#'
#' Build a labeltext data frame from selected columns. This helper supports
#' both tidyselect syntax and explicit character column names.
#'
#' For grouped data frames, extraction follows the grouped forestplot row
#' alignment logic so that label rows correspond to the first grouped estimate
#' panel.
#'
#' @param x A data frame (grouped or ungrouped).
#' @param ... Columns selected using tidyselect syntax.
#' @param cols Optional explicit character vector of column names. Cannot be
#'   combined with `...`.
#' @param names Optional replacement names for the extracted columns.
#' @param na Replacement value for missing values in atomic scalar cells.
#'
#' @return A data frame suitable for `labeltext` in `forestplot()`.
#' @examples
#' data(inventors_vs_mello)
#'
#' # ungrouped selection with explicit columns
#' fp_extract_labeltext(
#'     inventors_vs_mello,
#'     cols = c("author", "ai", "n1i", "ci", "n2i", "orci"),
#'     names = c("Study", "E1", "N1", "E2", "N2", "OR"),
#'     na = ""
#' )
#'
#' # grouped extraction follows grouped forestplot row alignment
#' # (rename estimate columns to mean/lower/upper for grouped extraction)
#' inventors_vs_mello |>
#'     dplyr::rename(mean = est, lower = lb, upper = ub) |>
#'     dplyr::group_by(group) |>
#'     fp_extract_labeltext(author, orci, na = "")
#' @export
fp_extract_labeltext <- function(x,
                                 ...,
                                 cols = NULL,
                                 names = NULL,
                                 na = "") {
    safeLoadPackage("dplyr")
    safeLoadPackage("tidyr")

    if (!is.data.frame(x)) {
        stop("'x' must be a data.frame or grouped_df")
    }

    dots <- rlang::enquos(...)
    if (length(dots) > 0 && !is.null(cols)) {
        stop("Provide either tidyselect columns via '...' or explicit 'cols', not both")
    }

    raw_x <- if (inherits(x, "grouped_df")) {
        x |> dplyr::ungroup()
    } else {
        x
    }

    selected_cols <- fpResolveLabeltextColumns(raw_x, dots = dots, cols = cols)
    if (length(selected_cols) == 0) {
        stop("No label columns selected")
    }

    out <- if (inherits(x, "grouped_df")) {
        fpExtractGroupedLabeltext(raw_x = raw_x, grouped_x = x, label_cols = selected_cols)
    } else {
        raw_x |> dplyr::select(dplyr::all_of(selected_cols))
    }

    if (!is.null(names)) {
        if (!is.character(names) || length(names) != ncol(out)) {
            stop("'names' must be a character vector with length equal to selected columns")
        }
        colnames(out) <- names
    }

    fpReplaceLabeltextNa(out, na = na)
}

fpResolveLabeltextColumns <- function(x, dots, cols) {
    safeLoadPackage("dplyr")

    if (!is.null(cols)) {
        if (!is.character(cols)) {
            stop("'cols' must be a character vector of column names")
        }
        missing_cols <- setdiff(cols, colnames(x))
        if (length(missing_cols) > 0) {
            stop("Unknown column selector(s): ", paste(missing_cols, collapse = ", "))
        }
        return(unique(cols))
    }

    if (length(dots) == 0) {
        stop("Specify label columns using '...' or 'cols'")
    }

    dplyr::select(x, !!!dots) |> colnames()
}

fpReplaceLabeltextNa <- function(x, na = "") {
    for (j in seq_len(ncol(x))) {
        col <- x[[j]]

        if (is.list(col)) {
            x[[j]] <- lapply(col, function(value) {
                if (is.atomic(value) && length(value) == 1L && is.na(value)) {
                    return(na)
                }
                value
            })
        } else {
            idx <- is.na(col)
            if (any(idx)) {
                col[idx] <- na
            }
            x[[j]] <- col
        }
    }

    x
}

fpGetVaryingGroups <- function(x) {
    groups <- attr(x, "groups") |>
        dplyr::select(-.rows & where(\(col) length(unique(col)) > 1)) |>
        colnames()

    if (length(groups) == 0) {
        original_groups <- attr(x, "groups") |>
            dplyr::select(-.rows) |>
            colnames()
        stop(
            "You are using groups but there is only one group (call dplyr::ungroup() to undo the groups first), the grouped variable(s): ",
            paste(original_groups, collapse = ", ")
        )
    }

    groups
}

fpExtractGroupedLabeltext <- function(raw_x, grouped_x, label_cols) {
    safeLoadPackage("dplyr")
    safeLoadPackage("tidyr")

    groups <- fpGetVaryingGroups(grouped_x)
    required_estimate_cols <- c("mean", "lower", "upper")
    missing_estimate_cols <- setdiff(required_estimate_cols, colnames(raw_x))

    if (length(missing_estimate_cols) > 0) {
        stop(
            "Grouped label extraction requires estimate columns in data: ",
            paste(missing_estimate_cols, collapse = ", ")
        )
    }

    fp_label_core_values <- raw_x |>
        dplyr::select(
            dplyr::all_of(label_cols),
            dplyr::all_of(required_estimate_cols)
        )

    fp_group_and_label_cols <- raw_x |>
        dplyr::mutate(dplyr::across(dplyr::all_of(label_cols), \(value) {
            if (is.character(value)) {
                ifelse(value == "", NA, value)
            } else {
                value
            }
        })) |>
        tidyr::unite(".fp_groups", dplyr::all_of(groups), sep = " > ", remove = FALSE, na.rm = TRUE) |>
        tidyr::unite(".fp_labels", dplyr::all_of(label_cols), sep = " > ", na.rm = TRUE) |>
        dplyr::select(dplyr::starts_with(".fp"), dplyr::all_of(groups))

    core_data <- dplyr::bind_cols(fp_label_core_values, fp_group_and_label_cols) |>
        dplyr::group_by(.fp_groups)

    fixed_data <- grp_fix_missing_rows(
        core_data,
        lblid = dplyr::all_of(label_cols)
    )

    fixed_data |>
        dplyr::ungroup() |>
        dplyr::filter(.fp_groups == .fp_groups[1]) |>
        dplyr::select(dplyr::all_of(label_cols))
}

#' Extract labels from a forestplot object
#'
#' Rebuild label columns on an existing `gforge_forestplot` object using
#' selectors mapped in a pipe-friendly style.
#'
#' This helper requires that the plot object was created from a data frame via
#' `forestplot.data.frame()` or `forestplot.grouped_df()` so that source data
#' are available.
#'
#' @param x A `gforge_forestplot` object.
#' @param ... Named mappings where each name is the output label column name and
#'   each value is a selector for one source column.
#' @param na Replacement value for missing values in atomic scalar cells.
#'
#' @return A modified `gforge_forestplot` object.
#' @examples
#' data(inventors_vs_mello)
#' inventors_vs_mello |>
#'     forestplot(mean = est, lower = lb, upper = ub, labeltext = author) |>
#'     fp_extract_labels(Study = author, E1 = ai, N1 = n1i, E2 = ci, N2 = n2i)
#' @export
fp_extract_labels <- function(x, ..., na = "") {
    if (!inherits(x, "gforge_forestplot")) {
        stop("'x' must be a gforge_forestplot object")
    }

    source_data <- x$extra_arguments$.fp_data
    if (is.null(source_data)) {
        stop("This plot object does not contain source data. Build the plot from a data.frame before calling fp_extract_labels()")
    }

    dots <- rlang::enquos(...)
    if (length(dots) == 0) {
        stop("Provide at least one named mapping, e.g. fp_extract_labels(Study = author)")
    }

    out_names <- names(dots)
    if (is.null(out_names) || any(out_names == "")) {
        stop("All mappings in '...' must be named, e.g. Study = author")
    }

    selector_data <- if (inherits(source_data, "grouped_df")) {
        source_data |> dplyr::ungroup()
    } else {
        source_data
    }

    selected_cols <- vapply(dots, function(dot) {
        selected <- dplyr::select(selector_data, !!dot)
        if (ncol(selected) != 1) {
            stop("Each mapping must resolve to exactly one column")
        }
        colnames(selected)[1]
    }, character(1))

    labels_source <- source_data
    estimate_names <- x$extra_arguments$.fp_estimate_names
    if (inherits(labels_source, "grouped_df") && !is.null(estimate_names)) {
        rename_map <- c(
            mean = estimate_names$mean,
            lower = estimate_names$lower,
            upper = estimate_names$upper
        )
        rename_map <- rename_map[!is.na(rename_map) & !is.null(rename_map)]
        rename_map <- rename_map[rename_map %in% colnames(labels_source)]
        rename_map <- rename_map[names(rename_map) != rename_map]

        if (length(rename_map) > 0) {
            labels_source <- labels_source |>
                dplyr::rename(!!!rename_map)
        }
    }

    new_labels <- fp_extract_labeltext(
        labels_source,
        cols = unname(selected_cols),
        names = out_names,
        na = na
    )

    x$labels <- prepLabelText(new_labels, nr = nrow(x$estimates))
    x$align <- prepAlign(NULL, graph.pos = x$graph.pos, nc = attr(x$labels, "no_cols"))

    x
}
