#' Safely loads package
#'
#' Stops if the package doesn't exist
#' @inheritParams base::requireNamespace
safeLoadPackage <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop("The package ", package, " is needed for this function to work. Please install it.",
         call. = FALSE
    )
  }
}
