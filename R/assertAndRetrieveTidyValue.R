#' Retriever of `tidyselect`
#'
#' As forestpot has evolved we now primarily use `tidyverse` select style. This
#' function helps with backward compatibility
#'
#' @param x The data with the potential value
#' @param value The value
#' @param name The name of the value
#' @param optional Is the value optional
#'
#' @return value with attribute
assertAndRetrieveTidyValue <- function(x,
                                       value,
                                       name = deparse(substitute(value)),
                                       optional = FALSE) {
  safeLoadPackage("dplyr")
  if (missing(value)) {
    value <- x[[name]]
    if (is.null(value)) {
      if (optional) {
        return(NULL)
      }

      stop(
        "You have not provided an argument",
        " and the data frame does not have a '", name, "' column: ",
        names(x) |> paste(collapse = ", ")
      )
    }
    return(structure(value, tidyFormat = TRUE))
  }

  # We are one-caller removed from the original call so we need to
  # do this nasty hack to get the parameter of the parent function
  orgName <- eval(substitute(substitute(value)), envir = parent.frame())
  tryCatch(dplyr::select(x, {{ orgName }}) |> structure(tidyFormat = TRUE),
    error = function(e) {
      return(structure(value,
        tidyFormat = FALSE
      ))
    }
  )
}
