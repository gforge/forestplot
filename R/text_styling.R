#' Text styling
#'
#' This is a collection of functions to allow styling of text
#'
#' @param txt The text to styl
#' @returns A list of txt with style attributes
#'
#' @examples
#' fp_txt_italic("Italic text")
#' @export
#' @rdname text_styling
fp_txt_italic <- function(txt)  {
  sapply(txt, \(str) {
    txt_gp <- attr(str, "txt_gp")
    if (is.null(txt_gp)) {
      txt_gp <- gpar()
    }
    txt_gp$fontface <- "italic"
    attr(str, "txt_gp")  <- txt_gp
    return(str)
  },
  simplify = FALSE,
  USE.NAMES = FALSE)
}

#' @export
#' @rdname text_styling
fp_txt_bold <- function(txt)  {
  sapply(txt, \(str) {
    txt_gp <- attr(str, "txt_gp")
    if (is.null(txt_gp)) {
      txt_gp <- gpar()
    }
    txt_gp$fontface <- "bold"
    attr(str, "txt_gp")  <- txt_gp
    return(str)
  },
  simplify = FALSE,
  USE.NAMES = FALSE)
}

#' @export
#' @rdname text_styling
fp_txt_plain <- function(txt)  {
  sapply(txt, \(str) {
    txt_gp <- attr(str, "txt_gp")
    if (is.null(txt_gp)) {
      txt_gp <- gpar()
    }
    txt_gp$fontface <- "plain"
    attr(str, "txt_gp")  <- txt_gp
    return(str)
  },
  simplify = FALSE,
  USE.NAMES = FALSE)
}

#' @export
#' @rdname text_styling
fp_align_left <- function(txt)  {
  sapply(txt, \(str) {
    attr(str, "align")  <- "l"
    return(str)
  },
  simplify = FALSE,
  USE.NAMES = FALSE)
}

#' @export
#' @rdname text_styling
fp_align_center <- function(txt)  {
  sapply(txt, \(str) {
    attr(str, "align")  <- "c"
    return(str)
  },
  simplify = FALSE,
  USE.NAMES = FALSE)
}

#' @export
#' @rdname text_styling
fp_align_right <- function(txt)  {
  sapply(txt, \(str) {
    attr(str, "align")  <- "r"
    return(str)
  },
  simplify = FALSE,
  USE.NAMES = FALSE)
}

merge_with_txt_gp <- function(gp_list, txt_out) {
  txt_gp <- attr(txt_out, "txt_gp")
  if (is.null(txt_gp)) {
    return(gp_list)
  }

  for (n in names(txt_gp)) {
    gp_list[[n]] <- txt_gp[[n]]
  }

  return(gp_list)
}
