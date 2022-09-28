#' Convert margins to viewport npc margins
#'
#' @param mar A vector of margins, at positions:
#'  - 1 = bottom
#'  - 2 = left
#'  - 3 = top
#'  - 4 = right
#'
#' @return Returns a list with `bottom`, `left`, `top`, and `right` as `unit("npc")`
prepGridMargins <- function(mar) {
  mar <- rep(mar, length.out = 4)
  marList <- list()

  # This breaks without separate variables
  marList$bottom <- convertY(mar[1], "npc")
  marList$left <- convertX(mar[2], "npc")
  marList$top <- convertY(mar[3], "npc")
  marList$right <- convertX(mar[4], "npc")
  return(marList)
}
