ask <- par(ask = TRUE)

# An example of how fpShapesGp works

styles <- fpShapesGp(
  default = gpar(col = "pink", lwd = 2, lineend = "square", linejoin = "mitre"),
  grid = list(
    gpar(col = "blue"),
    gpar(col = "black"),
    gpar(col = "blue")
  ),
  box = list(
    gpar(fill = "black"),
    gpar(fill = "blue"),
    gpar(fill = "black"),
    gpar(fill = "blue")
  ),
  lines = gpar(lty = "dashed"),
  vertices = gpar(lwd = 5, col = "red")
)

forestplot(
  labeltext = c("Author1", "Author2", "Author3", "Author4"),
  grid = c(1, 3, 5),
  mean = 1:4, lower = 0:3, upper = 2:5,
  shapes_gp = styles
)

par(ask = ask)