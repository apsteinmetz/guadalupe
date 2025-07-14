library (ggplot2)
library(grid)
library(gridExtra)

# Dummy plot
df <- data.frame(x = 1:10, y = 1:10)
base <- ggplot(df, aes(x, y)) + geom_point(color= "blue") +
  annotation_custom(
  grob = grid::rectGrob(
    gp = grid::gpar(fill = linearGradient(colours = c("black","lightblue")), col = "black")
  ),
  xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)

base
