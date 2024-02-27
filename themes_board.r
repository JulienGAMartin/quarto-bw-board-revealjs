library(showtext)
library(ggplot2)
font_add_google("Pangolin", "Pangolin")
showtext_auto()
knitr::opts_chunk$set(dev.args = list(bg = "transparent"))

# theme for chalkboard
theme_blackboard <- function(...) {
  cowplot::theme_cowplot() %+replace%
    theme(
      axis.ticks = element_line(colour = "white", linewidth = 0.25),
      text = element_text(colour = "white"),
      axis.text = element_text(colour = "white", family = "Pangolin", size = 16),
      axis.title = element_text(colour = "white", family = "Pangolin", size = 24),
      axis.line = element_line(colour = "white", linewidth = 1),
      # panel.background = element_rect(colour = NA, fill = "transparent"),
      # plot.background = element_rect(colour = "white", fill = "transparent"),
      legend.position = "bottom",
      legend.title = element_blank(),
      panel.grid.minor = element_blank(),
      # panel.grid.major.x = element_line(colour = "white", size = 0.25),
      # panel.grid.major.y = element_line(colour = "white", size = 0.25),
      legend.text = element_text(size = 24),
      ...
    )
}
def_col_geom <- function(col = "white") {
  #   params <- ls(pattern = "^geom_", env = as.environment("package:ggplot2"))
  #  geoms <- gsub("geom_", "", params[-c(4, 5, 11, 22, 25, 27, 36, 37, 45, 46)])
  geoms <- c(
    "abline", "area", "bar", "blank", "boxplot", "col", "contour",
    "contour_filled", "crossbar", "curve", "density", "density_2d",
    "density_2d_filled", "density2d", "density2d_filled", "dotplot", "errorbar",
    "errorbarh", "function", "hex", "hline", "label", "line", "linerange",
    "map", "path", "point", "pointrange", "polygon", "quantile", "raster",
    "rect", "ribbon", "rug", "segment", "sf", "smooth", "spoke", "step", "text",
    "tile", "violin", "vline"
  )

  lapply(geoms, update_geom_defaults, aes(colour = col))
  lapply(geoms, update_geom_defaults, aes(fill = col, colour = col))
}

check_model_bb <- function(mod) {
  pp <- check_model(mod)
  p <- plot(pp, return_list = TRUE)
  out <- p & (theme_blackboard() +
    theme(
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 12),
      legend.text = element_text(size = 10)
    )
  )
  out
}

# theme for whiteboard
theme_whiteboard <- function(...) {
  cowplot::theme_cowplot() %+replace%
    theme(
      axis.ticks = element_line(colour = "black", linewidth = 0.25),
      text = element_text(colour = "black"),
      axis.text = element_text(colour = "black", family = "Pangolin", size = 16),
      axis.title = element_text(colour = "black", family = "Pangolin", size = 24),
      axis.line = element_line(colour = "black", linewidth = 1),
      panel.background = element_rect(colour = NA, fill = "transparent"),
      # plot.background = element_rect(colour = "white", fill = "transparent"),
      legend.position = "bottom",
      legend.title = element_blank(),
      panel.grid.minor = element_blank(),
      # panel.grid.major.x = element_line(colour = "white", size = 0.25),
      # panel.grid.major.y = element_line(colour = "white", size = 0.25),
      legend.text = element_text(size = 24),
      ...
    )
}

check_model_wb <- function(mod) {
  pp <- check_model(mod)
  p <- plot(pp, return_list = TRUE)
  out <- p & (theme_whiteboard() +
    theme(
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 12),
      legend.text = element_text(size = 10)
    )
  )
  out
}
