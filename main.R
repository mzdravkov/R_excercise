# Question: Has the loss of water flow of the Nile coincided with the loss of water
# levels of lake Huron.
#
# Dataset: datasets::LakeHuron and datasets::Nile
# (built into R; 2201 people, no data files needed)
#
# Usage:
#   Rscript scripts/main.R [width] [height] [output_file]
#
# Arguments (all optional, defaults in brackets):
#   width        Width of the output image.
#   height       Height of the output image.
#   output_file  Path of the .png file to write [figure.png]
#
# Examples:
#   Rscript scripts/titanic_infographic.R
#   Rscript scripts/titanic_infographic.R 1200 800 figure.png

library(tidyverse)
library(grid)

args <- commandArgs(trailingOnly = TRUE)

width <- if (length(args) >= 1) args[1] else 8
height <- if (length(args) >= 2) as.numeric(args[2]) else 6
output_file <- if (length(args) >= 3) args[3] else "figure.png"

ds <- ts(cbind(Nile, LakeHuron), start=min(cbind(start(Nile)[1], start(LakeHuron)[1])))
df <- data.frame(ds)
df$year = start(ds)[1]:end(ds)[1]

df_long <- df %>%
  pivot_longer(
    cols = c(Nile, LakeHuron),
    names_to = "variable",
    values_to = "value"
  )


common <- theme(
  # panel.title = element_text(
  #  face = "bold", size = 10, colour = "#FAB8BA",
  #  lineheight = 1.1, margin = margin(b = 10)
  # ),
  #plot.background = element_rect(
  # fill = "#FAB8BA",
  #  colour = NA
  #),
  # plot.text = element_text(
  #  colour = "black",
  #  size = 12,
  #  face = "bold",
   # family = "Arial"
  #),
  plot.background = element_rect(fill = "#FBF8F3", colour = NA),
  panel.background = element_rect(fill = "#FBF8F3"),
  plot.title = element_text(
    face = "bold",
    hjust = 0.5,
    size = 14,
    color = "#705a35"
  ),
)

p1 <- ggplot(df, aes(x = year, y = Nile)) +
  geom_line(color = "#c10d3a") +
  geom_rect(
    aes(xmin = 1895, xmax = 1899,
        ymin = -Inf, ymax = Inf),
    fill = "#FAFAFA",
    alpha = 0.015,
    inherit.aes = FALSE
  ) +
  labs(
    title = "Annual flow of the river Nile at Aswan",
    x = NULL,
    # y = "Flow [108m3]"
    y = expression("Flow, 10" * ""^{8} * m^3)
  ) +
  common

p2 <- ggplot(df, aes(x = year, y = LakeHuron)) +
  geom_line(color = "#c10d3a") +
  geom_rect(
    aes(xmin = 1885, xmax = 1894,
        ymin = -Inf, ymax = Inf),
    fill = "#FAFAFA",
    alpha = 0.015,
    inherit.aes = FALSE
  ) +
  labs(
    title = "Water level of Lake Huron",
    x = "Year",
    y = "Level, ft"
  ) +
  common

png(
  filename = output_file,
  width = as.numeric(width),
  height = as.numeric(height),
  units = "in",
  res = 300,
  bg = "#FBF8F3"
)

grid.newpage()

pushViewport(
  viewport(
    layout = grid.layout(
      nrow = 3,
      ncol = 1,
      heights = unit(c(3, 3, 1), "null")
    )
  )
)

pushViewport(viewport(layout.pos.row = 1))
grid.draw(ggplotGrob(p1))
popViewport()

pushViewport(viewport(layout.pos.row = 2))
grid.draw(ggplotGrob(p2))
popViewport()

pushViewport(viewport(layout.pos.row = 3))
grid.rect(
  gp = gpar(
    fill = "#FBF8F3",
    col = NA
  )
)
grid.text(
  "In the period between 1895 and 1899 the Nile saw a significant reduction in water flow,\n only a few years after the level of Lake Huron dropped.",
  x = 0.5,
  y = 0.4,
  gp = gpar(fontsize = 10, col = "grey40", fontface = "italic")
) + theme(
  grid.background = "black"
)
popViewport()
popViewport()

dev.off()