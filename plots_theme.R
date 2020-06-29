#library
library(tidyverse)
library(cowplot)

#plots theme
my_theme <-   theme_half_open(12) +
 theme(
    plot.margin = margin(30, 10, 30, 20),
    plot.title = element_text(face = "bold.italic",
      size = 12,
      hjust = 0,
      vjust = 10,
      color = "gray20"),
    plot.title.position = "plot",
    axis.title = element_text(face = "italic", color = "gray20"),
    axis.title.x = element_text(vjust = -5),
    axis.title.y = element_text(vjust = 5),
    axis.text = element_text(color = "gray20"),
    legend.position = "top",
    legend.margin = margin(c(5,5,10,0)),
    legend.title = element_text(face = "italic"),
    legend.text = element_text(margin = margin(r = 10, unit = "pt")),
    plot.caption.position = "panel",
    plot.caption = element_text(size = 8, face = "italic", color = "grey30", vjust = -10),
    strip.text = element_text(face = "bold")
  )
theme_set(my_theme)

#set options to display bigger plots in jupyter notebook
options(repr.plot.width = 7,
  repr.plot.height = 5,
  repr.plot.res = 200)
