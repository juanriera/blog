## packages
library(tidyverse)
library(colorspace)
library(ragg)
library(cowplot)
library(ggtext)
library(pdftools)

df_penguins <- readr::read_csv('penguins.csv') |>
  mutate(species = if_else(species == "Adelie", "Adélie", species))

img <- png::readPNG("culmen_depth-transparente.png")
i1 <- grid::rasterGrob(img, interpolate = T)

img <- png::readPNG("lter_penguins.png")
i2 <- grid::rasterGrob(img, interpolate = T)

img <- png::readPNG("palmerpenguins.png")
i3 <- grid::rasterGrob(img, interpolate = T)

paleta_color <- c("darkorange", "blueviolet",  "aquamarine4")

scat <-
  ggplot(df_penguins, aes(flipper_length_mm, bill_length_mm, colour = species)) +
    geom_point(size = 3, alpha = 0.3, stroke = 1) +
    theme_minimal() +
    labs (title = "Flipper and bill length",
          subtitle="Dimensions for Adélie, Chinstrap and Gentoo penguins at Palmer Station LTER",
          caption = "Dataset and images by Allison Horst https://allisonhorst.github.io/palmerpenguins/") +
    theme(
      plot.title = element_text(size=22),
      legend.position = "none",
      plot.margin = margin(1,10,1.5,1.5, "cm")
    ) +
  scale_fill_manual(values=paleta_color) +
  scale_color_manual(values=paleta_color) +
  annotate(
    "text",
    x = 190, y = 54,
    label = "Chinstrap",
    color = "blueviolet",
    alpha = 0.5,
    size = 10,
    lineheight = .9
  ) +
  annotate(
    "text",
    x = 205, y = 34,
    label = "Adélie",
    color = "darkorange",
    alpha = 0.3,
    size = 10,
    lineheight = .9
  ) +
  annotate(
    "text",
    x = 223, y = 42,
    label = "Gentoo",
    color = "aquamarine4",
    alpha = 0.3,
    size = 10,
    lineheight = .9
  ) +
  annotation_custom(i1, ymin = 23, ymax = 45, xmin = 233, xmax =265) +
  annotation_custom(i2, ymin = 22, ymax = 72, xmin = 227, xmax =270) +
  annotation_custom(i3, ymin = 55, ymax = 65, xmin = 235, xmax =265) +
  coord_cartesian(clip = "off")

plot(scat)



    