## librerias
library(tidyverse)

theme_set(theme_minimal(base_size = 15))

## lee los datos
df_penguins <- readr::read_csv('penguins.csv') |>
  mutate(species = if_else(species == "Adelie", "Adélie", species))

## lee las imágenes
img <- png::readPNG("culmen_depth-transparente.png")
i1 <- grid::rasterGrob(img, interpolate = T)

img <- png::readPNG("lter_penguins.png")
i2 <- grid::rasterGrob(img, interpolate = T)

img <- png::readPNG("palmerpenguins.png")
i3 <- grid::rasterGrob(img, interpolate = T)

## prepara el gráfico
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
    plot.margin = margin(1,6,1.5,1.5, "cm"),
    plot.caption = element_text(hjust=0, face="italic")
  ) +
  scale_fill_manual(values=paleta_color) +
  scale_color_manual(values=paleta_color) +
  annotate(
    "text",
    x = 185, y = 54,
    label = "Chinstrap",
    color = "blueviolet",
    alpha = 0.8,
    size = 4,
    lineheight = .9
  ) +
  annotate(
    "text",
    x = 205, y = 34,
    label = "Adélie",
    color = "darkorange",
    alpha = 0.8,
    size = 4,
    lineheight = .9
  ) +
  annotate(
    "text",
    x = 223, y = 42,
    label = "Gentoo",
    color = "aquamarine4",
    alpha = 0.8,
    size = 4,
    lineheight = .9
  ) +
  annotation_custom(i1, ymin = 45, ymax = 65, xmin = 235, xmax =275) +
  annotation_custom(i2, ymin = 20, ymax = 55, xmin = 225, xmax =275) +
  annotation_custom(i3, ymin = 63.5, ymax = 70, xmin = 255, xmax =275) +
  coord_cartesian(clip = "off")

plot(scat)