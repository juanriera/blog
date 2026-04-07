library(tidyverse)
library(cowplot)
library(magick)

# ── Datos ──────────────────────────────────────────────────────────────────
df <- read_csv2("datos/camembert.csv",
                locale = locale(encoding = "ISO-8859-1")) |>
  mutate(
    fecha    = dmy(fecha),
    mes      = month(fecha),
    estacion = case_when(
      mes %in% 7:9          ~ "Verano",
      mes %in% c(12, 1, 2)  ~ "Invierno",
      TRUE                  ~ "Otras"
    ),
    estacion = factor(estacion, levels = c("Invierno", "Verano", "Otras")),
    hfd = (100-est)/(100-mg)*100
  )

colores <- c("Verano" = "#D2691E", "Invierno" = "#1B3A6B", "Otras" = "grey75")

# ── Gráfico base ───────────────────────────────────────────────────────────
p <- ggplot(df, aes(x = hfd, y = ph, color = estacion)) +
  geom_point(data = filter(df, estacion == "Otras"),
             alpha = 0.30, size = 2) +
  geom_point(data = filter(df, estacion != "Otras"),
             alpha = 0.45, size = 3) +
  stat_ellipse(
    data = filter(df, estacion != "Otras"),
    geom = "path", linewidth = 1.2, level = 0.75,
    show.legend = FALSE
  ) +
  # Flechas hacia las imágenes
  geom_curve(
    data = data.frame(x = 71, y = 4.58, xend = 74.5, yend = 4.58),
    aes(x = x, y = y, xend = xend, yend = yend),
    color = "#D2691E", linewidth = 1,
    arrow = arrow(length = unit(0.25, "cm"), type = "open"),
    curvature = 0.2, inherit.aes = FALSE
  ) +
  geom_curve(
    data = data.frame(x = 72, y = 4.82, xend = 74.5, yend = 4.96),
    aes(x = x, y = y, xend = xend, yend = yend),
    color = "#1B3A6B", linewidth = 1,
    arrow = arrow(length = unit(0.25, "cm"), type = "open"),
    curvature = -0.2, inherit.aes = FALSE
  ) +
  scale_color_manual(values = colores, name = NULL) +
  labs(
    title = "Evolución estacional de los valores de HFD y pH \n de un queso de pasta blanda tipo Camembert",
    x = "HFD",
    y = "pH"
  ) +
  scale_x_continuous(breaks = seq(65, 75, 2), limits = c(65, 80)) +
  scale_y_continuous(breaks = seq(4.4, 5.1, 0.1), limits = c(4.4, 5.1)) +
  theme_bw(base_size = 13) +
  theme(
    plot.title      = element_text(hjust = 0.5),
    legend.position = "none"
  )

# ── Imágenes ───────────────────────────────────────────────────────────────
img_verano   <- magick::image_read("imagenes/Camembert-commons-wikimedia_1.png")
img_invierno <- magick::image_read("imagenes/camembert_2.png")

# ── Composición con cowplot ────────────────────────────────────────────────
# Coordenadas en fracción del canvas total (0-1): x, y = esquina inferior izquierda
ggdraw(p) +
  draw_image(img_verano,
             x = 0.65, y = 0.58, width = 0.24, height = 0.30) +
  draw_label("En verano", x = 0.77, y = 0.16,
             color = "#D2691E", fontface = "bold", size = 12) +
  draw_image(img_invierno,
             x = 0.65, y = 0.18, width = 0.24, height = 0.30) +
  draw_label("En invierno", x = 0.77, y = 0.56,
             color = "#1B3A6B", fontface = "bold", size = 12)

# ggsave("imagenes/camembert_hfd_ph.png",
#        plot = last_plot(),
#        width = 10, height = 7, dpi = 300)
