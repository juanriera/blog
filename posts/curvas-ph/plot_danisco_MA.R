library(readr)
library(ggplot2)

datos <- read_csv2("curvas_danisco_MA_tidy.csv",
                   locale = locale(encoding = "ISO-8859-1"))

datos$temperatura <- factor(datos$temperatura, levels = c(25, 32, 37))

ggplot(datos, aes(x = hora, y = pH, color = temperatura)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = c("25" = "red", "32" = "blue", "37" = "green3"),
                     labels = c("25°C", "32°C", "37°C")) +
  scale_x_continuous(breaks = seq(0, 24, by = 1)) +
  scale_y_continuous(breaks = seq(4.0, 7.0, by = 0.2), limits = c(4.0, 7.0)) +
  labs(title   = "Curvas de acidificación — Cepa MA (Danisco)",
       x       = "Tiempo (horas)",
       y       = "pH",
       color   = "Temperatura") +
  theme_bw()
