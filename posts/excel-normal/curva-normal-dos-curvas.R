library(ggplot2)
library(ggthemes)

graf_normal <- function(Xmedia1, Xdt1, Xmedia2, Xdt2, titulo, texto1, texto2) {
  
  Xmin1 <- Xmedia1-4*Xdt1
  Xmax1 <- Xmedia1+4*Xdt1

  Xmin2 <- Xmedia2-4*Xdt2
  Xmax2 <- Xmedia2+4*Xdt2

  Ymax1 <- max(dnorm(Xmedia1, Xmedia1, Xdt1))
  Ymax2 <- max(dnorm(Xmedia2, Xmedia2, Xdt2))
  
  Xmin <- min(Xmin1, Xmin2)
  Xmax <- max(Xmax1, Xmax2)
  
  ggplot(data.frame(X = c(Xmin, Xmax)), aes(x = X)) +
    geom_hline(yintercept = 0, colour = "grey", linewidth = 1) +
    stat_function(fun = dnorm, args = c(Xmedia1, Xdt1), 
                  linewidth = 1, 
                  colour = "grey") +
    stat_function(fun = dnorm, args = c(Xmedia2, Xdt2), 
                  linewidth = 1, 
                  colour = "black") +
    geom_segment(aes(x = Xmedia1, y = 0, xend = Xmedia1, yend = Ymax1), 
                 linetype = "dashed", 
                 linewidth = 0, 
                 colour = "grey") +
    geom_segment(aes(x = Xmedia2, y = 0, xend = Xmedia2, yend = Ymax2), 
                 linetype = "dashed", 
                 linewidth = 0, 
                 colour = "black") +
    theme_economist(base_size = 25, horizontal = FALSE) +
    theme(
       line = element_blank(),
       axis.line.y = element_blank(),
       axis.text.x = element_blank(),
       axis.text.y = element_blank(),
       axis.ticks = element_blank(),
       axis.title.x = element_blank(),
       axis.title.y = element_blank(),
       legend.position = "none",
       panel.grid = element_blank(),
       panel.border = element_blank(),
       plot.background = element_rect(colour = "white"),
     ) +
    xlim(c(Xmin, Xmax)) +
    annotate(
      "text",
      x = Xmedia1, y = -0.003,
      label = stringr::str_wrap("Valor medio", width = 15),
      color = "black",
      alpha = 0.8,
      size = 7,
      lineheight = .9
    ) +
    annotate(
      "text",
      x = Xmin1 + 2, y = -0.003,
      label = stringr::str_wrap("Frío", width = 15),
      color = "black",
      alpha = 0.8,
      size = 7,
      lineheight = .9
    ) +
    annotate(
      "text",
      x = Xmax1, y = -0.003,
      label = stringr::str_wrap("Calor", width = 15),
      color = "black",
      alpha = 0.8,
      size = 7,
      lineheight = .9
    )  +
  labs(title = titulo) +
    annotate(
      "text",
      x = 230, y = 0.03,
      label = stringr::str_wrap(texto1,
                                width = 15),
      color = "blue",
      alpha = 0.8,
      size = 7,
      lineheight = .9
    ) +
    geom_segment(aes(x = 229.5, y = 0.025, xend = 233.5, yend = 0.007), 
                 alpha = 0.8,
                 linewidth = 0, 
                 colour = "blue") +
    annotate(
      "text",
      x = 275, y = 0.03,
      label = stringr::str_wrap(texto2,
                                width = 15),
      color = "red",
      alpha = 0.8,
      size = 7,
      lineheight = .9
    ) +
    geom_segment(aes(x = 274, y = 0.025, xend = 269, yend = 0.008), 
                 alpha = 0.8,
                 linewidth = 0, 
                 colour = "red")
  
}

g1 <- graf_normal(250, 7, 253, 7, "Aumento del valor medio", "Menos frío extremo", "Más calor extremo")
g2 <- graf_normal(250, 7, 250, 9, "Aumento de la varianza", "Más frío extremo", "Más calor extremo")
g3 <- graf_normal(250, 7, 253, 9, "Aumento del valor medio y de la varianza", 
                  "Menos impacto en frío extremo", "Más calor extremo")

g1 <- g1 + 
  geom_segment( x = 250, 
                y = 0.03, 
                xend = 253, 
                yend = 0.03, 
                colour='grey', 
                linewidth=2,
                arrow = arrow(length = unit(0.5, "cm"))) 

g2 <- g2 + 
  geom_segment( x = 250 + 3, 
                y = 0.02, 
                xend = 256, 
                yend = 0.015, 
                colour='grey', 
                linewidth=2,
                arrow = arrow(length = unit(0.5, "cm"))) +
  geom_segment( x = 247, 
                y = 0.02, 
                xend = 244, 
                yend = 0.015, 
                colour='grey', 
                linewidth=2,
                arrow = arrow(length = unit(0.5, "cm")))

g3 <- g3 + 
  geom_segment( x = 250, 
                y = 0.035, 
                xend = 253, 
                yend = 0.035, 
                colour='gray',
                linewidth=2,
                arrow = arrow(length = unit(0.5, "cm"))) +
  geom_segment( x = 257, 
                y = 0.02, 
                xend = 259, 
                yend = 0.015, 
                colour='gray', 
                linewidth=2,
                arrow = arrow(length = unit(0.5, "cm"))) +
  geom_segment( x = 249, 
                y = 0.02, 
                xend = 247, 
                yend = 0.015, 
                colour='gray', 
                linewidth=2,
                arrow = arrow(length = unit(0.5, "cm"))) 
  
g1
g2
g3

