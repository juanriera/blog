library(ggplot2)
library(ggthemes)

# https://stackoverflow.com/questions/75502312/in-ggplot-how-to-fill-area-between-two-normal-curves/

graf_normal <- function(Xmedia1, Xdt1, Xmedia2, Xdt2, n=100) {
  
  Xmin1 <- Xmedia1-4*Xdt1
  Xmax1 <- Xmedia1+4*Xdt1
  
  Xmin2 <- Xmedia2-4*Xdt2
  Xmax2 <- Xmedia2+4*Xdt2
  
  Ymax1 <- max(dnorm(Xmedia1, Xmedia1, Xdt1))
  Ymax2 <- max(dnorm(Xmedia2, Xmedia2, Xdt2))
  
  Xmin <- min(Xmin1, Xmin2)
  Xmax <- max(Xmax1, Xmax2)
  
  poly1 <- data.frame(xs = seq(Xmedia2+1.5*Xdt2, Xmax2, length.out = n)) |>
    transform(
      y1 = dnorm(xs, Xmedia1, Xdt1),
      y2 = dnorm(xs, Xmedia2, Xdt2)
    ) |>
    with(data.frame(X = c(xs, rev(xs)), Y = c(y1, rev(y2))))
  
  poly2 <- data.frame(xs = seq(Xmin1, Xmedia1-1.5*Xdt1, length.out = n)) |>
    transform(
      y1 = dnorm(xs, Xmedia1, Xdt1),
      y2 = dnorm(xs, Xmedia2, Xdt2)
    ) |>
    with(data.frame(X = c(xs, rev(xs)), Y = c(y1, rev(y2))))
  
  ggplot(data.frame(X = c(Xmin, Xmax)), aes(x = X)) +
    geom_hline(yintercept = 0, colour = "grey", linewidth = 1) +
    geom_polygon(aes(X, Y), data = poly1,
                 fill = "red", alpha = 0.5) +
    geom_polygon(aes(X, Y), data = poly2,
                 fill = "blue", alpha = 0.5) +
    stat_function(fun = dnorm, 
                  args = c(Xmedia1, Xdt1), 
                  linewidth = 1, 
                  colour = "grey") +
    stat_function(fun = dnorm, 
                  args = c(Xmedia2, Xdt2), 
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
    panel.background = element_rect(fill = "lightgray", colour = NA),
  ) +
    xlim(c(Xmin, Xmax)) 
}

g1 <- graf_normal(250, 7, 253, 7)

g1

