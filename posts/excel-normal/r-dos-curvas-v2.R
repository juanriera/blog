library(ggplot2)
library(ggh4x)

# https://stackoverflow.com/questions/75502312/in-ggplot-how-to-fill-area-between-two-normal-curves/
#
graf_normal <- function(Xmedia1, Xdt1, Xmedia2, Xdt2, n = 101) {
  x1 <- Xmedia1 + 4 * Xdt1 * seq(-1, 1, length.out = n)
  x2 <- Xmedia2 + 4 * Xdt2 * seq(-1, 1, length.out = n)
  
  dat <- data.frame(
    x = union(x1, x2)
  )
  dat$y1 <- dnorm(dat$x, Xmedia1, Xdt1)
  dat$y2 <- dnorm(dat$x, Xmedia2, Xdt2)
  
  Ymax1 <- dnorm(Xmedia1, Xmedia1, Xdt1)
  Ymax2 <- dnorm(Xmedia2, Xmedia2, Xdt2)
  
  ggplot(dat, aes(x)) +
    geom_hline(yintercept = 0, colour = "grey", linewidth = 1) +
    ggh4x::stat_difference(
      data = ~ subset(.x, x >= Xmedia2 + 1.5 * Xdt2),
      aes(ymin = y1, ymax = y2)
    ) +
    annotate(
      geom = "segment",
      x = c(Xmedia1, Xmedia2), y = 0,
      xend = c(Xmedia1, Xmedia2), yend = c(Ymax1, Ymax2),
      linetype = "dashed",
      linewidth = 1,
      colour = c("grey", "black")
    ) +
    geom_line(aes(y = y1), linewidth = 1, colour = "grey") +
    geom_line(aes(y = y2), linewidth = 1, colour = "black") +
    scale_fill_manual(values = c(scales::alpha("red", .5), "transparent")) +
    theme(
      line = element_blank(),
      axis.line.y = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      legend.position = "none",
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "lightgray", colour = NA),
    )
}

graf_normal(250, 7, 253, 7)
