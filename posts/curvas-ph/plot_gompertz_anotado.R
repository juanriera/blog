library(ggplot2)

# ── Parámetros de dos curvas representativas ─────────────────────────────────
# Mismo A, K y pH inicial — solo varía r
# t0 se calcula para que gompertz(0, A, K, r, t0) = pH_ini

A       <- 6.88
K       <- 4.36
pH_ini  <- 6.75   # pH en t = 0, igual para ambas curvas

# t0 = -log(-log((pH_ini - K) / (A - K))) / r
calc_t0 <- function(A, K, r, pH_ini) {
  -log(-log((pH_ini - K) / (A - K))) / r
}

curvas <- list(
  list(label = "r alto", A = A, K = K, r = 0.58,
       t0 = calc_t0(A, K, 0.58, pH_ini), color = "#2E75B6"),
  list(label = "r bajo",  A = A, K = K, r = 0.25,
       t0 = calc_t0(A, K, 0.25, pH_ini), color = "#C00000")
)

# ── Función de Gompertz ───────────────────────────────────────────────────────
gompertz <- function(t, A, K, r, t0) {
  K + (A - K) * exp(-exp(r * (t - t0)))
}

# ── Parámetros derivados ──────────────────────────────────────────────────────
vmax  <- function(A, K, r) r * (A - K) / exp(1)
ph_t0 <- function(A, K)    K + (A - K) / exp(1)   # pH en el punto de inflexión

# ── Datos para las curvas ─────────────────────────────────────────────────────
t_seq <- seq(0, 22, by = 0.05)

df <- do.call(rbind, lapply(curvas, function(cv) {
  data.frame(
    hora  = t_seq,
    pH    = gompertz(t_seq, cv$A, cv$K, cv$r, cv$t0),
    label = cv$label,
    color = cv$color
  )
}))

# ── Puntos de anotación por curva ─────────────────────────────────────────────
anot <- do.call(rbind, lapply(curvas, function(cv) {
  vm  <- vmax(cv$A, cv$K, cv$r)
  pht <- ph_t0(cv$A, cv$K)
  data.frame(
    label  = cv$label,
    color  = cv$color,
    t0     = cv$t0,
    ph_t0  = pht,
    vmax   = vm,
    A      = cv$A,
    K      = cv$K,
    r      = cv$r
  )
}))

# ── Segmentos para la tangente en t0 ─────────────────────────────────────────
# La tangente tiene pendiente -Vmax (negativa porque el pH baja)
# y pasa por (t0, pH(t0))
tangente <- do.call(rbind, lapply(curvas, function(cv) {
  vm  <- vmax(cv$A, cv$K, cv$r)
  pht <- ph_t0(cv$A, cv$K)
  dt  <- 1.8  # extensión de la tangente a cada lado
  data.frame(
    label = cv$label,
    color = cv$color,
    x     = cv$t0 - dt,
    xend  = cv$t0 + dt,
    y     = pht + vm * dt,
    yend  = pht - vm * dt
  )
}))

# ── Etiquetas de texto ────────────────────────────────────────────────────────
etiquetas <- do.call(rbind, lapply(curvas, function(cv) {
  vm  <- vmax(cv$A, cv$K, cv$r)
  pht <- ph_t0(cv$A, cv$K)
  data.frame(
    label = cv$label,
    color = cv$color,
    # Etiqueta A (asíntota superior) — a la izquierda
    t_A   = 0.4,
    ph_A  = cv$A + 0.06,
    txt_A = paste0("A = ", cv$A),
    # Etiqueta K (asíntota inferior) — a la derecha
    t_K   = 19.5,
    ph_K  = cv$K - 0.09,
    txt_K = paste0("K = ", cv$K),
    # Etiqueta t0
    t_t0      = ifelse(cv$label == "r alto",  6.0, 13.0),
    ph_t0_lbl = ifelse(cv$label == "r alto",  5.2,  5.2),
    txt_t0 = paste0("t0 = ", round(cv$t0, 3), " h"),
    # Etiqueta Vmax
    t_vm  = ifelse(cv$label == "r alto",  5.2, 11.2),
    ph_vm = ifelse(cv$label == "r alto",  5.6,  5.6),
    txt_vm = paste0("Vmax = ", round(vm, 3), " pH/h"),
    # Etiqueta r (junto al nombre de la curva)
    t_r   = ifelse(cv$label == "r alto", 0.8, 5),
    ph_r  = ifelse(cv$label == "r alto", 6.2, gompertz(1.0, cv$A, cv$K, cv$r, cv$t0) - 0.1),
    txt_r = paste0(cv$label, "\nr = ", cv$r, " h\u207b\u00b9")
  )
}))

# ── Gráfico ───────────────────────────────────────────────────────────────────
ggplot() +

  # Líneas de asíntota A y K (una sola, compartida porque A y K son iguales)
  geom_hline(yintercept = curvas[[1]]$A, linetype = "dotted", color = "gray50", linewidth = 0.5) +
  geom_hline(yintercept = curvas[[1]]$K, linetype = "dotted", color = "gray50", linewidth = 0.5) +

  # Curvas principales
  geom_line(data = df,
            aes(x = hora, y = pH, color = label),
            linewidth = 1.1) +

  # Tangentes en t0
  geom_segment(data = tangente,
               aes(x = x, xend = xend, y = y, yend = yend, color = label),
               linetype = "dashed", linewidth = 0.6) +

  # Puntos en t0
  geom_point(data = anot,
             aes(x = t0, y = ph_t0, color = label),
             size = 3, shape = 21, fill = "white", stroke = 1.5) +

  # Segmentos verticales hasta el eje X para t0
  geom_segment(data = anot,
               aes(x = t0, xend = t0, y = K - 0.05, yend = ph_t0, color = label),
               linetype = "dotted", linewidth = 0.5) +

  # Etiquetas de texto — A y K (solo una vez, en gris)
  geom_text(data = etiquetas[etiquetas$label == "r alto", ],
            aes(x = t_A, y = ph_A, label = txt_A),
            hjust = 0, size = 3.2, color = "gray40") +
  geom_text(data = etiquetas[etiquetas$label == "r alto", ],
            aes(x = t_K, y = ph_K, label = txt_K),
            hjust = 1, size = 3.2, color = "gray40") +

  # Etiquetas t0 por curva
  geom_text(data = etiquetas,
            aes(x = t_t0, y = ph_t0_lbl, label = txt_t0, color = label),
            hjust = 0, size = 3.0) +

  # Etiquetas Vmax por curva
  geom_text(data = etiquetas,
            aes(x = t_vm, y = ph_vm, label = txt_vm, color = label),
            hjust = 0, size = 3.0) +

  # Etiquetas r/nombre de curva
  geom_text(data = etiquetas,
            aes(x = t_r, y = ph_r, label = txt_r, color = label),
            hjust = 0, size = 3.0, lineheight = 0.9) +

  scale_color_manual(values = setNames(
    sapply(curvas, `[[`, "color"),
    sapply(curvas, `[[`, "label")
  )) +
  scale_x_continuous(breaks = seq(0, 22, by = 2), limits = c(0, 22)) +
  scale_y_continuous(breaks = seq(4.0, 7.2, by = 0.2), limits = c(4.0, 7.2)) +
  labs(
    title    = "Curva de acidificación — modelo de Gompertz",
    subtitle = expression(pH(t) == K + (A - K) %.% exp(-exp(r %.% (t - t[0])))),
    x        = "Tiempo (horas)",
    y        = "pH",
    color    = NULL
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position  = "none",
    plot.subtitle    = element_text(color = "gray40", size = 9),
    panel.grid.minor = element_blank()
  )
# ggsave("curva_gompertz_anotada.png", width = 9, height = 5.5, dpi = 150)
# cat("Guardado: curva_gompertz_anotada.png\n")
