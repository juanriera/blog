library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(gt)

# ── Leer datos ────────────────────────────────────────────────────────────────
datos <- read_csv2("curvas-pH-Danisco.csv",
                   locale = locale(encoding = "ISO-8859-1"),
                   show_col_types = FALSE) |>
  mutate(cepa = factor(cepa), temperatura = factor(temperatura))

# ── Limitar a zona de acidificación activa (hasta 14 h) ──────────────────────
T_MAX <- 14
datos_act <- datos |> filter(hora <= T_MAX)

# ── Modelos ───────────────────────────────────────────────────────────────────

# Gompertz invertido
gompertz <- function(t, A, K, r, t0) {
  K + (A - K) * exp(-exp(r * (t - t0)))
}

# Logística estándar (punto de inflexión al 50% de amplitud)
logistica <- function(t, A, K, r, t0) {
  K + (A - K) / (1 + exp(r * (t - t0)))
}

# Richards generalizada (nu libre: nu=1 → logística, nu→0 → Gompertz)
richards <- function(t, A, K, r, t0, nu) {
  K + (A - K) * (1 + nu * exp(r * (t - t0)))^(-1/nu)
}

# ── Función de ajuste para un grupo ──────────────────────────────────────────
ajustar_grupo <- function(df) {
  A_ini  <- max(df$pH)
  K_ini  <- min(df$pH)
  t0_ini <- df$hora[which.min(diff(df$pH))]

  # Gompertz
  m_gomp <- tryCatch(
    nls(pH ~ gompertz(hora, A, K, r, t0),
        data = df,
        start = list(A = A_ini, K = K_ini, r = 0.5, t0 = t0_ini),
        algorithm = "port",
        lower = list(A = 5.0, K = 3.5, r = 0.01, t0 = 0),
        upper = list(A = 7.5, K = 6.5, r = 5.0, t0 = T_MAX),
        control = nls.control(maxiter = 500)),
    error = function(e) NULL)

  # Logística
  m_log <- tryCatch(
    nls(pH ~ logistica(hora, A, K, r, t0),
        data = df,
        start = list(A = A_ini, K = K_ini, r = 0.5, t0 = t0_ini),
        algorithm = "port",
        lower = list(A = 5.0, K = 3.5, r = 0.01, t0 = 0),
        upper = list(A = 7.5, K = 6.5, r = 5.0, t0 = T_MAX),
        control = nls.control(maxiter = 500)),
    error = function(e) NULL)

  # Richards
  m_rich <- tryCatch(
    nls(pH ~ richards(hora, A, K, r, t0, nu),
        data = df,
        start = list(A = A_ini, K = K_ini, r = 0.5, t0 = t0_ini, nu = 1.0),
        algorithm = "port",
        lower = list(A = 5.0, K = 3.5, r = 0.01, t0 = 0,     nu = 0.01),
        upper = list(A = 7.5, K = 6.5, r = 5.0,  t0 = T_MAX, nu = 10.0),
        control = nls.control(maxiter = 1000)),
    error = function(e) NULL)

  calc_stats <- function(m, nombre, np) {
    if (is.null(m)) return(tibble(modelo = nombre, AIC = NA, BIC = NA, R2 = NA, K = NA, r = NA))
    n   <- nrow(df)
    rss <- sum(residuals(m)^2)
    tss <- sum((df$pH - mean(df$pH))^2)
    R2  <- 1 - rss / tss
    # AIC y BIC manuales (nls no los calcula directamente)
    sigma2 <- rss / (n - np)
    loglik <- -n/2 * log(2 * pi * sigma2) - rss / (2 * sigma2)
    AIC_m  <- -2 * loglik + 2 * np
    BIC_m  <- -2 * loglik + log(n) * np
    tibble(
      modelo = nombre,
      AIC    = round(AIC_m, 2),
      BIC    = round(BIC_m, 2),
      R2     = round(R2, 4),
      K      = round(coef(m)["K"], 3),
      r      = round(coef(m)["r"], 4)
    )
  }

  bind_rows(
    calc_stats(m_gomp, "Gompertz",  4),
    calc_stats(m_log,  "Logistica", 4),
    calc_stats(m_rich, "Richards",  5)
  )
}

# ── Aplicar por cepa × temperatura ───────────────────────────────────────────
resultados <- datos_act |>
  group_by(cepa, temperatura) |>
  reframe(ajustar_grupo(pick(everything())))

cat("\n=== COMPARACIÓN DE MODELOS (datos hasta", T_MAX, "h) ===\n")
resultados |> 
  gt()

resultados |>
  select(cepa, temperatura, modelo, AIC) %>%
  pivot_wider(
    names_from = modelo,
    values_from = AIC
  ) |>
  gt() |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = Richards)
  )


# ── Gráfico: AIC por modelo, cepa y temperatura ───────────────────────────────
res_ok <- resultados |> filter(!is.na(AIC))

ggplot(res_ok, aes(x = modelo, y = AIC, fill = modelo)) +
  geom_col(alpha = 0.85, width = 0.6) +
  geom_text(aes(label = round(AIC, 1)), vjust = -0.4, size = 3) +
  facet_grid(cepa ~ temperatura, labeller = label_both) +
  scale_fill_manual(values = c(
    "Gompertz"  = "#2E75B6",
    "Logistica" = "#C00000",
    "Richards"  = "#70AD47"
  )) +
  labs(title = "Comparación de modelos — AIC por cepa y temperatura",
       subtitle = paste("Datos limitados a t \u2264", T_MAX, "h"),
       x = NULL, y = "AIC", fill = NULL) +
  theme_bw(base_size = 10) +
  theme(legend.position = "bottom",
        panel.grid.major.x = element_blank(),
        strip.background = element_rect(fill = "#D6E4F0", color = NA),
        strip.text = element_text(face = "bold"))
