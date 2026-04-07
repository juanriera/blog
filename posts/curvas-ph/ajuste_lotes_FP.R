# original en zip: logistica_lotes.R
# sesion claude: sesion-claude-2026-04-04-Artículo LinkedIn
# zip: sesion_gompertz_queso.zip
#
library(dplyr)
library(readr)
library(ggplot2)
library(lubridate)
library(knitr)

# ── Leer datos ────────────────────────────────────────────────────────────────
datos_raw <- read_csv2("curvas_acidificacion.csv",
                       locale = locale(encoding = "ISO-8859-1"),
                       show_col_types = FALSE)

# ── Calcular tiempo en horas desde adición de fermentos ──────────────────────
datos <- datos_raw |>
  filter(!is.na(pH), !is.na(hora_control)) |>
  mutate(
    hora_control = dmy_hm(hora_control),
    lote = factor(lote)
  ) |>
  group_by(lote) |>
  mutate(
    t0_ref = min(hora_control[punto_control == "fermentos"], na.rm = TRUE),
    hora   = as.numeric(difftime(hora_control, t0_ref, units = "hours"))
  ) |>
  filter(hora >= 0) |>
  ungroup()

# cat("Datos disponibles por lote:\n")
# print(datos |> count(lote))

# ── Modelos ───────────────────────────────────────────────────────────────────
gompertz  <- function(t, A, K, r, t0) K + (A-K)*exp(-exp(r*(t-t0)))
logistica <- function(t, A, K, r, t0) K + (A-K)/(1 + exp(r*(t-t0)))

ajustar <- function(df, modelo_fn, np) {
  A_ini  <- max(df$pH, na.rm=TRUE)
  K_ini  <- min(df$pH, na.rm=TRUE)
  t0_ini <- df$hora[which.min(diff(df$pH))]
  tryCatch(
    nls(pH ~ modelo_fn(hora, A, K, r, t0),
        data = df,
        start = list(A=A_ini, K=K_ini, r=0.5, t0=t0_ini),
        algorithm = "port",
        lower = list(A=5.0, K=3.5, r=0.001, t0=0),
        upper = list(A=7.5, K=7.0, r=1.0,   t0=max(df$hora)),
        control = nls.control(maxiter=500, tol=1e-7)),
    error = function(e) { message("Error lote ", df$lote[1], ": ", e$message); NULL }
  )
}

calc_stats <- function(m, nombre, df) {
  if (is.null(m)) return(tibble(modelo=nombre, A=NA, K=NA, r=NA, t0=NA, Vmax=NA, R2=NA, AIC=NA))
  cf <- coef(m)
  A <- cf["A"]; K <- cf["K"]; r <- cf["r"]; t0 <- cf["t0"]
  Vmax <- r*(A-K)/exp(1)
  n    <- nrow(df)
  rss  <- sum(residuals(m)^2)
  tss  <- sum((df$pH - mean(df$pH))^2)
  R2   <- 1 - rss/tss
  np   <- length(cf)
  s2   <- rss/(n-np)
  ll   <- -n/2*log(2*pi*s2) - rss/(2*s2)
  AIC  <- -2*ll + 2*np
  tibble(modelo=nombre,
         A=round(A,3), K=round(K,3), r=round(r,4), t0=round(t0,2),
         Vmax=round(Vmax,4), R2=round(R2,4), AIC=round(AIC,2))
}

# ── Ajustar por lote ──────────────────────────────────────────────────────────
resultados <- datos |>
  group_by(lote) |>
  reframe({
    df <- pick(everything())
    mg <- ajustar(df, gompertz,  4)
    ml <- ajustar(df, logistica, 4)
    bind_rows(
      calc_stats(mg, "Gompertz",  df),
      calc_stats(ml, "Logistica", df)
    )
  })

cat("\n=== COMPARACIÓN GOMPERTZ vs LOGÍSTICA — 4 LOTES FP ===\n")
print(resultados, n=20)
# write.csv2(resultados, "/tmp/comparacion_lotes_FP.csv", row.names=FALSE)

# ── Predicciones ──────────────────────────────────────────────────────────────
pred_df <- datos |>
  group_by(lote) |>
  reframe({
    df    <- pick(everything())
    t_seq <- seq(0, max(df$hora), by=0.1)
    mg <- ajustar(df, gompertz,  4)
    ml <- ajustar(df, logistica, 4)
    bind_rows(
      if (!is.null(mg)) data.frame(hora=t_seq,
        pH=predict(mg, data.frame(hora=t_seq)), modelo="Gompertz"),
      if (!is.null(ml)) data.frame(hora=t_seq,
        pH=predict(ml, data.frame(hora=t_seq)), modelo="Logistica")
    )
  })

# ── Gráfico ───────────────────────────────────────────────────────────────────
ggplot() +
  geom_point(data = datos,
             aes(x=hora, y=pH),
             size=2, shape=21, fill="white", color="gray30", stroke=1) +
  geom_line(data = pred_df,
            aes(x=hora, y=pH, color=modelo, linetype=modelo),
            linewidth=0.9) +
  scale_color_manual(values=c("Gompertz"="#2E75B6", "Logistica"="#C00000")) +
  scale_linetype_manual(values=c("Gompertz"="dashed", "Logistica"="solid")) +
  scale_y_continuous(limits=c(4.5,7.2), breaks=seq(4.5,7.0,0.5)) +
  facet_wrap(~lote, ncol=2, labeller=label_both) +
  labs(title="Ajuste Gompertz vs Logística — datos reales FP",
       x="Tiempo desde adición de fermentos (horas)",
       y="pH", color="Modelo", linetype="Modelo") +
  theme_bw(base_size=10) +
  theme(legend.position="bottom",
        strip.background=element_rect(fill="#D6E4F0", color=NA),
        strip.text=element_text(face="bold"),
        panel.grid.minor=element_blank())

#ggsave("/tmp/ajuste_lotes_FP.png", width=8, height=6, dpi=150)
#cat("Gráfico guardado\n")
