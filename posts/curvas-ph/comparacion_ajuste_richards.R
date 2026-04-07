library(dplyr)
library(readr)
library(ggplot2)

datos <- read_csv2("curvas-pH-Danisco.csv",
                   locale = locale(encoding = "ISO-8859-1"),
                   show_col_types = FALSE) |>
  mutate(cepa = factor(cepa), temperatura = factor(temperatura))

T_MAX <- 14
datos_act <- datos |> filter(hora <= T_MAX)

richards <- function(t, A, K, r, t0, nu) {
  K + (A - K) * (1 + nu * exp(r * (t - t0)))^(-1/nu)
}
gompertz <- function(t, A, K, r, t0) {
  K + (A - K) * exp(-exp(r * (t - t0)))
}

vmax_richards <- function(A, K, r, nu) {
  r * (A - K) / ((1 + nu)^((1 + nu)/nu) * exp(-1))
}
vmax_gompertz <- function(A, K, r) {
  r * (A - K) / exp(1)
}
inflexion_richards <- function(nu) {
  1 / (1 + nu)^(1/nu)
}

ajustar_richards <- function(df) {
  A_ini  <- max(df$pH)
  K_ini  <- min(df$pH)
  t0_ini <- df$hora[which.min(diff(df$pH))]
  modelo <- tryCatch(
    nls(pH ~ richards(hora, A, K, r, t0, nu),
        data = df,
        start = list(A = A_ini, K = K_ini, r = 0.5, t0 = t0_ini, nu = 1.0),
        algorithm = "port",
        lower = list(A = 5.0, K = 3.5, r = 0.01, t0 = 0,     nu = 0.01),
        upper = list(A = 7.5, K = 6.5, r = 5.0,  t0 = T_MAX, nu = 10.0),
        control = nls.control(maxiter = 1000)),
    error = function(e) NULL)
  if (is.null(modelo)) return(tibble())
  A  <- coef(modelo)["A"];  K  <- coef(modelo)["K"]
  r  <- coef(modelo)["r"];  t0 <- coef(modelo)["t0"]
  nu <- coef(modelo)["nu"]
  vm   <- vmax_richards(A, K, r, nu)
  infl <- inflexion_richards(nu)
  R2   <- 1 - sum(residuals(modelo)^2) / sum((df$pH - mean(df$pH))^2)
  tibble(A=round(A,3), K=round(K,3), r=round(r,4), t0=round(t0,3),
         nu=round(nu,3), Vmax=round(vm,4),
         inflexion_pct=round(infl*100,1), R2=round(R2,4))
}

resultados <- datos_act |>
  group_by(cepa, temperatura) |>
  reframe(ajustar_richards(pick(everything())))

# cat("=== PARÁMETROS RICHARDS ===\n")
# print(resultados)
# write.csv2(resultados, "parametros_richards.csv", row.names = FALSE)

# Predicciones Richards y Gompertz para comparar visualmente
ajustar_gomp <- function(df) {
  A_ini  <- max(df$pH); K_ini <- min(df$pH)
  t0_ini <- df$hora[which.min(diff(df$pH))]
  tryCatch(
    nls(pH ~ gompertz(hora, A, K, r, t0), data = df,
        start = list(A=A_ini, K=K_ini, r=0.5, t0=t0_ini),
        algorithm="port",
        lower=list(A=5.0,K=3.5,r=0.01,t0=0),
        upper=list(A=7.5,K=6.5,r=5.0,t0=T_MAX),
        control=nls.control(maxiter=500)), error=function(e) NULL)
}
ajustar_rich <- function(df) {
  A_ini  <- max(df$pH); K_ini <- min(df$pH)
  t0_ini <- df$hora[which.min(diff(df$pH))]
  tryCatch(
    nls(pH ~ richards(hora, A, K, r, t0, nu), data = df,
        start = list(A=A_ini, K=K_ini, r=0.5, t0=t0_ini, nu=1.0),
        algorithm="port",
        lower=list(A=5.0,K=3.5,r=0.01,t0=0,nu=0.01),
        upper=list(A=7.5,K=6.5,r=5.0,t0=T_MAX,nu=10.0),
        control=nls.control(maxiter=1000)), error=function(e) NULL)
}

pred_df <- do.call(rbind, lapply(split(datos_act,
    paste(datos_act$cepa, datos_act$temperatura)), function(df) {
  mg <- ajustar_gomp(df)
  mr <- ajustar_rich(df)
  t_seq <- seq(0, T_MAX, by = 0.05)
  rbind(
    if (!is.null(mg)) data.frame(hora=t_seq, pH=predict(mg, data.frame(hora=t_seq)),
      modelo="Gompertz", cepa=df$cepa[1], temperatura=df$temperatura[1]),
    if (!is.null(mr)) data.frame(hora=t_seq, pH=predict(mr, data.frame(hora=t_seq)),
      modelo="Richards", cepa=df$cepa[1], temperatura=df$temperatura[1])
  )
}))

colores_temp <- c("25"="#E63946", "32"="#457B9D", "37"="#2D6A4F", "40"="#E9C46A")
lty_modelo   <- c("Gompertz"="dashed", "Richards"="solid")

# Gráfico MA
p_MA <- ggplot() +
  geom_point(data = datos_act |> filter(cepa=="MA"),
             aes(x=hora, y=pH, color=temperatura), size=1.2, alpha=0.5) +
  geom_line(data = pred_df |> filter(cepa=="MA"),
            aes(x=hora, y=pH, color=temperatura, linetype=modelo), linewidth=0.9) +
  scale_color_manual(values=colores_temp) +
  scale_linetype_manual(values=lty_modelo) +
  scale_x_continuous(breaks=seq(0,14,2)) +
  scale_y_continuous(limits=c(4.0,7.0), breaks=seq(4.0,7.0,0.2)) +
  labs(title="Cepa MA — Gompertz vs Richards (t \u2264 14 h)",
       x="Tiempo (horas)", y="pH", color="T\u00aa (\u00b0C)", linetype="Modelo") +
  theme_bw(base_size=10) +
  theme(legend.position="right",
        strip.background=element_rect(fill="#D6E4F0",color=NA))

# Gráfico TA050
p_TA <- ggplot() +
  geom_point(data = datos_act |> filter(cepa=="TA050"),
             aes(x=hora, y=pH, color=temperatura), size=1.2, alpha=0.5) +
  geom_line(data = pred_df |> filter(cepa=="TA050"),
            aes(x=hora, y=pH, color=temperatura, linetype=modelo), linewidth=0.9) +
  scale_color_manual(values=colores_temp) +
  scale_linetype_manual(values=lty_modelo) +
  scale_x_continuous(breaks=seq(0,14,2)) +
  scale_y_continuous(limits=c(4.0,7.5), breaks=seq(4.0,7.5,0.2)) +
  labs(title="Cepa TA050 — Gompertz vs Richards (t \u2264 14 h)",
       x="Tiempo (horas)", y="pH", color="T\u00aa (\u00b0C)", linetype="Modelo") +
  theme_bw(base_size=10) +
  theme(legend.position="right",
        strip.background=element_rect(fill="#D6E4F0",color=NA))

p_MA + p_TA

# ggsave("/tmp/ajuste_MA.png",   p_MA, width=7, height=4, dpi=150)
# ggsave("/tmp/ajuste_TA050.png", p_TA, width=7, height=4, dpi=150)
# cat("Gráficos guardados\n")
