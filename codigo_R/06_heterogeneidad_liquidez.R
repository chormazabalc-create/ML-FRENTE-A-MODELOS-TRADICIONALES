# =============================================================================
# 06_heterogeneidad_liquidez.R
# Hipótesis de límites al arbitraje (H3): ¿es mayor la ventaja del ML en las
# acciones ilíquidas y en los mercados menos líquidos?
#   - Terciles de liquidez formados cada semana dentro de cada país, con
#     información disponible en t: iliquidez de Amihud, proporción de días con
#     retorno cero y spread de Corwin-Schultz (T1 = más líquido, T3 = menos).
#   - Métricas por tercil y modelo; prueba de Diebold-Mariano (promedio de corte
#     transversal) de cada modelo contra el logit simple dentro de cada tercil, y
#     prueba de la diferencia de la ventaja entre T3 y T1.
#   - La misma prueba de Diebold-Mariano dentro de cada país.
#   - Carteras del quintil superior dentro de cada tercil, brutas y netas de spread.
# Entrada : resultados/predicciones/predicciones_h1.rds
# Salidas : t15_metricas_por_tercil, t16_dm_por_tercil, t16b_dm_por_pais,
#           t17_diferencia_T3_T1, t18_carteras_por_tercil, f03_ventaja_por_tercil
# =============================================================================
if (!exists("DIR_CODIGO")) DIR_CODIGO <- local({
  a <- commandArgs(FALSE); f <- sub("^--file=", "", a[grep("^--file=", a)])
  if (length(f) == 1) dirname(normalizePath(f)) else
    if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable() &&
        nzchar(rstudioapi::getSourceEditorContext()$path))
      dirname(normalizePath(rstudioapi::getSourceEditorContext()$path)) else getwd()
})
source(file.path(DIR_CODIGO, "00_configuracion.R"), encoding = "UTF-8")

pred <- readRDS(file.path(DIR_PREDICCIONES, "predicciones_h1.rds"))
MEDIDAS <- c(amihud = "amihud_63", ceros = "ceros_63", spread_cs = "cs_63")
LAG_NW <- 4L

tercil <- function(x) {
  r <- frank(x, ties.method = "first", na.last = "keep"); n <- sum(!is.na(x))
  as.integer(ceiling(3 * r / n))
}
dm_serie <- function(d) {
  d <- d[is.finite(d)]; if (length(d) < 20) return(c(NA_real_, NA_real_, NA_real_))
  fit <- lm(d ~ 1); se <- sqrt(sandwich::NeweyWest(fit, lag = LAG_NW, prewhite = FALSE)[1, 1])
  s <- coef(fit)[[1]] / se; c(coef(fit)[[1]], s, 2 * (1 - pnorm(abs(s))))
}

res_met <- list(); res_dm <- list(); res_dif <- list()
for (nm in names(MEDIDAS)) {
  v <- MEDIDAS[[nm]]
  pred[, tercil := tercil(get(v)), by = .(modelo, pais, semana)]
  base <- pred[modelo == "logit_simple", .(semana, ticker, p_base = p)]
  dd <- base[pred[!is.na(tercil)], on = .(semana, ticker)]
  res_met[[nm]] <- dd[, .(medida = nm, n = .N, auc = auc_rapido(y, p), auc_logit_simple = auc_rapido(y, p_base),
                          brier = brier(y, p), brier_logit_simple = brier(y, p_base),
                          accuracy = mean(as.integer(p > 0.5) == y)),
                      by = .(modelo, tercil)][order(modelo, tercil)]
  # Ventaja semanal en log-loss: pérdida(logit simple) - pérdida(modelo), promedio del tercil
  serie <- dd[modelo != "logit_simple", .(d = mean(perdida_log_obs(y, p_base) - perdida_log_obs(y, p))),
              by = .(modelo, tercil, semana)]
  res_dm[[nm]] <- serie[, { r <- dm_serie(d); list(medida = nm, semanas = .N, ventaja_media = r[1],
                                                    estadistico_DM = r[2], p_valor = r[3]) },
                        by = .(modelo, tercil)][order(modelo, tercil)]
  ancho <- dcast(serie[tercil %in% c(1, 3)], modelo + semana ~ tercil, value.var = "d")
  setnames(ancho, c("1", "3"), c("T1", "T3"))
  res_dif[[nm]] <- ancho[, { r <- dm_serie(T3 - T1); list(medida = nm, semanas = sum(is.finite(T3 - T1)),
                     dif_ventaja_T3_menos_T1 = r[1], estadistico = r[2], p_valor = r[3]) }, by = modelo]
}
t15 <- rbindlist(res_met); t16 <- rbindlist(res_dm); t17 <- rbindlist(res_dif)
redondear <- function(dt) { x <- names(dt)[vapply(dt, is.double, TRUE)]; dt[, (x) := lapply(.SD, round, 6), .SDcols = x]; dt }
guardar_tabla(redondear(t15), "t15_metricas_por_tercil_liquidez")
guardar_tabla(redondear(t16), "t16_dm_por_tercil_liquidez")
guardar_tabla(redondear(t17), "t17_diferencia_ventaja_T3_T1")
print(t17)

## Ventaja del ML por país (mercados más y menos líquidos del bloque) -----------
# Misma prueba de Diebold-Mariano (log-loss, promedio de corte transversal semanal
# con Newey-West), pero dentro de cada país.
base <- pred[modelo == "logit_simple", .(semana, ticker, p_base = p)]
dd <- base[pred[modelo != "logit_simple"], on = .(semana, ticker)]
serie_p <- dd[, .(d = mean(perdida_log_obs(y, p_base) - perdida_log_obs(y, p))), by = .(modelo, pais, semana)]
t16b <- serie_p[, { r <- dm_serie(d); list(semanas = .N, ventaja_media = r[1], estadistico_DM = r[2],
                                             p_valor = r[3]) }, by = .(modelo, pais)][order(modelo, pais)]
guardar_tabla(redondear(t16b), "t16b_dm_por_pais")

## Carteras por tercil de Amihud (quintil superior dentro del tercil) ----------
pred[, tercil := tercil(amihud_63), by = .(modelo, pais, semana)]
cart <- pred[!is.na(tercil) & modelo != "ingenuo"]
setorder(cart, modelo, pais, tercil, semana, -p)
cart[, k := pmax(1L, floor(CUANTIL_CARTERA * .N)), by = .(modelo, pais, tercil, semana)]
cart[, sel := seq_len(.N) <= k, by = .(modelo, pais, tercil, semana)]
sem <- cart[, .(r_c = mean(ret_fwd[sel]), r_b = mean(ret_fwd), spread = mean(cs_63[sel], na.rm = TRUE)),
            by = .(modelo, pais, tercil, semana)]
sem <- sem[, .(r_c = mean(r_c), r_b = mean(r_b), spread = mean(spread, na.rm = TRUE)), by = .(modelo, tercil, semana)]
t18 <- sem[, .(activo_anual_bruto = 52 * mean(r_c - r_b),
               spread_cs_medio = mean(spread, na.rm = TRUE),
               t_activo = mean(r_c - r_b) / (sd(r_c - r_b) / sqrt(.N))), by = .(modelo, tercil)][order(modelo, tercil)]
guardar_tabla(redondear(t18), "t18_carteras_por_tercil_amihud")

gg <- ggplot(t16[medida == "amihud"], aes(factor(tercil), ventaja_media, fill = modelo)) +
  geom_col(position = "dodge") + geom_hline(yintercept = 0) +
  labs(x = "Tercil de iliquidez de Amihud (1 = más líquido)",
       y = "Ventaja media en log-loss vs. logit simple", fill = NULL) + theme_minimal()
ggsave(file.path(DIR_FIGURAS, "f03_ventaja_por_tercil_amihud.png"), gg, width = 8, height = 4, dpi = 200)
msg("Listo. Siguiente paso: 07_robustez.R")
