# =============================================================================
# 04_evaluacion_pruebas.R
# Métricas fuera de muestra y pruebas formales de capacidad predictiva.
#   - Métricas: AUC, accuracy, Brier, log-loss, precisión, sensibilidad (recall),
#     F1, media geométrica de sensibilidad y especificidad (G-mean) y coeficiente
#     de Matthews (MCC), y proporción pronosticada al alza (umbral 0,5).
#   - Pesaran y Timmermann (1992): exactitud direccional vs. independencia.
#   - Diebold y Mariano (1995) en la versión de Gu, Kelly y Xiu (2020): para cada
#     semana se promedia en el corte transversal la diferencia de pérdidas entre dos
#     modelos y se contrasta la media de esa serie con errores Newey-West.
#   - Clark y West (2007): modelos anidados (logit simple dentro de los demás).
# Entrada : resultados/predicciones/predicciones_h{H}.rds
# Salidas : t07_metricas_globales, t08_metricas_por_anio, t09_metricas_por_pais,
#           t10_pesaran_timmermann, t11_diebold_mariano, t12_clark_west (h{H}),
#           f01_auc_por_anio y f04_importancia_variables
# =============================================================================
if (!exists("DIR_CODIGO")) DIR_CODIGO <- local({
  a <- commandArgs(FALSE); f <- sub("^--file=", "", a[grep("^--file=", a)])
  if (length(f) == 1) dirname(normalizePath(f)) else
    if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable() &&
        nzchar(rstudioapi::getSourceEditorContext()$path))
      dirname(normalizePath(rstudioapi::getSourceEditorContext()$path)) else getwd()
})
source(file.path(DIR_CODIGO, "00_configuracion.R"), encoding = "UTF-8")

H <- if (exists("HORIZONTE")) HORIZONTE else 1L
SUF <- if (is.na(VENTANA_MOVIL_ANIOS)) sprintf("h%d", H) else sprintf("h%d_movil%d", H, VENTANA_MOVIL_ANIOS)
pred <- readRDS(file.path(DIR_PREDICCIONES, sprintf("predicciones_%s.rds", SUF)))
MODELOS <- c("ingenuo", "logit_simple", "logit_completo", "elastic_net", "random_forest",
             "xgboost", "red_neuronal")
MODELOS <- intersect(MODELOS, unique(pred$modelo))
pred[, modelo := factor(modelo, levels = MODELOS)]
LAG_NW <- max(4L, H)       # rezagos Newey-West (al menos el horizonte del pronóstico)

## 1. Métricas ---------------------------------------------------------------------
metricas <- function(y, p) {
  yhat <- as.integer(p > 0.5)
  tp <- as.numeric(sum(yhat == 1 & y == 1)); tn <- as.numeric(sum(yhat == 0 & y == 0))
  fp <- as.numeric(sum(yhat == 1 & y == 0)); fn <- as.numeric(sum(yhat == 0 & y == 1))
  sens <- if (tp + fn > 0) tp / (tp + fn) else NA_real_
  espe <- if (tn + fp > 0) tn / (tn + fp) else NA_real_
  prec <- if (tp + fp > 0) tp / (tp + fp) else NA_real_
  list(n = length(y), prop_sube = mean(y), prop_pronostica_sube = mean(yhat),
       auc = auc_rapido(y, p), accuracy = (tp + tn) / length(y),
       g_media = if (is.finite(sens) && is.finite(espe)) sqrt(sens * espe) else NA_real_,
       mcc = { d <- sqrt((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn)); if (d > 0) (tp * tn - fp * fn) / d else 0 },
       brier = brier(y, p), log_loss = log_loss(y, p),
       precision = prec, recall = sens,
       f1 = if (is.finite(prec) && is.finite(sens) && prec + sens > 0) 2 * prec * sens / (prec + sens) else NA_real_)
}
redondear <- function(dt) {   # 4 decimales; 6 para diferencias de pérdida (son del orden de 1e-4)
  num <- names(dt)[vapply(dt, is.double, TRUE)]; fina <- intersect(num, c("dif_media", "media_f"))
  dt[, (setdiff(num, fina)) := lapply(.SD, round, 4), .SDcols = setdiff(num, fina)]
  if (length(fina)) dt[, (fina) := lapply(.SD, round, 6), .SDcols = fina]
  dt
}

t07 <- redondear(pred[, metricas(y, p), by = modelo][order(modelo)])
t08 <- redondear(pred[, metricas(y, p), by = .(anio_prueba, modelo)][order(anio_prueba, modelo)])
t09 <- redondear(pred[, metricas(y, p), by = .(pais, modelo)][order(pais, modelo)])
guardar_tabla(t07, sprintf("t07_metricas_globales_%s", SUF))
guardar_tabla(t08, sprintf("t08_metricas_por_anio_%s", SUF))
guardar_tabla(t09, sprintf("t09_metricas_por_pais_%s", SUF))
print(t07[, .(modelo, auc, accuracy, g_media, mcc, brier, log_loss)])

## 2. Pesaran y Timmermann (1992) ----------------------------------------------
pesaran_timmermann <- function(y, p) {
  x <- as.integer(p > 0.5); n <- length(y)
  P <- mean(x == y); Py <- mean(y); Px <- mean(x)
  Pe <- Py * Px + (1 - Py) * (1 - Px)
  vP <- Pe * (1 - Pe) / n
  vPe <- (2 * Py - 1)^2 * Px * (1 - Px) / n + (2 * Px - 1)^2 * Py * (1 - Py) / n +
         4 * Py * Px * (1 - Py) * (1 - Px) / n^2
  den <- vP - vPe
  s <- if (is.finite(den) && den > 0) (P - Pe) / sqrt(den) else NA_real_
  list(n = n, aciertos = P, aciertos_esperados_indep = Pe, estadistico_PT = s,
       p_valor = if (is.finite(s)) 1 - pnorm(s) else NA_real_)   # unilateral: H1 = poder predictivo
}
t10 <- rbind(pred[modelo != "ingenuo", c(list(pais = "Conjunto"), pesaran_timmermann(y, p)), by = modelo],
             pred[modelo != "ingenuo", pesaran_timmermann(y, p), by = .(modelo, pais)], use.names = TRUE)
guardar_tabla(redondear(t10), sprintf("t10_pesaran_timmermann_%s", SUF))

## 3. Diebold-Mariano con promedio de corte transversal (Gu, Kelly y Xiu, 2020) ------
ancho <- dcast(pred, semana + ticker + pais + y ~ modelo, value.var = "p")
perdida <- function(y, p, tipo) switch(tipo,
  brier = (p - y)^2, log = perdida_log_obs(y, p), cero_uno = as.numeric(as.integer(p > 0.5) != y))
dm_test <- function(d_t, lag = LAG_NW) {
  d_t <- d_t[is.finite(d_t)]
  if (length(d_t) < 20) return(list(dif_media = NA_real_, estadistico_DM = NA_real_, p_valor = NA_real_))
  fit <- lm(d_t ~ 1)
  se <- sqrt(sandwich::NeweyWest(fit, lag = lag, prewhite = FALSE, adjust = TRUE)[1, 1])
  s <- coef(fit)[[1]] / se
  list(dif_media = coef(fit)[[1]], estadistico_DM = s, p_valor = 2 * (1 - pnorm(abs(s))))
}
comparar <- function(dt, m_a, m_b, tipo) {
  # d_t > 0  <=>  el modelo b tiene menor pérdida que el modelo a (b mejora sobre a)
  serie <- dt[, .(d = mean(perdida(y, get(m_a), tipo) - perdida(y, get(m_b), tipo))), by = semana]
  c(list(modelo_base = m_a, modelo_rival = m_b, perdida = tipo, semanas = nrow(serie)), dm_test(serie$d))
}
pares <- rbind(data.table(a = "logit_simple", b = setdiff(MODELOS, c("ingenuo", "logit_simple"))),
               data.table(a = "ingenuo", b = setdiff(MODELOS, "ingenuo")),
               data.table(a = c("elastic_net", "elastic_net", "logit_completo"),
                          b = c("xgboost", "random_forest", "elastic_net")))
pares <- unique(pares[a %in% MODELOS & b %in% MODELOS])
t11 <- rbindlist(lapply(c("log", "brier", "cero_uno"), function(tp)
  rbindlist(lapply(seq_len(nrow(pares)), function(i) comparar(ancho, pares$a[i], pares$b[i], tp)))))
guardar_tabla(redondear(t11), sprintf("t11_diebold_mariano_%s", SUF))

## 4. Clark y West (2007) para modelos anidados (probabilidades, pérdida cuadrática) --
clark_west <- function(dt, m_a, m_b) {
  serie <- dt[, .(f = mean((y - get(m_a))^2 - ((y - get(m_b))^2 - (get(m_a) - get(m_b))^2))), by = semana]
  r <- dm_test(serie$f)
  list(modelo_anidado = m_a, modelo_amplio = m_b, semanas = nrow(serie), media_f = r$dif_media,
       estadistico_CW = r$estadistico_DM,
       p_valor_unilateral = if (is.finite(r$estadistico_DM)) 1 - pnorm(r$estadistico_DM) else NA_real_)
}
t12 <- rbindlist(lapply(setdiff(MODELOS, c("ingenuo", "logit_simple")),
                        function(m) clark_west(ancho, "logit_simple", m)))
guardar_tabla(redondear(t12), sprintf("t12_clark_west_%s", SUF))

## 5. Figura: AUC por año y modelo ------------------------------------------------
gg <- ggplot(t08[modelo != "ingenuo"], aes(anio_prueba, auc, colour = modelo)) +
  geom_hline(yintercept = 0.5, linetype = 2) + geom_line() + geom_point() +
  labs(x = "Año de prueba", y = "AUC fuera de muestra", colour = NULL) + theme_minimal()
ggsave(file.path(DIR_FIGURAS, sprintf("f01_auc_por_anio_%s.png", SUF)), gg, width = 7, height = 4, dpi = 200)

## 6. Figura: importancia de las variables (promedio de los años de prueba) ----------
imp_f <- file.path(DIR_TABLAS, sprintf("t05_importancia_variables_%s.csv", SUF))
if (file.exists(imp_f)) {
  imp <- fread(imp_f)
  imp_m <- imp[, .(importancia = mean(caida_auc_norm, na.rm = TRUE)), by = .(modelo, variable)]
  top <- imp_m[order(modelo, -importancia), head(.SD, 10), by = modelo]
  gg <- ggplot(top, aes(reorder(variable, importancia), importancia)) + geom_col() + coord_flip() +
    facet_wrap(~modelo, scales = "free_y") +
    labs(x = NULL, y = "Importancia normalizada (caída del AUC al fijar la variable en cero)") +
    theme_minimal()
  ggsave(file.path(DIR_FIGURAS, sprintf("f04_importancia_variables_%s.png", SUF)), gg,
         width = 10, height = 6, dpi = 200)
}
msg("Listo. Siguiente paso: 05_carteras_costos.R")
