# =============================================================================
# 07_robustez.R
# Pruebas de robustez:
#   (a) submuestras de estrés: 2020 (pandemia) y 2022 (alza de tasas), y el resto;
#   (b) excluyendo las semanas con retorno exactamente cero en t+1 (precios sin
#       transacción), que en acciones ilíquidas pueden inflar la "predictibilidad";
#   (c) horizonte de 4 semanas (vuelve a correr 03 y 04 con HORIZONTE = 4; tarda);
#   (d) opcional (CORRER_MOVIL <- TRUE): ventana de entrenamiento móvil de 5 años.
# Entrada : resultados/predicciones/predicciones_h1.rds
# Salidas : t19_robustez_submuestras, t20_robustez_sin_retornos_cero y,
#           si CORRER_H4 = TRUE, las tablas t07-t12 con sufijo _h4.
# =============================================================================
if (!exists("DIR_CODIGO")) DIR_CODIGO <- local({
  a <- commandArgs(FALSE); f <- sub("^--file=", "", a[grep("^--file=", a)])
  if (length(f) == 1) dirname(normalizePath(f)) else
    if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable() &&
        nzchar(rstudioapi::getSourceEditorContext()$path))
      dirname(normalizePath(rstudioapi::getSourceEditorContext()$path)) else getwd()
})
source(file.path(DIR_CODIGO, "00_configuracion.R"), encoding = "UTF-8")
CORRER_H4 <- if (exists("CORRER_H4")) CORRER_H4 else TRUE
CORRER_MOVIL <- if (exists("CORRER_MOVIL")) CORRER_MOVIL else FALSE   # ventana móvil de 5 años (opcional)

pred <- readRDS(file.path(DIR_PREDICCIONES, "predicciones_h1.rds"))
resumen <- function(y, p) list(n = length(y), auc = auc_rapido(y, p), accuracy = mean(as.integer(p > 0.5) == y),
                               brier = brier(y, p), log_loss = log_loss(y, p))
pred[, periodo := fcase(anio_prueba == 2020, "2020 (pandemia)", anio_prueba == 2022, "2022 (alza de tasas)",
                        default = "resto de los años")]
t19 <- pred[, resumen(y, p), by = .(periodo, modelo)][order(periodo, modelo)]
t20 <- pred[ret_fwd != 0, resumen(y, p), by = modelo]
t20 <- merge(t20, pred[, .(prop_ret_cero = mean(ret_fwd == 0)), by = modelo], by = "modelo")
redondear <- function(dt) { x <- names(dt)[vapply(dt, is.double, TRUE)]; dt[, (x) := lapply(.SD, round, 4), .SDcols = x]; dt }
guardar_tabla(redondear(t19), "t19_robustez_submuestras")
guardar_tabla(redondear(t20), "t20_robustez_sin_retornos_cero")
print(t20)

if (isTRUE(CORRER_H4)) {
  msg("Robustez: horizonte de 4 semanas (vuelve a estimar todos los modelos)...")
  HORIZONTE <- 4L
  source(file.path(DIR_CODIGO, "03_modelos_walkforward.R"), encoding = "UTF-8")
  source(file.path(DIR_CODIGO, "04_evaluacion_pruebas.R"), encoding = "UTF-8")
  rm(HORIZONTE)
}
if (isTRUE(CORRER_MOVIL)) {
  msg("Robustez: ventana de entrenamiento móvil de 5 años (vuelve a estimar todos los modelos)...")
  VENTANA_MOVIL_ANIOS <- 5L
  source(file.path(DIR_CODIGO, "03_modelos_walkforward.R"), encoding = "UTF-8")
  source(file.path(DIR_CODIGO, "04_evaluacion_pruebas.R"), encoding = "UTF-8")
  VENTANA_MOVIL_ANIOS <- NA
}
msg("Listo. Revise resultados/tablas y resultados/figuras.")
