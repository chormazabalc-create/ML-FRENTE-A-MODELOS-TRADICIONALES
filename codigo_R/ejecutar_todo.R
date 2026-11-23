# =============================================================================
# ejecutar_todo.R
# Corre el flujo completo DESPUÉS de la descarga (01_descarga_yahoo.R).
# No requiere internet. Abrir codigo_R.Rproj en RStudio y ejecutar con Source.
#
# Orden: 02 panel -> 03 modelos (h = 1) -> 04 métricas y pruebas -> 05 carteras
#        -> 06 liquidez -> 07 robustez (incluye h = 4, que repite 03 y 04).
# Para una prueba rápida del código (grillas y árboles reducidos), antes de
# ejecutar escriba en la consola:  Sys.setenv(TESIS_MODO_RAPIDO = "1")
# y para volver al modo completo:   Sys.setenv(TESIS_MODO_RAPIDO = "0")
# =============================================================================
if (!exists("DIR_CODIGO")) DIR_CODIGO <- local({
  a <- commandArgs(FALSE); f <- sub("^--file=", "", a[grep("^--file=", a)])
  if (length(f) == 1) dirname(normalizePath(f)) else
    if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable() &&
        nzchar(rstudioapi::getSourceEditorContext()$path))
      dirname(normalizePath(rstudioapi::getSourceEditorContext()$path)) else getwd()
})
inicio <- Sys.time()
pasos <- c("02_construccion_panel.R", "03_modelos_walkforward.R", "04_evaluacion_pruebas.R",
           "05_carteras_costos.R", "06_heterogeneidad_liquidez.R", "07_robustez.R")
for (s in pasos) {
  cat("\n==================== ", s, " ====================\n", sep = "")
  source(file.path(DIR_CODIGO, s), encoding = "UTF-8")
}
cat(sprintf("\nFlujo completo en %.1f minutos. Tablas en %s\n",
            as.numeric(difftime(Sys.time(), inicio, units = "mins")), DIR_TABLAS))
sink(file.path(DIR_RESULTADOS, "sesion_R.txt")); print(sessionInfo()); sink()
