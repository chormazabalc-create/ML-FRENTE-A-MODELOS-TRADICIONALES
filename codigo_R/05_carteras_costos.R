# =============================================================================
# 05_carteras_costos.R
# Traducción económica: cada semana y en cada país se compra el quintil de acciones
# con mayor probabilidad predicha de subir (solo posiciones largas, igual
# ponderación) y se compara con la cartera equiponderada de todas las acciones
# elegibles del país. La cartera del conjunto promedia los cuatro países (retornos en
# moneda local; el retorno activo, cartera menos benchmark, neutraliza en buena
# medida el efecto cambiario).
# Costos: escenarios de 0, 50, 100 y 150 pb ida y vuelta aplicados a la rotación,
# más un escenario con el spread estimado de Corwin y Schultz de cada acción.
# Costo de equilibrio = retorno activo bruto medio / rotación media.
# Entrada : resultados/predicciones/predicciones_h1.rds
# Salidas : t13_carteras_resumen, t14_carteras_por_pais, f02_retorno_acumulado
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
pred <- pred[modelo != "ingenuo"]

## 1. Pesos semanales por país y modelo ----------------------------------------
setorder(pred, modelo, pais, semana, -p)
pred[, n_sem := .N, by = .(modelo, pais, semana)]
pred[, k := pmax(1L, floor(CUANTIL_CARTERA * n_sem))]
pred[, en_cartera := seq_len(.N) <= k, by = .(modelo, pais, semana)]
pred[, w := fifelse(en_cartera, 1 / k, 0)]

# Rotación: 0,5 * suma |w_t - w_{t-1}| (pesos objetivo; se ignora la deriva intrasemanal)
rot <- function(dt) {
  w <- dcast(dt, semana ~ ticker, value.var = "w", fill = 0)
  m <- as.matrix(w[, -1]); dm <- rbind(m[1, ], diff(m))       # primera semana: compra inicial
  cs <- dcast(dt, semana ~ ticker, value.var = "cs_63")          # spread conocido en t
  cs <- as.matrix(cs[, lapply(.SD, nafill, type = "locf"), .SDcols = -1])  # salida: último spread
  cs[!is.finite(cs)] <- 0
  data.table(semana = w$semana, rotacion = 0.5 * rowSums(abs(dm)),
             costo_cs = rowSums(abs(dm) * cs / 2))                # medio spread por transacción
}
semanal <- pred[, {
  cart <- .SD[en_cartera == TRUE]
  list(ret_cartera = mean(cart$ret_fwd), ret_benchmark = mean(ret_fwd), n_cartera = nrow(cart))
}, by = .(modelo, pais, semana)]
rot_dt <- pred[, rot(.SD), by = .(modelo, pais)]
semanal <- rot_dt[semanal, on = .(modelo, pais, semana)]
semanal[is.na(costo_cs), costo_cs := 0]

## 2. Medidas de desempeño ------------------------------------------------------
max_caida <- function(r) { v <- cumprod(1 + r); min(v / cummax(v) - 1) }
t_nw <- function(x, lag = 4L) {   # estadístico t del promedio con errores de Newey-West
  x <- x[is.finite(x)]; if (length(x) < 20) return(NA_real_)
  fit <- lm(x ~ 1); coef(fit)[[1]] / sqrt(sandwich::NeweyWest(fit, lag = lag, prewhite = FALSE)[1, 1])
}
resumen <- function(r_c, r_b, rotacion, costo_cs) {
  out <- list(semanas = length(r_c), rotacion_media = mean(rotacion),
              ret_anual_cartera_bruto = 52 * mean(r_c), ret_anual_benchmark = 52 * mean(r_b),
              activo_anual_bruto = 52 * mean(r_c - r_b), t_activo_bruto_nw = t_nw(r_c - r_b),
              sharpe_cartera_bruto = sqrt(52) * mean(r_c) / sd(r_c),
              sharpe_benchmark = sqrt(52) * mean(r_b) / sd(r_b),
              max_caida_cartera = max_caida(r_c),
              costo_equilibrio_pb = 1e4 * mean(r_c - r_b) / mean(rotacion))
  for (c in setdiff(COSTOS_PB, 0)) {
    r_n <- r_c - rotacion * c / 1e4
    out[[paste0("activo_anual_neto_", c, "pb")]] <- 52 * mean(r_n - r_b)
    out[[paste0("t_activo_neto_", c, "pb_nw")]] <- t_nw(r_n - r_b)
    out[[paste0("sharpe_neto_", c, "pb")]] <- sqrt(52) * mean(r_n) / sd(r_n)
  }
  r_cs <- r_c - costo_cs
  out$activo_anual_neto_spread_cs <- 52 * mean(r_cs - r_b)
  out$t_activo_neto_spread_cs_nw <- t_nw(r_cs - r_b)
  out$sharpe_neto_spread_cs <- sqrt(52) * mean(r_cs) / sd(r_cs)
  out
}
t14 <- semanal[, resumen(ret_cartera, ret_benchmark, rotacion, costo_cs), by = .(modelo, pais)]
mila <- semanal[, .(ret_cartera = mean(ret_cartera), ret_benchmark = mean(ret_benchmark),
                    rotacion = mean(rotacion), costo_cs = mean(costo_cs)), by = .(modelo, semana)]
t13 <- mila[, resumen(ret_cartera, ret_benchmark, rotacion, costo_cs), by = modelo]
num <- function(dt) { v <- names(dt)[vapply(dt, is.double, TRUE)]; dt[, (v) := lapply(.SD, round, 4), .SDcols = v]; dt }
guardar_tabla(num(t13), "t13_carteras_resumen_conjunto")
guardar_tabla(num(t14), "t14_carteras_por_pais")
print(t13[, .(modelo, activo_anual_bruto, activo_anual_neto_100pb, rotacion_media, costo_equilibrio_pb)])

## 3. Figura: retorno activo acumulado (bruto y neto de 100 pb) -----------------
setorder(mila, modelo, semana)
mila[, `:=`(acum_bruto = cumprod(1 + ret_cartera - ret_benchmark) - 1,
            acum_neto100 = cumprod(1 + ret_cartera - rotacion * 0.01 - ret_benchmark) - 1), by = modelo]
gg <- ggplot(melt(mila, id.vars = c("modelo", "semana"), measure.vars = c("acum_bruto", "acum_neto100")),
             aes(semana, value, colour = modelo)) + geom_line() +
  facet_wrap(~variable, labeller = as_labeller(c(acum_bruto = "Bruto", acum_neto100 = "Neto de 100 pb"))) +
  geom_hline(yintercept = 0, linetype = 2) +
  labs(x = NULL, y = "Retorno activo acumulado (quintil superior - equiponderada)", colour = NULL) +
  theme_minimal()
ggsave(file.path(DIR_FIGURAS, "f02_retorno_activo_acumulado.png"), gg, width = 9, height = 4.5, dpi = 200)
saveRDS(semanal, file.path(DIR_PREDICCIONES, "carteras_semanales.rds"))
msg("Listo. Siguiente paso: 06_heterogeneidad_liquidez.R")
