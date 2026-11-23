# =============================================================================
# 02_construccion_panel.R
# Construye el panel acción-semana (viernes a viernes) a partir de la base diaria
# congelada de Yahoo Finance: limpieza, características (predictores), variable
# dependiente, filtros de elegibilidad ex ante y normalización por rango.
#
# Entrada : datos/crudos/yahoo_diario_AAAAMMDD.csv.gz (el más reciente)
# Salidas : datos/procesados/panel_semanal.rds   (objeto data.table)
#           datos/procesados/panel_semanal.csv.gz
#           resultados/tablas/diccionario_variables.csv
#           resultados/tablas/t01_muestra_por_pais_anio.csv
#           resultados/tablas/t02_descriptivos_liquidez_por_pais.csv
#           resultados/tablas/t03_saltos_sospechosos.csv
# No requiere internet.
# =============================================================================
if (!exists("DIR_CODIGO")) DIR_CODIGO <- local({
  a <- commandArgs(FALSE); f <- sub("^--file=", "", a[grep("^--file=", a)])
  if (length(f) == 1) dirname(normalizePath(f)) else
    if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable() &&
        nzchar(rstudioapi::getSourceEditorContext()$path))
      dirname(normalizePath(rstudioapi::getSourceEditorContext()$path)) else getwd()
})
source(file.path(DIR_CODIGO, "00_configuracion.R"), encoding = "UTF-8")

## 1. Lectura ---------------------------------------------------------------------
f_crudo <- if (exists("ARCHIVO_CRUDO")) ARCHIVO_CRUDO else
  ultimo_archivo(DIR_DATOS_CRUDOS, "^yahoo_diario_.*\\.csv\\.gz$")
msg("Leyendo", basename(f_crudo))
d <- fread(f_crudo)
d[, fecha := as.Date(fecha)]
num_cols <- intersect(c("apertura", "maximo", "minimo", "cierre", "volumen", "cierre_aj"), names(d))
d[, (num_cols) := lapply(.SD, as.numeric), .SDcols = num_cols]   # el volumen puede venir como entero
setorder(d, ticker, fecha)

# Semana que termina el viernes (los sábados/domingos ocasionales van al viernes previo)
viernes <- function(x) { u <- as.integer(format(x, "%u")); x + ifelse(u <= 5L, 5L - u, -(u - 5L)) }
d[, semana := viernes(fecha)]

acc <- merge(d, UNIVERSO, by = "ticker")                     # acciones
mer <- merge(d, SERIES_MERCADO, by = "ticker")               # índices, FX y globales
msg("Acciones con datos:", uniqueN(acc$ticker), "| series de mercado:", uniqueN(mer$ticker))

## 2. Tipo de cambio diario (moneda local por USD) para el volumen en dólares -----
fx <- mer[grepl("^fx_", serie), .(fecha, pais = sub("^fx_", "", serie), fx = cierre)]
fx <- fx[is.finite(fx) & fx > 0]
calendario <- CJ(pais = PAISES, fecha = seq(min(d$fecha), max(d$fecha), by = "day"))
fx <- fx[calendario, on = .(pais, fecha)]
setorder(fx, pais, fecha)
fx[, fx := nafill(fx, type = "locf"), by = pais]
acc <- fx[acc, on = .(pais, fecha)]

## 3. Variables diarias y ventanas móviles (en días hábiles) ---------------------
acc <- acc[is.finite(cierre_aj) & cierre_aj > 0]
setorder(acc, ticker, fecha)
acc[, volumen := fifelse(is.na(volumen), 0, volumen)]
acc[, r := cierre_aj / shift(cierre_aj) - 1, by = ticker]
acc[, dvol_usd := fifelse(is.finite(fx) & fx > 0, cierre * volumen / fx, NA_real_)]
acc[, sin_vol := as.integer(volumen <= 0)]
acc[, ret_cero := as.integer(!is.na(r) & r == 0)]
# Iliquidez de Amihud diaria: |r| / volumen en millones de USD, solo días con volumen
acc[, amihud_d := fifelse(volumen > 0 & dvol_usd > 0, abs(r) / (dvol_usd / 1e6), NA_real_)]

# Spread de Corwin y Schultz (2012) con dos días consecutivos (t-1, t), asignado a t
# para no usar información futura. Ajuste por variación nocturna y spreads negativos = 0.
acc[, `:=`(h1 = shift(maximo), l1 = shift(minimo), c1 = shift(cierre)), by = ticker]
acc[, `:=`(h2 = maximo, l2 = minimo)]
acc[l2 > c1 & is.finite(c1), `:=`(h2 = h2 - (l2 - c1), l2 = l2 - (l2 - c1))]
acc[h2 < c1 & is.finite(c1), `:=`(l2 = l2 + (c1 - h2), h2 = h2 + (c1 - h2))]
k <- 3 - 2 * sqrt(2)
acc[, beta_cs  := log(h1 / l1)^2 + log(h2 / l2)^2]
acc[, gamma_cs := log(pmax(h1, h2) / pmin(l1, l2))^2]
acc[, alfa_cs  := (sqrt(2 * beta_cs) - sqrt(beta_cs)) / k - sqrt(gamma_cs / k)]
acc[, cs_d := 2 * (exp(alfa_cs) - 1) / (1 + exp(alfa_cs))]
acc[!is.finite(cs_d), cs_d := NA_real_]
acc[cs_d < 0, cs_d := 0]
acc[, c("h1", "l1", "c1", "h2", "l2", "beta_cs", "gamma_cs", "alfa_cs") := NULL]

# Saltos sospechosos (posibles splits mal ajustados): se reportan, no se corrigen aquí
saltos <- acc[abs(r) > 0.5, .(ticker, pais, fecha, r, cierre_aj)]
guardar_tabla(saltos, "t03_saltos_sospechosos")

roll_mean <- function(x, n, min_obs = ceiling(n / 2)) {
  m <- frollmean(x, n, na.rm = TRUE, algo = "exact")
  ok <- frollsum(as.numeric(!is.na(x)), n) >= min_obs
  fifelse(ok, m, NA_real_)
}
roll_sd <- function(x, n) {
  m1 <- roll_mean(x, n); m2 <- roll_mean(x^2, n)
  sqrt(pmax(m2 - m1^2, 0))
}
roll_skew <- function(x, n) {
  m1 <- roll_mean(x, n); m2 <- roll_mean(x^2, n); m3 <- roll_mean(x^3, n)
  v <- m2 - m1^2
  fifelse(v > 0, (m3 - 3 * m1 * m2 + 2 * m1^3) / v^1.5, NA_real_)
}
roll_max <- function(x, n) frollapply(x, n, function(z) if (all(is.na(z))) NA_real_ else max(z, na.rm = TRUE))

msg("Calculando ventanas móviles diarias...")
acc[, `:=`(
  vol_21    = roll_sd(r, 21),
  vol_63    = roll_sd(r, 63),
  vol_126   = roll_sd(r, 126),
  max_21    = roll_max(r, 21),
  asim_63   = roll_skew(r, 63),
  amihud_63 = roll_mean(amihud_d, 63, min_obs = 10),
  dvol_63   = roll_mean(dvol_usd, 63),
  ceros_63  = roll_mean(ret_cero, 63),
  sinvol_63 = roll_mean(sin_vol, 63),
  cs_63     = roll_mean(cs_d, 63, min_obs = 10),
  pmax_252  = roll_max(cierre_aj, 252)
), by = ticker]

## 4. Agregación semanal -----------------------------------------------------------
sem <- acc[, .(
  fecha_ult = fecha[.N], precio = cierre_aj[.N], dias = .N,
  dias_con_vol = sum(volumen > 0), dvol_sem = sum(dvol_usd, na.rm = TRUE),
  vol_21 = vol_21[.N], vol_63 = vol_63[.N], vol_126 = vol_126[.N], max_21 = max_21[.N],
  asim_63 = asim_63[.N], amihud_63 = amihud_63[.N], dvol_63 = dvol_63[.N],
  ceros_63 = ceros_63[.N], sinvol_63 = sinvol_63[.N], cs_63 = cs_63[.N], pmax_252 = pmax_252[.N]
), by = .(pais, ticker, semana)]
setorder(sem, ticker, semana)

# Retornos semanales y características de la serie semanal
sem[, ret_1 := precio / shift(precio) - 1, by = ticker]
sem[, gap := as.integer(semana - shift(semana)), by = ticker]      # 7 = semanas contiguas
sem[!is.na(gap) & gap > 7, ret_1 := NA_real_]                        # no encadenar huecos
acum <- function(r, n) exp(frollsum(log1p(r), n)) - 1
sem[, `:=`(
  mom_4   = acum(ret_1, 4),
  mom_13  = acum(ret_1, 13),
  mom_26  = acum(ret_1, 26),
  mom_52_4 = shift(acum(ret_1, 48), 4),                                # t-51 .. t-4
  ma10    = precio / frollmean(precio, 10) - 1,
  ma40    = precio / frollmean(precio, 40) - 1,
  p52max  = precio / pmax_252,
  dvol_chg = log1p(dvol_sem) - log1p(shift(frollmean(dvol_sem, 13), 1)),
  n_hist  = seq_len(.N)
), by = ticker]

# Mercado local: promedio equiponderado de los retornos semanales del país
mkt <- sem[is.finite(ret_1), .(mkt_ret_1 = mean(ret_1), n_mkt = .N), by = .(pais, semana)]
setorder(mkt, pais, semana)
mkt[, mkt_vol_13 := sqrt(pmax(frollmean(mkt_ret_1^2, 13) - frollmean(mkt_ret_1, 13)^2, 0)), by = pais]
sem <- mkt[sem, on = .(pais, semana)]
setorder(sem, ticker, semana)

# Beta y volatilidad idiosincrática móviles (52 semanas, mínimo 26)
sem[, `:=`(rx = fifelse(is.finite(ret_1) & is.finite(mkt_ret_1), ret_1, NA_real_),
           rm = fifelse(is.finite(ret_1) & is.finite(mkt_ret_1), mkt_ret_1, NA_real_))]
sem[, `:=`(mx = roll_mean(rx, 52, 26), mm = roll_mean(rm, 52, 26),
           mxm = roll_mean(rx * rm, 52, 26), mm2 = roll_mean(rm^2, 52, 26),
           mx2 = roll_mean(rx^2, 52, 26)), by = ticker]
sem[, beta_52 := (mxm - mx * mm) / (mm2 - mm^2)]
sem[, ivol_52 := sqrt(pmax((mx2 - mx^2) - beta_52^2 * (mm2 - mm^2), 0))]
sem[, c("rx", "rm", "mx", "mm", "mxm", "mm2", "mx2") := NULL]

## 5. Variables globales y tipo de cambio (semanales) ------------------------------
glob <- mer[, .(valor = cierre[.N]), by = .(serie, semana)]
glob <- dcast(glob, semana ~ serie, value.var = "valor")
setorder(glob, semana)
r1 <- function(x) x / shift(x) - 1
r4 <- function(x) x / shift(x, 4) - 1
g <- glob[, .(semana)]
if ("vix" %in% names(glob))          g[, `:=`(m_vix = log(glob$vix), m_vix_chg = log(glob$vix) - shift(log(glob$vix)))]
if ("sp500" %in% names(glob))        g[, m_spx_ret_1 := r1(glob$sp500)]
if ("tasa10y_eeuu" %in% names(glob)) g[, m_tnx_chg := glob$tasa10y_eeuu - shift(glob$tasa10y_eeuu)]
if ("dolar_indice" %in% names(glob)) g[, m_dxy_ret_1 := r1(glob$dolar_indice)]
if ("cobre" %in% names(glob))        g[, m_cobre_ret_4 := r4(glob$cobre)]
if ("oro" %in% names(glob))          g[, m_oro_ret_4 := r4(glob$oro)]
if ("wti" %in% names(glob))          g[, m_wti_ret_4 := r4(glob$wti)]
if ("eem" %in% names(glob))          g[, m_eem_ret_1 := r1(glob$eem)]
sem <- g[sem, on = "semana"]

fxw <- mer[grepl("^fx_", serie), .(fx = cierre[.N]), by = .(pais = sub("^fx_", "", serie), semana)]
setorder(fxw, pais, semana)
fxw[, `:=`(m_fx_ret_1 = fx / shift(fx) - 1, m_fx_ret_4 = fx / shift(fx, 4) - 1), by = pais]
sem <- fxw[, .(pais, semana, m_fx_ret_1, m_fx_ret_4)][sem, on = .(pais, semana)]
sem[, `:=`(m_mkt_ret_1 = mkt_ret_1, m_mkt_vol_13 = mkt_vol_13)]

## 6. Variable dependiente (dirección de t+1 y de t+1..t+4) ----------------------
setorder(sem, ticker, semana)
sem[, `:=`(ret_fwd_1 = shift(ret_1, -1),
           ret_fwd_4 = shift(precio, -4) / precio - 1,
           gap_fwd   = as.integer(shift(semana, -1) - semana)), by = ticker]
sem[!is.na(gap_fwd) & gap_fwd > 7, ret_fwd_1 := NA_real_]
sem[, `:=`(y_1 = as.integer(ret_fwd_1 > 0), y_4 = as.integer(ret_fwd_4 > 0))]

## 7. Filtros de elegibilidad ex ante --------------------------------------------
sem[, elegible := n_hist >= MIN_SEMANAS_HISTORIA &
                  !is.na(sinvol_63) & sinvol_63 <= MAX_PROP_DIAS_SIN_VOLUMEN &
                  is.finite(ret_1) & abs(ret_1) <= 1 &
                  is.finite(ret_fwd_1) & abs(ret_fwd_1) <= 1 &
                  semana >= FECHA_MUESTRA_INICIO & (semana + 7) <= FECHA_MUESTRA_FIN + 6]
panel <- sem[elegible == TRUE]
panel[, n_pais_sem := .N, by = .(pais, semana)]
panel <- panel[n_pais_sem >= MIN_ACCIONES_POR_SEMANA]
msg("Panel elegible:", nrow(panel), "acción-semanas;", uniqueN(panel$ticker), "acciones")

## 8. Normalización por rango en el corte transversal (Gu, Kelly y Xiu, 2020) -----
# Rangos dentro de cada país y semana, como en Hanauer y Kalsbach (2023) y Cakici,
# Fieberg, Metko y Zaremba (2023): cada característica mide la posición de la acción
# respecto de su propio mercado (las diferencias de nivel entre países las recogen
# las variables indicadoras de país).
CARACT_ACCION <- c(ret_1 = "c_ret_1", mom_4 = "c_mom_4", mom_13 = "c_mom_13",
                   mom_26 = "c_mom_26", mom_52_4 = "c_mom_52_4", vol_21 = "c_vol_4",
                   vol_63 = "c_vol_13", vol_126 = "c_vol_26", max_21 = "c_max_4",
                   asim_63 = "c_asim_13", amihud_63 = "c_amihud_13", dvol_63 = "c_dvol_13",
                   dvol_chg = "c_dvol_chg", ceros_63 = "c_ceros_13", sinvol_63 = "c_sinvol_13",
                   cs_63 = "c_cs_13", p52max = "c_p52max", ma10 = "c_ma10", ma40 = "c_ma40",
                   beta_52 = "c_beta_52", ivol_52 = "c_ivol_52")
rango_pm1 <- function(x) {
  ok <- is.finite(x); out <- rep(0, length(x)); n <- sum(ok)
  if (n > 1) out[ok] <- 2 * (frank(x[ok], ties.method = "average") - 1) / (n - 1) - 1
  out                                                   # faltantes = mediana (0)
}
for (v in names(CARACT_ACCION)) panel[, (CARACT_ACCION[[v]]) := rango_pm1(get(v)), by = .(pais, semana)]
for (p in PAISES) panel[, (paste0("d_", p)) := as.integer(pais == p)]

VARS_MERCADO <- intersect(c("m_mkt_ret_1", "m_mkt_vol_13", "m_fx_ret_1", "m_fx_ret_4", "m_vix",
                            "m_vix_chg", "m_spx_ret_1", "m_tnx_chg", "m_dxy_ret_1", "m_cobre_ret_4",
                            "m_oro_ret_4", "m_wti_ret_4", "m_eem_ret_1"), names(panel))
VARS_MERCADO <- VARS_MERCADO[vapply(VARS_MERCADO, function(v) mean(is.finite(panel[[v]])) > 0.9, TRUE)]
for (v in VARS_MERCADO) {                                  # faltantes puntuales: último valor
  setorder(panel, ticker, semana); panel[, (v) := nafill(get(v), "locf"), by = ticker]
  panel[!is.finite(get(v)), (v) := 0]
}
panel[, anio := as.integer(format(semana, "%Y"))]
panel[, anio_obj := as.integer(format(semana + 7, "%Y"))]   # año de la semana pronosticada
setorder(panel, semana, pais, ticker)

attr(panel, "vars_accion")  <- unname(CARACT_ACCION)
attr(panel, "vars_mercado") <- VARS_MERCADO
attr(panel, "vars_pais")    <- paste0("d_", PAISES)
saveRDS(panel, file.path(DIR_DATOS_PROC, "panel_semanal.rds"))
fwrite(panel, file.path(DIR_DATOS_PROC, "panel_semanal.csv.gz"))

## 9. Diccionario de variables -------------------------------------------------
dicc <- data.table(
  variable = c("y_1", "y_4", unname(CARACT_ACCION), VARS_MERCADO, paste0("d_", PAISES)),
  descripcion = c(
    "1 si el retorno de la semana t+1 es positivo (variable dependiente principal)",
    "1 si el retorno acumulado t+1..t+4 es positivo (robustez)",
    "Retorno de la semana t (reversión de corto plazo)",
    "Retorno acumulado de 4 semanas (t-3..t)", "Retorno acumulado de 13 semanas",
    "Retorno acumulado de 26 semanas", "Momentum 52 semanas sin las 4 últimas (t-51..t-4)",
    "Volatilidad de retornos diarios, 21 días hábiles", "Volatilidad diaria, 63 días",
    "Volatilidad diaria, 126 días", "Máximo retorno diario, 21 días",
    "Asimetría de retornos diarios, 63 días",
    "Iliquidez de Amihud: promedio de |r|/volumen en millones de USD, 63 días",
    "Volumen diario promedio en USD, 63 días (tamaño y liquidez)",
    "Volumen de la semana t respecto del promedio de las 13 previas (log)",
    "Proporción de días con retorno cero, 63 días (Lesmond, Ogden y Trzcinka)",
    "Proporción de días sin volumen, 63 días",
    "Spread estimado de Corwin y Schultz, promedio de 63 días",
    "Precio / máximo de 52 semanas", "Precio / media móvil de 10 semanas - 1",
    "Precio / media móvil de 40 semanas - 1",
    "Beta respecto del mercado local, 52 semanas", "Volatilidad idiosincrática, 52 semanas",
    c(m_mkt_ret_1 = "Retorno semanal del mercado local (promedio equiponderado de la muestra)",
      m_mkt_vol_13 = "Volatilidad del mercado local, 13 semanas",
      m_fx_ret_1 = "Variación semanal del tipo de cambio (moneda local por USD)",
      m_fx_ret_4 = "Variación del tipo de cambio en 4 semanas",
      m_vix = "Log del VIX", m_vix_chg = "Cambio semanal del log VIX",
      m_spx_ret_1 = "Retorno semanal del S&P 500", m_tnx_chg = "Cambio semanal de la tasa del Tesoro a 10 años",
      m_dxy_ret_1 = "Retorno semanal del índice dólar", m_cobre_ret_4 = "Retorno del cobre, 4 semanas",
      m_oro_ret_4 = "Retorno del oro, 4 semanas", m_wti_ret_4 = "Retorno del petróleo WTI, 4 semanas",
      m_eem_ret_1 = "Retorno semanal del ETF de mercados emergentes (EEM)")[VARS_MERCADO],
    paste("Indicador de país:", PAISES)),
  tipo = c("dependiente", "dependiente", rep("acción (rango [-1,1])", length(CARACT_ACCION)),
           rep("mercado/global (estandarizada en cada bloque)", length(VARS_MERCADO)),
           rep("efecto fijo de país", length(PAISES)))
)
guardar_tabla(dicc, "diccionario_variables")

## 10. Tablas descriptivas de la muestra ---------------------------------------
t01 <- panel[, .(acciones = uniqueN(ticker), obs = .N, prop_sube = round(mean(y_1), 3),
                 prop_ret_cero = round(mean(ret_fwd_1 == 0), 3)), by = .(pais, anio)][order(pais, anio)]
guardar_tabla(t01, "t01_muestra_por_pais_anio")
t02 <- panel[, .(acciones = uniqueN(ticker), obs = .N,
                 ret_sem_medio_pct = round(100 * mean(ret_fwd_1), 3),
                 ret_sem_sd_pct = round(100 * sd(ret_fwd_1), 3),
                 prop_sube = round(mean(y_1), 3),
                 amihud_mediana = round(median(amihud_63, na.rm = TRUE), 4),
                 dvol_usd_mediana_mill = round(median(dvol_63, na.rm = TRUE) / 1e6, 3),
                 ceros_mediana = round(median(ceros_63, na.rm = TRUE), 3),
                 cs_spread_mediana_pct = round(100 * median(cs_63, na.rm = TRUE), 3)),
              by = pais][order(pais)]
guardar_tabla(t02, "t02_descriptivos_liquidez_por_pais")
print(t02)
msg("Listo. Siguiente paso: 03_modelos_walkforward.R")
