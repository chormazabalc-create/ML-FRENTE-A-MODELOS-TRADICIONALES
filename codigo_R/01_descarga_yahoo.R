# =============================================================================
# 01_descarga_yahoo.R
# Descarga precios diarios (apertura, máximo, mínimo, cierre, cierre ajustado y
# volumen) de las acciones de Chile, Colombia, México y Perú, los índices locales, los tipos de cambio y
# las variables globales desde Yahoo Finance.
#
# REQUIERE INTERNET. Es el único script del flujo que necesita conexión (junto
# con el opcional 01b). Todo lo demás corre sobre los archivos que este escribe.
#
# Salidas (carpeta datos/crudos):
#   yahoo_diario_AAAAMMDD.csv.gz   panel diario largo (una fila por ticker-día)
#   yahoo_log_AAAAMMDD.csv         resultado de la descarga por ticker
#
# Regla de replicación: la base cruda se descarga UNA vez y se congela con la
# fecha en el nombre. Yahoo reajusta precios hacia atrás con cada dividendo, por
# lo que dos descargas en fechas distintas no son idénticas.
# =============================================================================
if (!exists("DIR_CODIGO")) DIR_CODIGO <- local({
  a <- commandArgs(FALSE); f <- sub("^--file=", "", a[grep("^--file=", a)])
  if (length(f) == 1) dirname(normalizePath(f)) else
    if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable() &&
        nzchar(rstudioapi::getSourceEditorContext()$path))
      dirname(normalizePath(rstudioapi::getSourceEditorContext()$path)) else getwd()
})
source(file.path(DIR_CODIGO, "00_configuracion.R"), encoding = "UTF-8")

## 1. Funciones de descarga ------------------------------------------------------
UA <- paste("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36",
            "(KHTML, like Gecko) Chrome/126.0 Safari/537.36")

# Convierte la respuesta JSON del endpoint v8/chart en un data.table diario.
parsear_chart_v8 <- function(txt, ticker) {
  js <- jsonlite::fromJSON(txt, simplifyVector = TRUE)
  res <- js$chart$result
  if (is.null(res) || !length(res$timestamp)) return(NULL)
  ts  <- unlist(res$timestamp)
  off <- if (!is.null(res$meta$gmtoffset)) as.numeric(res$meta$gmtoffset[1]) else 0
  q   <- res$indicators$quote[[1]]
  adj <- tryCatch(unlist(res$indicators$adjclose[[1]]$adjclose), error = function(e) NULL)
  num <- function(x) { x <- unlist(x); if (is.null(x)) rep(NA_real_, length(ts)) else as.numeric(x) }
  out <- data.table(
    ticker   = ticker,
    fecha    = as.Date(as.POSIXct(ts + off, origin = "1970-01-01", tz = "UTC")),
    apertura = num(q$open), maximo = num(q$high), minimo = num(q$low),
    cierre   = num(q$close), volumen = num(q$volume),
    cierre_aj = if (is.null(adj)) num(q$close) else as.numeric(adj),
    moneda   = if (!is.null(res$meta$currency)) res$meta$currency[1] else NA_character_
  )
  out <- out[!is.na(cierre)]
  # Yahoo a veces repite la última fila con hora intradía: se conserva la última por fecha
  out <- out[, .SD[.N], by = .(ticker, fecha)]
  setcolorder(out, c("ticker", "fecha"))
  out[]
}

descargar_v8 <- function(ticker, desde, hasta, intentos = 3) {
  url <- sprintf(paste0("https://query1.finance.yahoo.com/v8/finance/chart/%s",
                        "?period1=%d&period2=%d&interval=1d&events=div%%7Csplit",
                        "&includeAdjustedClose=true"),
                 utils::URLencode(ticker, reserved = TRUE),
                 as.integer(as.POSIXct(desde, tz = "UTC")),
                 as.integer(as.POSIXct(hasta, tz = "UTC")))
  for (k in seq_len(intentos)) {
    h <- curl::new_handle()
    curl::handle_setheaders(h, "User-Agent" = UA, "Accept" = "application/json")
    r <- tryCatch(curl::curl_fetch_memory(url, handle = h), error = function(e) NULL)
    if (!is.null(r) && r$status_code == 200) {
      out <- tryCatch(parsear_chart_v8(rawToChar(r$content), ticker), error = function(e) NULL)
      if (!is.null(out) && nrow(out)) return(out)
      return(NULL)                        # respuesta válida pero sin datos
    }
    if (!is.null(r) && r$status_code == 404) return(NULL)   # ticker inexistente
    Sys.sleep(2 * k)                      # 429 u otro error: espera creciente
  }
  NULL
}

# Respaldo: quantmod (maneja cookies/crumb de Yahoo por su cuenta)
descargar_quantmod <- function(ticker, desde, hasta) {
  if (!requireNamespace("quantmod", quietly = TRUE)) return(NULL)
  x <- tryCatch(suppressWarnings(quantmod::getSymbols(ticker, src = "yahoo", from = desde,
                  to = hasta, auto.assign = FALSE, warnings = FALSE)), error = function(e) NULL)
  if (is.null(x) || !NROW(x)) return(NULL)
  m <- as.matrix(x)
  data.table(ticker = ticker, fecha = as.Date(zoo::index(x)),
             apertura = m[, 1], maximo = m[, 2], minimo = m[, 3], cierre = m[, 4],
             volumen = m[, 5], cierre_aj = m[, 6], moneda = NA_character_)[!is.na(cierre)]
}

descargar_ticker <- function(ticker, desde = FECHA_DESCARGA_INICIO, hasta = FECHA_DESCARGA_FIN) {
  out <- descargar_v8(ticker, desde, hasta); fuente <- "yahoo_v8"
  if (is.null(out)) { out <- descargar_quantmod(ticker, desde, hasta); fuente <- "quantmod" }
  if (is.null(out)) return(list(datos = NULL, fuente = NA_character_))
  list(datos = out, fuente = fuente)
}

## 2. Descarga --------------------------------------------------------------------
todos <- rbind(UNIVERSO[, .(tipo = "accion", pais, ticker)],
               SERIES_MERCADO[, .(tipo = "mercado", pais = NA_character_, ticker)])
msg("Descargando", nrow(todos), "series de Yahoo Finance entre", format(FECHA_DESCARGA_INICIO),
    "y", format(FECHA_DESCARGA_FIN))

lista <- vector("list", nrow(todos)); log <- vector("list", nrow(todos))
for (i in seq_len(nrow(todos))) {
  tk <- todos$ticker[i]
  r  <- descargar_ticker(tk)
  ok <- !is.null(r$datos)
  lista[[i]] <- r$datos
  log[[i]] <- data.table(tipo = todos$tipo[i], pais = todos$pais[i], ticker = tk, descargado = ok,
                         fuente = r$fuente,
                         primera_fecha = if (ok) min(r$datos$fecha) else as.Date(NA),
                         ultima_fecha  = if (ok) max(r$datos$fecha) else as.Date(NA),
                         n_dias = if (ok) nrow(r$datos) else 0L)
  cat(sprintf("[%3d/%3d] %-14s %s\n", i, nrow(todos), tk,
              if (ok) sprintf("ok (%s, %d días desde %s)", r$fuente, nrow(r$datos),
                              format(min(r$datos$fecha))) else "SIN DATOS"))
  Sys.sleep(0.35)   # evita bloqueos por ráfagas
}
crudo <- rbindlist(lista, fill = TRUE)
log   <- rbindlist(log, fill = TRUE)

## 3. Guardar ---------------------------------------------------------------------
sello <- format(Sys.Date(), "%Y%m%d")
f_datos <- file.path(DIR_DATOS_CRUDOS, paste0("yahoo_diario_", sello, ".csv.gz"))
f_log   <- file.path(DIR_DATOS_CRUDOS, paste0("yahoo_log_", sello, ".csv"))
fwrite(crudo, f_datos)
fwrite(log, f_log)

msg("Filas descargadas:", nrow(crudo), "| tickers con datos:", sum(log$descargado), "de", nrow(log))
print(log[, .(candidatos = .N, con_datos = sum(descargado),
              desde_2014 = sum(descargado & primera_fecha <= as.Date("2014-12-31"), na.rm = TRUE)),
          by = .(tipo, pais)])
msg("Archivos escritos:\n  ", f_datos, "\n  ", f_log)
msg("Siguiente paso: 02_construccion_panel.R (no requiere internet).")
