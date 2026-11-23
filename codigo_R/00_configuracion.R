# =============================================================================
# 00_configuracion.R
# Tesis UFT "Machine learning y dirección de los retornos accionarios en Chile, Colombia, México y Perú"
# Código de replicación del profesor guía (R). Los estudiantes replican en Python.
#
# Este archivo NO se ejecuta solo: lo cargan todos los demás scripts.
# Define rutas, parámetros del diseño, universo de tickers y paquetes.
# Para cambiar el diseño (fechas, modelos, costos), editar SOLO este archivo.
# =============================================================================

## 0. Ubicación de la carpeta codigo_R -----------------------------------------
# Funciona con Source en RStudio, con source() desde otro script y con Rscript.
.dir_script <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", args[grep("^--file=", args)])
  if (length(f) == 1 && nzchar(f)) return(dirname(normalizePath(f)))
  for (i in rev(seq_len(sys.nframe()))) {
    of <- tryCatch(sys.frame(i)$ofile, error = function(e) NULL)
    if (!is.null(of)) return(dirname(normalizePath(of)))
  }
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    p <- tryCatch(rstudioapi::getSourceEditorContext()$path, error = function(e) "")
    if (nzchar(p)) return(dirname(normalizePath(p)))
  }
  getwd()
}
if (!exists("DIR_CODIGO")) DIR_CODIGO <- .dir_script()

DIR_DATOS_CRUDOS  <- file.path(DIR_CODIGO, "datos", "crudos")
DIR_DATOS_PROC    <- file.path(DIR_CODIGO, "datos", "procesados")
DIR_RESULTADOS    <- file.path(DIR_CODIGO, "resultados")
DIR_TABLAS        <- file.path(DIR_RESULTADOS, "tablas")
DIR_FIGURAS       <- file.path(DIR_RESULTADOS, "figuras")
DIR_PREDICCIONES  <- file.path(DIR_RESULTADOS, "predicciones")
for (d in c(DIR_DATOS_CRUDOS, DIR_DATOS_PROC, DIR_TABLAS, DIR_FIGURAS, DIR_PREDICCIONES))
  dir.create(d, showWarnings = FALSE, recursive = TRUE)

## 1. Paquetes ------------------------------------------------------------------
# Núcleo (obligatorios). xgboost >= 1.7; el código es compatible con la API 3.x.
PAQUETES <- c("data.table", "curl", "jsonlite", "glmnet", "ranger", "xgboost",
              "pROC", "ggplot2", "sandwich", "nnet")
.instalar_si_falta <- function(pkgs) {
  falta <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(falta)) install.packages(falta, repos = "https://cloud.r-project.org")
}
.instalar_si_falta(PAQUETES)
suppressPackageStartupMessages({
  library(data.table); library(ggplot2)
})
options(datatable.print.nrows = 20, scipen = 999)

## 2. Diseño ----------------------------------------------------------------------
FECHA_DESCARGA_INICIO <- as.Date("2013-12-01")  # 52 semanas de historia antes de 2015
FECHA_DESCARGA_FIN    <- as.Date("2026-01-16")  # incluye la semana t+1 de fines de 2025
FECHA_MUESTRA_INICIO  <- as.Date("2015-01-01")  # primera semana t del panel
FECHA_MUESTRA_FIN     <- as.Date("2025-12-31")  # última semana t+1 evaluada

# Validación caminante (walk-forward) con reestimación anual:
#   entrenamiento 2015..(a-2), validación (a-1), prueba a, para a = 2020..2025
ANIOS_PRUEBA       <- 2020:2025
ANIO_INICIO_ENTREN <- 2015
SEMANAS_PURGA      <- 1      # semanas que se eliminan al final de cada bloque (horizonte h)
VENTANA_MOVIL_ANIOS <- if (exists("VENTANA_MOVIL_ANIOS")) VENTANA_MOVIL_ANIOS else NA  # NA = ventana expansiva (Gu, Kelly y Xiu); p. ej. 5 = móvil de 5 años (robustez)

# Filtros de elegibilidad ex ante (solo con información disponible en t)
MIN_SEMANAS_HISTORIA <- 52    # historia mínima para calcular las características
MAX_PROP_DIAS_SIN_VOLUMEN <- 0.50  # en las últimas 13 semanas
MIN_ACCIONES_POR_SEMANA   <- 5     # semanas-país con menos acciones se descartan

# Semillas y paralelismo
SEMILLA  <- 20260921
N_HILOS  <- max(1L, parallel::detectCores() - 1L)

# Modo rápido: TRUE reduce grillas y árboles (para probar que el código corre).
MODO_RAPIDO <- identical(Sys.getenv("TESIS_MODO_RAPIDO"), "1")

# Costos de transacción (ida y vuelta, en puntos base) para las carteras
COSTOS_PB <- c(0, 50, 100, 150)
CUANTIL_CARTERA <- 0.20   # quintil superior de probabilidad predicha

## 3. Universo de tickers (Yahoo Finance) ----------------------------------------
# Punto de partida: composición reciente de los índices (documento 03 de Ayuda profesor).
# La muestra final la decide el filtro de cobertura y liquidez, no esta lista.
TICKERS <- list(
  Chile = paste0(c("SQM-B","FALABELLA","COPEC","CENCOSUD","CMPC","ENELAM","ENELCHILE",
                   "BSANTANDER","CHILE","BCI","ITAUCL","LTM","CCU","ANDINA-B","COLBUN",
                   "AGUAS-A","ENTEL","VAPORES","CAP","PARAUCO","MALLPLAZA","CENCOMALLS",
                   "RIPLEY","SMU","CONCHATORO","SECURITY","IAM","QUINENCO","SONDA",
                   "SALFACORP","BESALCO","ILC","HABITAT","ECL","AESANDES","ORO-BLANCO",
                   "NORTEGRAN","SM-CHILE-B"), ".SN"),
  Mexico = paste0(c("AMXB","WALMEX","FEMSAUBD","GFNORTEO","GMEXICOB","CEMEXCPO","BIMBOA",
                    "TLEVISACPO","GAPB","ASURB","OMAB","KOFUBL","AC","ALFAA","ALSEA",
                    "GRUMAB","KIMBERA","LIVEPOLC-1","PINFRA","PE&OLES","GCARSOA1",
                    "ELEKTRA","BBAJIOO","GFINBURO","MEGACPO","ORBIA","CUERVO","LABB","Q",
                    "VESTA","BOLSAA","RA","GENTERA","CHDRAUIB","VOLARA"), ".MX"),
  Colombia = paste0(c("ECOPETROL","BCOLOMBIA","PFBCOLOM","ISA","GRUPOSURA","PFGRUPSURA",
                      "GRUPOARGOS","PFGRUPOARG","CEMARGOS","PFCEMARGOS","NUTRESA","GEB",
                      "CELSIA","PFDAVVNDA","BOGOTA","CORFICOLCF","PFAVAL","PROMIGAS",
                      "MINEROS","CNEC","ETB","TERPEL","CONCONCRET","BVC","EXITO"), ".CL"),
  Peru = paste0(c("CREDITC1","ALICORC1","VOLCABC1","FERREYC1","BACKUSI1","CPACASC1",
                  "UNACEMC1","INRETC1","ENGEPEC1","LUSURC1","MINSURI1","CVERDEC1",
                  "ATACOBC1","RELAPAC1","AENZAC1","CASAGRC1","SIDERC1","CORAREI1",
                  "ENDISPC1","BVLAC1"), ".LM")
)

# Índices locales, tipos de cambio y variables globales
SERIES_MERCADO <- data.table(
  ticker = c("^IPSA", "^MXX", "^COLCAP", "ICOLCAP.CL", "^SPBLPGPT",
             "CLP=X", "MXN=X", "COP=X", "PEN=X",
             "^VIX", "^GSPC", "^TNX", "DX-Y.NYB", "HG=F", "GC=F", "CL=F", "BZ=F",
             "EEM", "ILF"),
  serie  = c("indice_Chile", "indice_Mexico", "indice_Colombia", "indice_Colombia_etf",
             "indice_Peru",
             "fx_Chile", "fx_Mexico", "fx_Colombia", "fx_Peru",
             "vix", "sp500", "tasa10y_eeuu", "dolar_indice", "cobre", "oro", "wti", "brent",
             "eem", "ilf")
)

PAISES <- names(TICKERS)
UNIVERSO <- rbindlist(lapply(PAISES, function(p) data.table(pais = p, ticker = TICKERS[[p]])))

## 4. Utilidades comunes -------------------------------------------------------
ultimo_archivo <- function(dir, patron) {
  f <- list.files(dir, pattern = patron, full.names = TRUE)
  if (!length(f)) stop("No se encontró ningún archivo '", patron, "' en ", dir,
                       ". Ejecute primero el script anterior del flujo.")
  f[order(file.info(f)$mtime, decreasing = TRUE)][1]
}
msg <- function(...) cat(format(Sys.time(), "%H:%M:%S"), "|", ..., "\n")
guardar_tabla <- function(dt, nombre) {
  fwrite(dt, file.path(DIR_TABLAS, paste0(nombre, ".csv")))
  invisible(dt)
}

## 5. Funciones de evaluación compartidas ----------------------------------------
auc_rapido <- function(y, p) {
  ok <- is.finite(p) & !is.na(y); y <- y[ok]; p <- p[ok]
  n1 <- sum(y == 1); n0 <- sum(y == 0)
  if (n1 == 0 || n0 == 0) return(NA_real_)
  r <- frank(p, ties.method = "average")
  (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}
log_loss <- function(y, p, eps = 1e-6) { p <- pmin(pmax(p, eps), 1 - eps); -mean(y * log(p) + (1 - y) * log(1 - p)) }
brier    <- function(y, p) mean((p - y)^2)
perdida_log_obs <- function(y, p, eps = 1e-6) { p <- pmin(pmax(p, eps), 1 - eps); -(y * log(p) + (1 - y) * log(1 - p)) }
