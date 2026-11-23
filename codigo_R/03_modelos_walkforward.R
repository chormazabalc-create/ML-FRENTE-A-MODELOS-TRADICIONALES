# =============================================================================
# 03_modelos_walkforward.R
# Estima los modelos con validación caminante (walk-forward) y reestimación anual:
#   entrenamiento 2015..(a-2) | validación (a-1) | prueba a,  a = 2020..2025
# Como en Gu, Kelly y Xiu (2020): la muestra de entrenamiento crece un año en cada
# reestimación, la de validación mantiene su tamaño (un año) y se desplaza, y los
# hiperparámetros se eligen solo con el error en validación. Entre bloques se
# purga el horizonte del pronóstico (las semanas cuyo objetivo cae en el bloque
# siguiente).
#
# Modelos: ingenuo (frecuencia histórica), logit simple ("tradicional"), logit con
# todos los predictores, logit elastic net, random forest, gradient boosting
# (XGBoost) y red neuronal de una capa (nnet, promedio de varias semillas).
#
# Entrada : datos/procesados/panel_semanal.rds
# Salidas : resultados/predicciones/predicciones_h{H}.rds   (fuera de muestra)
#           resultados/tablas/t04_hiperparametros_h{H}.csv
#           resultados/tablas/t05_importancia_variables_h{H}.csv
#           resultados/tablas/t06_shap_xgboost_h{H}.csv
# Parámetro: HORIZONTE (1 = semana siguiente; 4 = robustez). Por defecto 1.
# Tiempo aproximado (computador personal, 4 núcleos): 10 a 30 minutos.
# =============================================================================
if (!exists("DIR_CODIGO")) DIR_CODIGO <- local({
  a <- commandArgs(FALSE); f <- sub("^--file=", "", a[grep("^--file=", a)])
  if (length(f) == 1) dirname(normalizePath(f)) else
    if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable() &&
        nzchar(rstudioapi::getSourceEditorContext()$path))
      dirname(normalizePath(rstudioapi::getSourceEditorContext()$path)) else getwd()
})
source(file.path(DIR_CODIGO, "00_configuracion.R"), encoding = "UTF-8")
suppressPackageStartupMessages({ library(glmnet); library(ranger); library(xgboost); library(nnet) })

H <- if (exists("HORIZONTE")) HORIZONTE else 1L
SUF <- if (is.na(VENTANA_MOVIL_ANIOS)) sprintf("h%d", H) else sprintf("h%d_movil%d", H, VENTANA_MOVIL_ANIOS)
yvar <- paste0("y_", H); rvar <- paste0("ret_fwd_", H)
panel <- readRDS(file.path(DIR_DATOS_PROC, "panel_semanal.rds"))
VA <- attr(panel, "vars_accion"); VM <- attr(panel, "vars_mercado"); VP <- attr(panel, "vars_pais")
panel <- panel[is.finite(get(rvar))]
panel[, y := get(yvar)]

# Conjuntos de información
X_SIMPLE <- intersect(c("c_ret_1", "c_dvol_13", "m_fx_ret_1", "m_mkt_ret_1", VP), names(panel))
X_FULL   <- c(VA, VM, VP)
msg("Horizonte:", H, "semana(s) | observaciones:", nrow(panel), "| predictores:", length(X_FULL))

# Grillas de hiperparámetros (reducidas en modo rápido)
GRID_ENET <- data.table(alpha = if (MODO_RAPIDO) c(0.5) else c(0, 0.5, 1))
GRID_RF   <- CJ(mtry = if (MODO_RAPIDO) 6L else c(3L, 6L, 12L),
                min.node.size = if (MODO_RAPIDO) 200L else c(100L, 500L))
N_ARBOLES_RF <- if (MODO_RAPIDO) 100L else 300L
GRID_XGB  <- CJ(max_depth = if (MODO_RAPIDO) 2L else c(1L, 2L, 4L),
                eta = if (MODO_RAPIDO) 0.1 else 0.05,
                min_child_weight = if (MODO_RAPIDO) 50 else c(10, 100))
MAX_RONDAS_XGB <- if (MODO_RAPIDO) 200L else 1000L
GRID_NN   <- CJ(size = if (MODO_RAPIDO) 4L else c(4L, 8L), decay = if (MODO_RAPIDO) 0.1 else c(0.01, 0.1))
N_SEMILLAS_NN <- if (MODO_RAPIDO) 2L else 5L

## Funciones auxiliares ---------------------------------------------------------
matriz <- function(dt, vars, centro = NULL, escala = NULL) {
  X <- as.matrix(dt[, ..vars]); storage.mode(X) <- "double"
  if (!is.null(centro)) X <- sweep(sweep(X, 2, centro, "-"), 2, escala, "/")
  X[!is.finite(X)] <- 0
  X
}
estandarizador <- function(dt, vars) {         # solo variables de mercado; el resto queda igual
  centro <- setNames(rep(0, length(vars)), vars); escala <- setNames(rep(1, length(vars)), vars)
  for (v in intersect(vars, VM)) {
    centro[v] <- mean(dt[[v]], na.rm = TRUE); s <- sd(dt[[v]], na.rm = TRUE)
    escala[v] <- if (is.finite(s) && s > 0) s else 1
  }
  list(centro = centro, escala = escala)
}
quitar_constantes <- function(X) X[, apply(X, 2, function(z) length(unique(z)) > 1), drop = FALSE]

ajustar_logit <- function(X, y) {
  X <- quitar_constantes(X)
  if (any(grepl("^d_", colnames(X)))) {        # una categoría de país como base
    dps <- grep("^d_", colnames(X), value = TRUE); X <- X[, setdiff(colnames(X), dps[1]), drop = FALSE]
  }
  f <- suppressWarnings(glm.fit(cbind(1, X), y, family = binomial()))
  b <- f$coefficients; b[!is.finite(b)] <- 0
  list(b = b, vars = colnames(X))
}
predecir_logit <- function(m, X) as.numeric(plogis(cbind(1, X[, m$vars, drop = FALSE]) %*% m$b))

ultimo_log_xgb <- function(m) {
  el <- tryCatch(m$evaluation_log, error = function(e) NULL)
  if (is.null(el)) el <- attributes(m)$evaluation_log
  el
}
entrenar_xgb <- function(Xtr, ytr, params, nrounds, Xva = NULL, yva = NULL) {
  dtr <- xgb.DMatrix(Xtr, label = ytr)
  args <- list(params = c(params, list(objective = "binary:logistic", eval_metric = "logloss",
                                       subsample = 0.8, colsample_bytree = 0.8, nthread = N_HILOS)),
               data = dtr, nrounds = nrounds, verbose = 0)
  if (!is.null(Xva)) {
    ev <- list(validacion = xgb.DMatrix(Xva, label = yva))
    if ("evals" %in% names(formals(xgb.train))) args$evals <- ev else args$watchlist <- ev
    args$early_stopping_rounds <- 50
  }
  set.seed(SEMILLA)
  do.call(xgb.train, args)
}

importancia_anulacion <- function(predictor, Xte, yte, vars) {
  # Gu, Kelly y Xiu (2020): pérdida de ajuste al fijar en cero cada predictor
  # (cero = mediana del corte transversal para las características normalizadas).
  base <- auc_rapido(yte, predictor(Xte))
  vapply(vars, function(v) { Xp <- Xte; Xp[, v] <- 0; base - auc_rapido(yte, predictor(Xp)) }, 0)
}

## Bucle walk-forward --------------------------------------------------------------
pred_list <- list(); hp_list <- list(); imp_list <- list(); shap_list <- list()
for (a in ANIOS_PRUEBA) {
  t0 <- Sys.time()
  ini_val <- panel[anio == a - 1, min(semana)]; ini_test <- panel[anio == a, min(semana)]
  anio_ini <- if (is.na(VENTANA_MOVIL_ANIOS)) ANIO_INICIO_ENTREN else max(ANIO_INICIO_ENTREN, a - 1 - VENTANA_MOVIL_ANIOS)
  tr <- panel[anio >= anio_ini & anio <= a - 2 & semana + 7 * H < ini_val]
  va <- panel[anio == a - 1 & semana + 7 * H < ini_test]
  te <- panel[anio == a]
  if (!nrow(te)) next
  msg(sprintf("Año de prueba %d | entrenamiento %d obs. (%d-%d) | validación %d | prueba %d",
              a, nrow(tr), anio_ini, a - 2, nrow(va), nrow(te)))
  est <- estandarizador(tr, X_FULL)
  Xtr <- matriz(tr, X_FULL, est$centro, est$escala); Xva <- matriz(va, X_FULL, est$centro, est$escala)
  Xte <- matriz(te, X_FULL, est$centro, est$escala)
  ytr <- tr$y; yva <- va$y; yte <- te$y
  P <- list()

  # (0) Ingenuo: frecuencia histórica de semanas al alza en entrenamiento
  P$ingenuo <- rep(mean(ytr), nrow(te))

  # (1) Logit simple: el benchmark "tradicional" (retorno rezagado, volumen, FX, mercado, país)
  m1 <- ajustar_logit(Xtr[, X_SIMPLE, drop = FALSE], ytr)
  P$logit_simple <- predecir_logit(m1, Xte)

  # (2) Logit con todos los predictores, sin regularizar
  m2 <- ajustar_logit(Xtr, ytr)
  P$logit_completo <- predecir_logit(m2, Xte)
  f_logit <- function(X) predecir_logit(m2, X)

  # (3) Elastic net: alpha y lambda por log-loss en validación
  mejor <- list(ll = Inf)
  for (al in GRID_ENET$alpha) {
    fit <- glmnet(Xtr, ytr, family = "binomial", alpha = al, nlambda = 60, lambda.min.ratio = 1e-4)
    pv <- predict(fit, newx = Xva, type = "response")
    ll <- apply(pv, 2, function(p) log_loss(yva, p))
    j <- which.min(ll)
    if (ll[j] < mejor$ll) mejor <- list(ll = ll[j], fit = fit, lambda = fit$lambda[j], alpha = al)
  }
  f_enet <- function(X) as.numeric(predict(mejor$fit, newx = X, s = mejor$lambda, type = "response"))
  P$elastic_net <- f_enet(Xte)
  n_activas <- sum(as.numeric(coef(mejor$fit, s = mejor$lambda))[-1] != 0)
  hp_list[[length(hp_list) + 1]] <- data.table(anio_prueba = a, modelo = "elastic_net",
    hiperparametros = sprintf("alpha=%.2f; lambda=%.5f; variables activas=%d", mejor$alpha, mejor$lambda, n_activas),
    logloss_validacion = mejor$ll)

  # (4) Random forest (ranger)
  dtr <- data.frame(Xtr, check.names = FALSE); dtr$y <- factor(ytr, levels = c(0, 1))
  mejor_rf <- list(ll = Inf)
  for (g in seq_len(nrow(GRID_RF))) {
    rf <- ranger(y ~ ., data = dtr, probability = TRUE, num.trees = N_ARBOLES_RF,
                 mtry = min(GRID_RF$mtry[g], ncol(Xtr)), min.node.size = GRID_RF$min.node.size[g],
                 num.threads = N_HILOS, seed = SEMILLA)
    pv <- predict(rf, data = data.frame(Xva, check.names = FALSE))$predictions[, "1"]
    ll <- log_loss(yva, pv)
    if (ll < mejor_rf$ll) mejor_rf <- list(ll = ll, rf = rf, g = g)
  }
  f_rf <- function(X) predict(mejor_rf$rf, data = data.frame(X, check.names = FALSE))$predictions[, "1"]
  P$random_forest <- f_rf(Xte)
  hp_list[[length(hp_list) + 1]] <- data.table(anio_prueba = a, modelo = "random_forest",
    hiperparametros = sprintf("árboles=%d; mtry=%d; min.node.size=%d", N_ARBOLES_RF,
                              GRID_RF$mtry[mejor_rf$g], GRID_RF$min.node.size[mejor_rf$g]),
    logloss_validacion = mejor_rf$ll)

  # (5) Gradient boosting (XGBoost): parada temprana en validación y reajuste con esas rondas
  mejor_xgb <- list(ll = Inf)
  for (g in seq_len(nrow(GRID_XGB))) {
    pr <- as.list(GRID_XGB[g])
    m <- entrenar_xgb(Xtr, ytr, pr, MAX_RONDAS_XGB, Xva, yva)
    el <- ultimo_log_xgb(m); k <- which.min(el[[2]])
    if (el[[2]][k] < mejor_xgb$ll) mejor_xgb <- list(ll = el[[2]][k], params = pr, rondas = k)
  }
  m_xgb <- entrenar_xgb(Xtr, ytr, mejor_xgb$params, mejor_xgb$rondas)
  f_xgb <- function(X) predict(m_xgb, xgb.DMatrix(X))
  P$xgboost <- f_xgb(Xte)
  hp_list[[length(hp_list) + 1]] <- data.table(anio_prueba = a, modelo = "xgboost",
    hiperparametros = sprintf("max_depth=%d; eta=%.2f; min_child_weight=%g; rondas=%d",
                              mejor_xgb$params$max_depth, mejor_xgb$params$eta,
                              mejor_xgb$params$min_child_weight, mejor_xgb$rondas),
    logloss_validacion = mejor_xgb$ll)
  sh <- predict(m_xgb, xgb.DMatrix(Xte), predcontrib = TRUE)
  shap_list[[length(shap_list) + 1]] <- data.table(anio_prueba = a, variable = X_FULL,
                                                   shap_medio_abs = colMeans(abs(sh[, seq_along(X_FULL), drop = FALSE])))

  # (6) Red neuronal de una capa oculta (nnet), promedio de varias semillas
  nn_fit <- function(size, decay) lapply(seq_len(N_SEMILLAS_NN), function(s) {
    set.seed(SEMILLA + s)
    nnet(Xtr, ytr, size = size, decay = decay, entropy = TRUE, maxit = 300, trace = FALSE, MaxNWts = 20000)
  })
  nn_pred <- function(ms, X) rowMeans(sapply(ms, function(m) as.numeric(predict(m, X))))
  mejor_nn <- list(ll = Inf)
  for (g in seq_len(nrow(GRID_NN))) {
    ms <- nn_fit(GRID_NN$size[g], GRID_NN$decay[g])
    ll <- log_loss(yva, nn_pred(ms, Xva))
    if (ll < mejor_nn$ll) mejor_nn <- list(ll = ll, ms = ms, g = g)
  }
  f_nn <- function(X) nn_pred(mejor_nn$ms, X)
  P$red_neuronal <- f_nn(Xte)
  hp_list[[length(hp_list) + 1]] <- data.table(anio_prueba = a, modelo = "red_neuronal",
    hiperparametros = sprintf("neuronas=%d; decay=%g; semillas=%d", GRID_NN$size[mejor_nn$g],
                              GRID_NN$decay[mejor_nn$g], N_SEMILLAS_NN),
    logloss_validacion = mejor_nn$ll)

  # Importancia de variables en el año de prueba
  for (nm in c("logit_completo", "elastic_net", "random_forest", "xgboost", "red_neuronal")) {
    fn <- switch(nm, logit_completo = f_logit, elastic_net = f_enet, random_forest = f_rf,
                 xgboost = f_xgb, red_neuronal = f_nn)
    imp_list[[length(imp_list) + 1]] <- data.table(anio_prueba = a, modelo = nm, variable = X_FULL,
                                                   caida_auc = importancia_anulacion(fn, Xte, yte, X_FULL))
  }

  pred_list[[length(pred_list) + 1]] <- rbindlist(lapply(names(P), function(nm)
    te[, .(semana, anio, pais, ticker, y, ret_fwd = get(rvar), amihud_63, ceros_63, cs_63, dvol_63,
           modelo = nm, p = P[[nm]])]))
  msg(sprintf("  listo en %.1f min", as.numeric(difftime(Sys.time(), t0, units = "mins"))))
}

pred <- rbindlist(pred_list); pred[, anio_prueba := anio]
saveRDS(pred, file.path(DIR_PREDICCIONES, sprintf("predicciones_%s.rds", SUF)))
guardar_tabla(rbindlist(hp_list), sprintf("t04_hiperparametros_%s", SUF))
imp <- rbindlist(imp_list)
imp[, caida_auc_norm := pmax(caida_auc, 0) / sum(pmax(caida_auc, 0)), by = .(anio_prueba, modelo)]
guardar_tabla(imp, sprintf("t05_importancia_variables_%s", SUF))
guardar_tabla(rbindlist(shap_list), sprintf("t06_shap_xgboost_%s", SUF))
msg("Predicciones fuera de muestra:", nrow(pred) / uniqueN(pred$modelo), "por modelo.",
    "Siguiente paso: 04_evaluacion_pruebas.R")
