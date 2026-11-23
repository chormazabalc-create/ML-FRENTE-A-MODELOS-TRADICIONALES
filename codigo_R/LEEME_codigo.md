# Código de replicación en R

Esta carpeta contiene el flujo completo que produce los resultados del documento de trabajo de la tesis, y la base de datos con la que se produjeron.

## Qué hay

- `00_configuracion.R` a `07_robustez.R` y `ejecutar_todo.R`: los programas, en el orden en que se ejecutan.  
- `codigo_R.Rproj`: proyecto de RStudio (fija la carpeta de trabajo).  
- `datos/crudos/yahoo_diario_20261009.csv.gz`: base cruda congelada, descargada de Yahoo Finance el 9 de octubre de 2026 (precios diarios de apertura, máximo, mínimo, cierre y cierre ajustado, y volumen, de las acciones, los índices, los tipos de cambio y las variables globales). `yahoo_log_20261009.csv` registra qué series se descargaron y con qué cobertura.  
- Las carpetas `datos/procesados/` y `resultados/` (tablas CSV, figuras PNG y predicciones) se crean al ejecutar el flujo.

## Cómo correrlo

1. Abrir `codigo_R.Rproj` en RStudio.  
2. **No es necesario ejecutar `01_descarga_yahoo.R`:** la base congelada ya está en `datos/crudos/`. Si lo ejecutan, se creará una base con otra fecha, el flujo usará la más reciente y las cifras dejarán de coincidir con las del documento (Yahoo Finance reajusta los precios hacia atrás con cada dividendo y solo conserva las empresas listadas hoy).  
3. Para reproducir todas las tablas del documento, incluida la robustez con ventana de entrenamiento móvil de cinco años, escribir en la consola `CORRER_MOVIL <- TRUE` y luego ejecutara con **Source**. Corre, sin internet, los pasos 02 a 07 y deja las tablas en `resultados/tablas/` (CSV) y las figuras en `resultados/figuras/` (PNG). La corrida completa toma alrededor de una hora y media en un computador de dos núcleos.  
   - Prueba rápida del código antes de la corrida completa: en la consola `Sys.setenv(TESIS_MODO_RAPIDO = "1")` y luego Source de `ejecutar_todo.R` (grillas y árboles reducidos; los resultados de esa prueba no son los del documento). Para la corrida definitiva, `Sys.setenv(TESIS_MODO_RAPIDO = "0")`.  
   - Para omitir la robustez con horizonte de cuatro semanas, que repite la estimación de todos los modelos: `CORRER_H4 <- FALSE` antes de ejecutar.

Paquetes (se instalan solos si faltan): data.table, curl, jsonlite, glmnet, ranger, xgboost, pROC, ggplot2, sandwich, nnet. Opcional: quantmod, rstudioapi. La corrida presentada en la tesis se hizo con R 4.4.1 (ver sesion_R.txt); con otras versiones de R o de los paquetes pueden aparecer diferencias en los modelos que usan números aleatorios, en especial en la red neuronal.

## Scripts

| Script | Qué hace | Salidas principales |
| :---- | :---- | :---- |
| `00_configuracion.R` | Rutas, parámetros del diseño (fechas, años de prueba, filtros, costos), universo de tickers y funciones comunes. Lo cargan los demás; no se ejecuta solo. | — |
| `01_descarga_yahoo.R` | Descarga de Yahoo Finance (no necesario: la base ya está descargada). | `datos/crudos/` |
| `02_construccion_panel.R` | Panel acción-semana (viernes), 21 características por acción normalizadas por rango en \[−1, 1\] dentro de cada país y semana, 13 variables de mercado y globales, variable dependiente (dirección de *t* \+ 1 y de *t* \+ 1 a *t* \+ 4), filtros de elegibilidad. | `datos/procesados/panel_semanal.rds`, `diccionario_variables.csv`, t01–t03 |
| `03_modelos_walkforward.R` | Validación caminante 2020–2025 (entrenamiento 2015 a *a* − 2, validación *a* − 1, prueba *a*) con purga; modelos: ingenuo, logit simple, logit completo, *elastic net*, *random forest*, XGBoost, red neuronal. Importancia de variables y SHAP. | `resultados/predicciones/`, t04–t06 |
| `04_evaluacion_pruebas.R` | AUC, exactitud, Brier, log-loss, F1, G-mean y MCC; Pesaran-Timmermann; Diebold-Mariano con promedio de corte transversal y Newey-West; Clark-West; figura de importancia de variables. | t07–t12, f01, f04 |
| `05_carteras_costos.R` | Quintil superior de probabilidad por país, igual ponderación, contra la cartera equiponderada; costos de 50/100/150 pb y spread de Corwin-Schultz; estadístico *t* de Newey-West del retorno activo; costo de equilibrio. | t13–t14, f02 |
| `06_heterogeneidad_liquidez.R` | Terciles de Amihud, retornos cero y spread; ventaja del ML por tercil, diferencia T3 − T1 y ventaja por país. | t15–t18 (incluye t16b), f03 |
| `07_robustez.R` | 2020, 2022 y resto de los años; sin retornos cero en *t* \+ 1; horizonte de cuatro semanas; ventana de entrenamiento móvil de cinco años (con `CORRER_MOVIL <- TRUE`). | t19–t20 y tablas con sufijo `_h4` / `_h1_movil5` |
| `ejecutar_todo.R` | Corre 02 a 07 en orden y guarda `sessionInfo()`. | `resultados/sesion_R.txt` |

&nbsp;

## Correspondencia con el documento

Las tablas del capítulo 5 se arman a partir de las tablas CSV de `resultados/tablas/`: t01 y t02 (tablas 5.1 y 5.2), t07 (5.3), t10 (5.4), t11 y t12 (5.5), t08 (5.6), t11 (5.7), t05 y t06 (5.8), t15, t16 y t17 (5.9), t09 y t16b (5.10), t13 (5.11 y 5.12), t14 y t18 (5.13), t19, t20 y las tablas con sufijo `_h4` y `_h1_movil5` (5.14), t04 (anexo B), y t03, t08, t09, t10, t11 y t15 (anexo C). Las figuras 5.1 a 5.4 son f01, f04, f03 y f02 de `resultados/figuras/`.

## Advertencias

- Yahoo Finance solo conserva las empresas listadas hoy (sesgo de supervivencia) y reajusta los precios hacia atrás con cada dividendo; por eso la base se congela con la fecha de descarga y todos los resultados se citan con esa fecha. En la descarga del 9 de octubre de 2026 se obtuvieron 103 de las 118 acciones del universo (ver el anexo A del documento y `yahoo_log_20261009.csv`).  
- Los retornos semanales iguales a cero (semanas sin transacciones) cuentan como «no sube»; `07_robustez.R` repite la evaluación excluyéndolos.  
- Los comentarios iniciales de algunos programas mencionan que el código es el del profesor guía: es el mismo flujo que se entrega aquí.