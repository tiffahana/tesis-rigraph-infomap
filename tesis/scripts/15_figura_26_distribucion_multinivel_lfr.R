# Figura 26: distribuciÃ³n de nodos y comunidades finales en LFR
# Tesis de Daniela Salinas Castro
#
# Genera:
# 1) grÃ¡fico de barras con dos paneles;
# 2) tabla CSV con el resumen por mÃ³dulo superior;
# 3) resumen de texto para verificaciÃ³n.

repo_dir <- normalizePath(
  getwd(),
  winslash = "/",
  mustWork = TRUE
)

if (!file.exists(file.path(repo_dir, "DESCRIPTION")) ||
    !dir.exists(file.path(repo_dir, "tesis"))) {
  stop(
    "Ejecute este script desde la raíz del repositorio ",
    "tesis-rigraph-infomap."
  )
}

base_dir <- file.path(repo_dir, "tesis")

archivo_csv <- file.path(
  base_dir,
  "datos", "lfr", "jerarquia_reforzada",
  "multilevel_modules_lfr_modificado.csv"
)

if (!file.exists(archivo_csv)) {
  stop(
    paste0(
      "No se encontrÃ³ el archivo final de LFR:\n",
      archivo_csv
    )
  )
}

multinivel_lfr <- read.csv(
  archivo_csv,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

columnas_esperadas <- c(
  "node_id",
  "level_2",
  "final_module"
)

if (!all(columnas_esperadas %in% names(multinivel_lfr))) {
  stop(
    paste0(
      "El archivo no contiene todas las columnas esperadas.\n",
      "Columnas encontradas: ",
      paste(names(multinivel_lfr), collapse = ", ")
    )
  )
}

# ------------------------------------------------------------
# Calcular la distribuciÃ³n por mÃ³dulo superior
# ------------------------------------------------------------

nodos_por_modulo <- table(
  multinivel_lfr$level_2
)

relacion_modulo_comunidad <- unique(
  multinivel_lfr[c("level_2", "final_module")]
)

comunidades_por_modulo <- table(
  relacion_modulo_comunidad$level_2
)

modulos <- sort(
  unique(multinivel_lfr$level_2)
)

cantidad_nodos <- as.numeric(
  nodos_por_modulo[as.character(modulos)]
)

cantidad_comunidades <- as.numeric(
  comunidades_por_modulo[as.character(modulos)]
)

tabla_distribucion <- data.frame(
  modulo_superior = modulos,
  nodos = cantidad_nodos,
  comunidades_finales_agrupadas = cantidad_comunidades
)

# ------------------------------------------------------------
# Verificar que corresponda al resultado final
# ------------------------------------------------------------

total_nodos <- nrow(multinivel_lfr)
total_modulos <- length(unique(multinivel_lfr$level_2))
total_comunidades <- length(unique(multinivel_lfr$final_module))

if (total_nodos != 5000) {
  warning(
    paste0(
      "Se esperaban 5000 nodos, pero se encontraron ",
      total_nodos,
      "."
    )
  )
}

if (total_modulos != 6) {
  warning(
    paste0(
      "Se esperaban 6 mÃ³dulos superiores, pero se encontraron ",
      total_modulos,
      "."
    )
  )
}

if (total_comunidades != 147) {
  warning(
    paste0(
      "Se esperaban 147 comunidades finales, pero se encontraron ",
      total_comunidades,
      "."
    )
  )
}

# ------------------------------------------------------------
# Crear archivos de salida
# ------------------------------------------------------------

salida_dir <- file.path(
  base_dir,
  "figuras"
)

dir.create(
  salida_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

archivo_png <- file.path(
  salida_dir,
  "figura_26_distribucion_multinivel_lfr.png"
)

archivo_tabla <- file.path(
  salida_dir,
  "figura_26_tabla_distribucion_lfr.csv"
)

archivo_txt <- file.path(
  salida_dir,
  "figura_26_resumen_distribucion_lfr.txt"
)

write.csv(
  tabla_distribucion,
  archivo_tabla,
  row.names = FALSE
)

# ------------------------------------------------------------
# Generar grÃ¡fico de dos paneles
# ------------------------------------------------------------

paleta <- grDevices::hcl.colors(
  n = total_modulos,
  palette = "Set 2"
)

png(
  filename = archivo_png,
  width = 2400,
  height = 1300,
  res = 200
)

par(
  mfrow = c(1, 2),
  mar = c(6, 6, 5, 2),
  oma = c(2, 1, 4, 1)
)

pos_nodos <- barplot(
  height = cantidad_nodos,
  names.arg = paste("MÃ³dulo", modulos),
  col = paleta,
  border = "gray30",
  ylim = c(0, max(cantidad_nodos) * 1.18),
  ylab = "Cantidad de nodos",
  xlab = "MÃ³dulos superiores de level_2",
  main = "TamaÃ±o de los mÃ³dulos superiores",
  las = 1,
  cex.names = 0.9
)

text(
  x = pos_nodos,
  y = cantidad_nodos,
  labels = cantidad_nodos,
  pos = 3,
  cex = 1.05
)

pos_comunidades <- barplot(
  height = cantidad_comunidades,
  names.arg = paste("MÃ³dulo", modulos),
  col = paleta,
  border = "gray30",
  ylim = c(0, max(cantidad_comunidades) * 1.22),
  ylab = "Cantidad de comunidades finales",
  xlab = "MÃ³dulos superiores de level_2",
  main = "Comunidades finales agrupadas",
  las = 1,
  cex.names = 0.9
)

text(
  x = pos_comunidades,
  y = cantidad_comunidades,
  labels = cantidad_comunidades,
  pos = 3,
  cex = 1.05
)

mtext(
  "Resultado multinivel de la red jerÃ¡rquica LFR",
  outer = TRUE,
  side = 3,
  line = 1.8,
  cex = 1.45,
  font = 2
)

mtext(
  paste0(
    total_nodos,
    " nodos | ",
    total_modulos,
    " mÃ³dulos superiores | ",
    total_comunidades,
    " comunidades finales"
  ),
  outer = TRUE,
  side = 3,
  line = 0.3,
  cex = 1.05
)

dev.off()

capture.output(
  {
    cat("DISTRIBUCIÃ“N MULTINIVEL DE LA RED LFR\n")
    cat("====================================\n\n")

    cat("Nodos totales:", total_nodos, "\n")
    cat("MÃ³dulos superiores:", total_modulos, "\n")
    cat("Comunidades finales:", total_comunidades, "\n\n")

    print(
      tabla_distribucion,
      row.names = FALSE
    )
  },
  file = archivo_txt
)

cat(
  "GrÃ¡fico generado:\n",
  archivo_png,
  "\n\n",
  "Tabla generada:\n",
  archivo_tabla,
  "\n\n",
  "Resumen generado:\n",
  archivo_txt,
  "\n",
  sep = ""
)
