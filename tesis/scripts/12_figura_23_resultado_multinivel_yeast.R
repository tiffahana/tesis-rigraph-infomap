# Figura 23: resultado multinivel de Yeast
# Tesis de Daniela Salinas Castro
#
# La figura mantiene la misma lÃ³gica de la Figura 22:
# 1) cantidad de nodos por mÃ³dulo superior de level_2;
# 2) cantidad de comunidades finales agrupadas en cada mÃ³dulo superior.
#
# En Yeast se utilizan barras horizontales porque existen 15 mÃ³dulos
# superiores y asÃ­ las etiquetas se mantienen legibles.

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

# Buscar el archivo final de la ejecuciÃ³n con 500 trials.
archivos_encontrados <- list.files(
  path = base_dir,
  pattern = "^yeast_multilevel_modules_modificado_500\\.csv$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)

# Respaldo por si el archivo fue guardado sin el sufijo "_500".
if (length(archivos_encontrados) == 0) {
  archivos_encontrados <- list.files(
    path = base_dir,
    pattern = "^yeast_multilevel_modules_modificado.*\\.csv$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )
}

if (length(archivos_encontrados) == 0) {
  stop(
    paste0(
      "No se encontrÃ³ un archivo CSV de multilevel_modules para Yeast dentro de:\n",
      base_dir
    )
  )
}

# Si existen varias copias, utilizar la modificada mÃ¡s recientemente.
fechas <- file.info(archivos_encontrados)$mtime
archivo_csv <- archivos_encontrados[which.max(fechas)]

multinivel <- read.csv(
  archivo_csv,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

columnas_esperadas <- c(
  "node_id",
  "level_2",
  "final_module"
)

if (!all(columnas_esperadas %in% names(multinivel))) {
  stop(
    paste0(
      "El archivo no contiene todas las columnas esperadas.\n",
      "Columnas encontradas: ",
      paste(names(multinivel), collapse = ", ")
    )
  )
}

# -------------------------------------------------------------------
# Resumir la jerarquÃ­a detectada
# -------------------------------------------------------------------

# Cantidad de nodos en cada mÃ³dulo superior.
nodos_por_supermodulo <- table(multinivel$level_2)

# RelaciÃ³n Ãºnica entre cada comunidad final y su mÃ³dulo superior.
relacion <- unique(
  multinivel[c("level_2", "final_module")]
)

# Cantidad de comunidades finales contenidas en cada mÃ³dulo superior.
comunidades_por_supermodulo <- table(relacion$level_2)

modulos <- sort(unique(multinivel$level_2))

nodos <- as.numeric(
  nodos_por_supermodulo[as.character(modulos)]
)

comunidades <- as.numeric(
  comunidades_por_supermodulo[as.character(modulos)]
)

cantidad_nodos <- nrow(multinivel)
cantidad_supermodulos <- length(modulos)
cantidad_comunidades <- length(unique(multinivel$final_module))

# Validaciones con los resultados finales documentados.
if (cantidad_nodos != 2375) {
  warning(
    paste0(
      "Se esperaban 2375 nodos, pero el archivo contiene ",
      cantidad_nodos,
      ". Revisa que corresponda a la ejecuciÃ³n final."
    )
  )
}

if (cantidad_supermodulos != 15) {
  warning(
    paste0(
      "Se esperaban 15 mÃ³dulos superiores en level_2, pero se encontraron ",
      cantidad_supermodulos,
      "."
    )
  )
}

if (cantidad_comunidades != 203) {
  warning(
    paste0(
      "Se esperaban 203 comunidades finales, pero se encontraron ",
      cantidad_comunidades,
      "."
    )
  )
}

# Ordenar los mÃ³dulos de mayor a menor cantidad de nodos.
orden <- order(nodos, decreasing = FALSE)

modulos_ordenados <- modulos[orden]
nodos_ordenados <- nodos[orden]
comunidades_ordenadas <- comunidades[orden]

salida_dir <- file.path(base_dir, "figuras")
dir.create(salida_dir, showWarnings = FALSE, recursive = TRUE)

archivo_png <- file.path(
  salida_dir,
  "figura_23_resultado_multinivel_yeast.png"
)

archivo_txt <- file.path(
  salida_dir,
  "figura_23_resumen_yeast.txt"
)

paleta <- grDevices::hcl.colors(
  n = cantidad_supermodulos,
  palette = "Set 2"
)

paleta_ordenada <- paleta[orden]

# -------------------------------------------------------------------
# Generar figura
# -------------------------------------------------------------------
png(
  filename = archivo_png,
  width = 2600,
  height = 1800,
  res = 200
)

par(
  mfrow = c(1, 2),
  mar = c(5, 8, 5, 2),
  oma = c(2, 1, 5, 1)
)

pos_nodos <- barplot(
  height = nodos_ordenados,
  names.arg = paste("MÃ³dulo", modulos_ordenados),
  horiz = TRUE,
  col = paleta_ordenada,
  border = "gray30",
  xlim = c(0, max(nodos_ordenados) * 1.20),
  xlab = "Cantidad de nodos",
  main = "TamaÃ±o de los mÃ³dulos superiores",
  las = 1,
  cex.names = 0.85
)

text(
  x = nodos_ordenados,
  y = pos_nodos,
  labels = nodos_ordenados,
  pos = 4,
  cex = 0.9
)

pos_comunidades <- barplot(
  height = comunidades_ordenadas,
  names.arg = paste("MÃ³dulo", modulos_ordenados),
  horiz = TRUE,
  col = paleta_ordenada,
  border = "gray30",
  xlim = c(0, max(comunidades_ordenadas) * 1.24),
  xlab = "Cantidad de comunidades finales",
  main = "Comunidades finales agrupadas",
  las = 1,
  cex.names = 0.85
)

text(
  x = comunidades_ordenadas,
  y = pos_comunidades,
  labels = comunidades_ordenadas,
  pos = 4,
  cex = 0.9
)

mtext(
  "Resultado multinivel de la red Yeast",
  outer = TRUE,
  side = 3,
  line = 2.3,
  cex = 1.45,
  font = 2
)

mtext(
  paste0(
    cantidad_nodos,
    " nodos | ",
    cantidad_supermodulos,
    " mÃ³dulos superiores | ",
    cantidad_comunidades,
    " comunidades finales"
  ),
  outer = TRUE,
  side = 3,
  line = 0.7,
  cex = 1.05
)

dev.off()

# -------------------------------------------------------------------
# Guardar resumen textual
# -------------------------------------------------------------------
capture.output(
  {
    cat("RESULTADO MULTINIVEL DE YEAST\n")
    cat("=============================\n\n")

    cat("Archivo utilizado:\n")
    cat(archivo_csv, "\n\n")

    cat("Cantidad de nodos:", cantidad_nodos, "\n")
    cat("MÃ³dulos superiores en level_2:", cantidad_supermodulos, "\n")
    cat("Comunidades finales:", cantidad_comunidades, "\n\n")

    resumen <- data.frame(
      modulo_superior = modulos,
      nodos = nodos,
      comunidades_finales_agrupadas = comunidades
    )

    print(resumen, row.names = FALSE)
  },
  file = archivo_txt
)

cat("Figura generada correctamente:\n")
cat(archivo_png, "\n\n")

cat("Resumen generado correctamente:\n")
cat(archivo_txt, "\n\n")

cat("Archivo de entrada utilizado:\n")
cat(archivo_csv, "\n")
