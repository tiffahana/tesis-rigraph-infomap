# Figura 22: resultado multinivel de USairports
# Tesis de Daniela Salinas Castro
#
# Este script utiliza el CSV ya generado por la prueba final para evitar
# volver a ejecutar Infomap y conservar exactamente los resultados del informe:
# 745 nodos, 5 mÃ³dulos superiores en level_2 y 52 comunidades finales.

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

archivos_encontrados <- list.files(
  path = base_dir,
  pattern = "^usairports_multilevel_modules_modificado\\.csv$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)

if (length(archivos_encontrados) == 0) {
  stop(
    paste0(
      "No se encontrÃ³ usairports_multilevel_modules_modificado.csv dentro de:\n",
      base_dir
    )
  )
}

fechas <- file.info(archivos_encontrados)$mtime
archivo_csv <- archivos_encontrados[which.max(fechas)]

multinivel <- read.csv(
  archivo_csv,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

columnas_esperadas <- c("node_id", "level_2", "final_module")

if (!all(columnas_esperadas %in% names(multinivel))) {
  stop(
    paste0(
      "El archivo no contiene todas las columnas esperadas.\n",
      "Columnas encontradas: ",
      paste(names(multinivel), collapse = ", ")
    )
  )
}

nodos_por_supermodulo <- table(multinivel$level_2)

relacion <- unique(
  multinivel[c("level_2", "final_module")]
)

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

if (cantidad_nodos != 745) {
  warning(
    paste0(
      "Se esperaban 745 nodos, pero el archivo contiene ",
      cantidad_nodos,
      ". Revisa que corresponda a la ejecuciÃ³n final."
    )
  )
}

if (cantidad_supermodulos != 5) {
  warning(
    paste0(
      "Se esperaban 5 mÃ³dulos superiores en level_2, pero se encontraron ",
      cantidad_supermodulos,
      "."
    )
  )
}

if (cantidad_comunidades != 52) {
  warning(
    paste0(
      "Se esperaban 52 comunidades finales, pero se encontraron ",
      cantidad_comunidades,
      "."
    )
  )
}

salida_dir <- file.path(base_dir, "figuras")
dir.create(salida_dir, showWarnings = FALSE, recursive = TRUE)

archivo_png <- file.path(
  salida_dir,
  "figura_22_resultado_multinivel_usairports.png"
)

archivo_txt <- file.path(
  salida_dir,
  "figura_22_resumen_usairports.txt"
)

paleta <- grDevices::hcl.colors(
  n = cantidad_supermodulos,
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
  height = nodos,
  names.arg = paste("MÃ³dulo", modulos),
  col = paleta,
  border = "gray30",
  ylim = c(0, max(nodos) * 1.18),
  ylab = "Cantidad de nodos",
  xlab = "MÃ³dulos superiores de level_2",
  main = "TamaÃ±o de los mÃ³dulos superiores",
  las = 1,
  cex.names = 0.9
)

text(
  x = pos_nodos,
  y = nodos,
  labels = nodos,
  pos = 3,
  cex = 1.05
)

pos_comunidades <- barplot(
  height = comunidades,
  names.arg = paste("MÃ³dulo", modulos),
  col = paleta,
  border = "gray30",
  ylim = c(0, max(comunidades) * 1.22),
  ylab = "Cantidad de comunidades finales",
  xlab = "MÃ³dulos superiores de level_2",
  main = "Comunidades finales agrupadas",
  las = 1,
  cex.names = 0.9
)

text(
  x = pos_comunidades,
  y = comunidades,
  labels = comunidades,
  pos = 3,
  cex = 1.05
)

mtext(
  "Resultado multinivel de la red USairports",
  outer = TRUE,
  side = 3,
  line = 1.8,
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
  line = 0.3,
  cex = 1.05
)

dev.off()

capture.output(
  {
    cat("RESULTADO MULTINIVEL DE USAIRPORTS\n")
    cat("=================================\n\n")

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
