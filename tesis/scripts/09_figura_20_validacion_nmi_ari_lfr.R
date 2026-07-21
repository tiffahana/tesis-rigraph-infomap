# Figura 20: resultados de validaciÃ³n NMI y ARI en la red LFR
# Tesis de Daniela Salinas Castro

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

archivo_metricas <- file.path(
  base_dir,
  "datos", "lfr", "jerarquia_reforzada",
  "metricas_validacion_lfr.csv"
)

if (!file.exists(archivo_metricas)) {
  stop(
    paste0(
      "No se encontrÃ³ el archivo:\n",
      archivo_metricas,
      "\nRevisa que la carpeta principal del proyecto sea correcta."
    )
  )
}

metricas_lfr <- read.csv(
  archivo_metricas,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

columnas_esperadas <- c("comparacion", "NMI", "ARI")

if (!all(columnas_esperadas %in% names(metricas_lfr))) {
  stop(
    paste0(
      "El archivo no contiene las columnas esperadas.\n",
      "Columnas encontradas: ",
      paste(names(metricas_lfr), collapse = ", ")
    )
  )
}

if (nrow(metricas_lfr) < 2) {
  stop("El archivo debe contener las comparaciones macro y micro.")
}

etiquetas <- c(
  "NMI\nmacro",
  "ARI\nmacro",
  "NMI\nmicro",
  "ARI\nmicro"
)

valores <- c(
  metricas_lfr$NMI[1],
  metricas_lfr$ARI[1],
  metricas_lfr$NMI[2],
  metricas_lfr$ARI[2]
)

salida_dir <- file.path(base_dir, "figuras")
dir.create(salida_dir, showWarnings = FALSE, recursive = TRUE)

archivo_png <- file.path(
  salida_dir,
  "figura_20_validacion_nmi_ari_lfr.png"
)

png(
  filename = archivo_png,
  width = 1800,
  height = 1200,
  res = 200
)

par(
  mar = c(6, 6, 4, 2) + 0.1,
  cex.axis = 1.1,
  cex.lab = 1.2,
  cex.main = 1.3
)

posiciones <- barplot(
  valores,
  names.arg = etiquetas,
  ylim = c(0, 1.12),
  ylab = "Valor de la mÃ©trica",
  main = "ValidaciÃ³n de la jerarquÃ­a detectada en la red LFR",
  las = 1
)

text(
  x = posiciones,
  y = valores,
  labels = format(valores, nsmall = 1),
  pos = 3,
  cex = 1.1
)

abline(
  h = 1,
  lty = 2
)

mtext(
  "Macro: comunidades conocidas vs. level_2 | Micro: comunidades conocidas vs. final_module",
  side = 1,
  line = 4.5,
  cex = 0.9
)

dev.off()

cat("Figura generada correctamente:\n")
cat(archivo_png, "\n")
