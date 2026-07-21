# Figura 31: comparaciÃ³n entre igraph original e igraph modificado
# Tesis de Daniela Salinas Castro
#
# La figura compara la cantidad de comunidades finales obtenidas por:
#   - igraph original
#   - versiÃ³n local modificada de cluster_infomap()
#
# Se utilizan tres paneles independientes para evitar que las diferencias
# de escala entre Zachary, USairports y Yeast oculten los valores pequeÃ±os.

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
  "figura_31_igraph_original_vs_modificado.png"
)

archivo_csv <- file.path(
  salida_dir,
  "figura_31_igraph_original_vs_modificado.csv"
)

archivo_txt <- file.path(
  salida_dir,
  "figura_31_resumen_igraph_original_vs_modificado.txt"
)

# Resultados finales documentados en la memoria
resultados <- data.frame(
  red = c(
    "Zachary Karate Club",
    "USairports",
    "Yeast"
  ),
  igraph_original = c(
    3,
    52,
    201
  ),
  igraph_modificado = c(
    3,
    52,
    203
  ),
  misma_particion = c(
    "SÃ­",
    "SÃ­",
    "No"
  ),
  interpretacion = c(
    "Resultado final conservado",
    "Resultado final conservado",
    "Resultado comparable, no idÃ©ntico"
  ),
  stringsAsFactors = FALSE
)

write.csv(
  resultados,
  archivo_csv,
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# Colores definidos de forma estable para distinguir implementaciones.
colores <- c(
  "gray75",
  "gray30"
)

png(
  filename = archivo_png,
  width = 2600,
  height = 1500,
  res = 220
)

par(
  mfrow = c(1, 3),
  mar = c(6, 5, 6, 2),
  oma = c(2, 1, 5, 1)
)

for (i in seq_len(nrow(resultados))) {
  valores <- c(
    resultados$igraph_original[i],
    resultados$igraph_modificado[i]
  )

  diferencia <- resultados$igraph_modificado[i] -
    resultados$igraph_original[i]

  limite_y <- max(valores) * 1.25

  if (limite_y == 0) {
    limite_y <- 1
  }

  posiciones <- barplot(
    height = valores,
    names.arg = c(
      "Original",
      "Modificado"
    ),
    col = colores,
    border = "gray20",
    ylim = c(0, limite_y),
    ylab = "Comunidades finales",
    main = paste0(
      resultados$red[i],
      "\nMisma particiÃ³n: ",
      resultados$misma_particion[i]
    ),
    las = 1,
    cex.names = 0.95,
    cex.axis = 0.95,
    cex.lab = 1.0,
    cex.main = 1.05
  )

  text(
    x = posiciones,
    y = valores,
    labels = valores,
    pos = 3,
    cex = 1.05,
    font = 2
  )

  mtext(
    paste0(
      "Diferencia: ",
      ifelse(
        diferencia > 0,
        paste0("+", diferencia),
        diferencia
      )
    ),
    side = 1,
    line = 4.3,
    cex = 0.90,
    font = 3
  )
}

mtext(
  "ComparaciÃ³n entre igraph original e igraph modificado",
  outer = TRUE,
  side = 3,
  line = 2.0,
  cex = 1.45,
  font = 2
)

mtext(
  "Cantidad de comunidades finales obtenidas en cada red",
  outer = TRUE,
  side = 3,
  line = 0.5,
  cex = 1.05
)

dev.off()

capture.output(
  {
    cat("COMPARACIÃ“N ENTRE IGRAPH ORIGINAL E IGRAPH MODIFICADO\n")
    cat("====================================================\n\n")

    print(
      resultados,
      row.names = FALSE
    )

    cat(
      "\nZachary y USairports conservaron la misma particiÃ³n final.\n"
    )

    cat(
      "Yeast presentÃ³ una diferencia de dos comunidades finales, ",
      "con calidad comparable pero sin equivalencia exacta.\n",
      sep = ""
    )
  },
  file = archivo_txt
)

cat(
  "Figura generada correctamente:\n",
  archivo_png,
  "\n\n",
  "Tabla CSV generada correctamente:\n",
  archivo_csv,
  "\n\n",
  "Resumen generado correctamente:\n",
  archivo_txt,
  "\n",
  sep = ""
)
