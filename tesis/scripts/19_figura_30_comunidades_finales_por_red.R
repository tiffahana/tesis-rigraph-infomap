# Figura 30 (versiÃ³n corregida):
# comparaciÃ³n de comunidades finales por red
# Tesis de Daniela Salinas Castro
#
# CorrecciÃ³n: la cantidad de nodos se incorpora bajo el nombre de cada red
# para evitar que se superponga con el valor 203 correspondiente a Yeast.

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
  "figura_30_comunidades_finales_por_red_corregida.png"
)

archivo_csv <- file.path(
  salida_dir,
  "figura_30_comunidades_finales_por_red.csv"
)

archivo_txt <- file.path(
  salida_dir,
  "figura_30_resumen_comunidades_finales_por_red.txt"
)

resultados <- data.frame(
  red = c(
    "Zachary Karate Club",
    "USairports",
    "Yeast",
    "LFR"
  ),
  nodos = c(
    34,
    745,
    2375,
    5000
  ),
  comunidades_finales = c(
    3,
    52,
    203,
    147
  ),
  stringsAsFactors = FALSE
)

write.csv(
  resultados,
  archivo_csv,
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# barplot() horizontal dibuja desde abajo hacia arriba.
resultados_plot <- resultados[
  nrow(resultados):1,
  ,
  drop = FALSE
]

etiquetas_red <- paste0(
  resultados_plot$red,
  "\n(",
  resultados_plot$nodos,
  " nodos)"
)

paleta <- grDevices::hcl.colors(
  n = nrow(resultados_plot),
  palette = "Set 2"
)

limite_x <- max(
  resultados_plot$comunidades_finales
) * 1.18

png(
  filename = archivo_png,
  width = 2200,
  height = 1450,
  res = 220
)

par(
  mar = c(6, 12, 5, 3)
)

posiciones <- barplot(
  height = resultados_plot$comunidades_finales,
  names.arg = etiquetas_red,
  horiz = TRUE,
  col = paleta,
  border = "gray30",
  xlim = c(0, limite_x),
  xlab = "Cantidad de comunidades finales",
  main = paste0(
    "Comunidades finales detectadas por red\n",
    "VersiÃ³n local modificada de cluster_infomap()"
  ),
  las = 1,
  cex.names = 0.92,
  cex.axis = 1.0,
  cex.lab = 1.1,
  cex.main = 1.2
)

# Valores colocados despuÃ©s de cada barra, con espacio suficiente.
text(
  x = resultados_plot$comunidades_finales + 3,
  y = posiciones,
  labels = resultados_plot$comunidades_finales,
  adj = c(0, 0.5),
  cex = 1.05,
  font = 2
)

dev.off()

capture.output(
  {
    cat("COMPARACIÃ“N DE COMUNIDADES FINALES POR RED\n")
    cat("==========================================\n\n")

    print(
      resultados,
      row.names = FALSE
    )

    cat(
      "\nLa comparaciÃ³n corresponde a la versiÃ³n local modificada ",
      "de cluster_infomap().\n",
      sep = ""
    )
  },
  file = archivo_txt
)

cat(
  "Figura corregida generada correctamente:\n",
  archivo_png,
  "\n",
  sep = ""
)
