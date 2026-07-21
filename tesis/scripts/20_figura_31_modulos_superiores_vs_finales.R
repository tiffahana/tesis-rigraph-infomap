# Figura 31: comparaciÃ³n entre mÃ³dulos superiores y comunidades finales
# Tesis de Daniela Salinas Castro
#
# Genera un grÃ¡fico tipo "dumbbell" para comparar, en cada red,
# la cantidad de mÃ³dulos superiores detectados en level_2 con la
# cantidad de comunidades finales almacenadas en final_module.
#
# Se utiliza una escala logarÃ­tmica en el eje horizontal para que
# los valores pequeÃ±os y grandes sean visibles en una misma figura.

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
  "figura_31_modulos_superiores_vs_comunidades_finales.png"
)

archivo_csv <- file.path(
  salida_dir,
  "figura_31_modulos_superiores_vs_comunidades_finales.csv"
)

archivo_txt <- file.path(
  salida_dir,
  "figura_31_resumen_modulos_vs_comunidades.txt"
)

# Resultados finales documentados en la memoria
resultados <- data.frame(
  red = c(
    "Zachary Karate Club",
    "USairports",
    "Yeast",
    "LFR"
  ),
  modulos_superiores = c(
    3,
    5,
    15,
    6
  ),
  comunidades_finales = c(
    3,
    52,
    203,
    147
  ),
  stringsAsFactors = FALSE
)

resultados$factor_expansion <- round(
  resultados$comunidades_finales /
    resultados$modulos_superiores,
  1
)

write.csv(
  resultados,
  archivo_csv,
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# Invertir el orden para que Zachary aparezca arriba.
datos_plot <- resultados[
  nrow(resultados):1,
  ,
  drop = FALSE
]

posiciones_y <- seq_len(
  nrow(datos_plot)
)

limite_minimo <- 2
limite_maximo <- 300

png(
  filename = archivo_png,
  width = 2400,
  height = 1500,
  res = 220
)

par(
  mar = c(7, 11, 6, 4)
)

plot(
  x = datos_plot$modulos_superiores,
  y = posiciones_y,
  type = "n",
  log = "x",
  xlim = c(limite_minimo, limite_maximo),
  ylim = c(0.5, nrow(datos_plot) + 0.5),
  yaxt = "n",
  xlab = "Cantidad de mÃ³dulos o comunidades (escala logarÃ­tmica)",
  ylab = "",
  main = paste0(
    "MÃ³dulos superiores y comunidades finales por red\n",
    "VersiÃ³n local modificada de cluster_infomap()"
  ),
  cex.axis = 1.0,
  cex.lab = 1.1,
  cex.main = 1.2
)

axis(
  side = 2,
  at = posiciones_y,
  labels = datos_plot$red,
  las = 1,
  tick = FALSE,
  cex.axis = 1.0
)

abline(
  v = c(3, 5, 10, 20, 50, 100, 200),
  lty = 3,
  col = "gray85"
)

# LÃ­nea que une ambos niveles para cada red.
segments(
  x0 = datos_plot$modulos_superiores,
  y0 = posiciones_y,
  x1 = datos_plot$comunidades_finales,
  y1 = posiciones_y,
  lwd = 4,
  col = "gray70"
)

# MÃ³dulos superiores.
points(
  x = datos_plot$modulos_superiores,
  y = posiciones_y,
  pch = 21,
  bg = "white",
  col = "gray20",
  cex = 2.2,
  lwd = 2
)

# Comunidades finales.
points(
  x = datos_plot$comunidades_finales,
  y = posiciones_y,
  pch = 19,
  cex = 2.2
)

# Etiquetas numÃ©ricas.
text(
  x = datos_plot$modulos_superiores,
  y = posiciones_y + 0.16,
  labels = datos_plot$modulos_superiores,
  cex = 0.95,
  font = 2
)

text(
  x = datos_plot$comunidades_finales,
  y = posiciones_y + 0.16,
  labels = datos_plot$comunidades_finales,
  cex = 0.95,
  font = 2
)

# Factor de expansiÃ³n jerÃ¡rquica.
text(
  x = sqrt(
    datos_plot$modulos_superiores *
      datos_plot$comunidades_finales
  ),
  y = posiciones_y - 0.20,
  labels = paste0(
    datos_plot$factor_expansion,
    "Ã—"
  ),
  cex = 0.90,
  font = 3
)

legend(
  "bottomright",
  legend = c(
    "MÃ³dulos superiores (level_2)",
    "Comunidades finales (final_module)",
    "Factor final/superior"
  ),
  pch = c(21, 19, NA),
  pt.bg = c("white", NA, NA),
  lty = c(NA, NA, NA),
  bty = "n",
  cex = 0.92
)

mtext(
  "El factor indica cuÃ¡ntas comunidades finales existen, en promedio, por cada mÃ³dulo superior.",
  side = 1,
  line = 5.2,
  cex = 0.85,
  font = 3
)

dev.off()

capture.output(
  {
    cat("COMPARACIÃ“N ENTRE MÃ“DULOS SUPERIORES Y COMUNIDADES FINALES\n")
    cat("===========================================================\n\n")

    print(
      resultados,
      row.names = FALSE
    )

    cat(
      "\nEl factor de expansiÃ³n corresponde a comunidades_finales / ",
      "modulos_superiores.\n",
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
