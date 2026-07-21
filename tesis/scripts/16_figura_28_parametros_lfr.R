# Figura 28: parÃ¡metros utilizados para generar la red jerÃ¡rquica LFR
# Tesis de Daniela Salinas Castro
#
# Genera una tabla en formato PNG con los parÃ¡metros documentados
# para la configuraciÃ³n final del benchmark jerÃ¡rquico LFR.

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
  "figura_28_parametros_lfr.png"
)

archivo_csv <- file.path(
  salida_dir,
  "figura_28_parametros_lfr.csv"
)

parametros_lfr <- data.frame(
  parametro = c(
    "NÃºmero de nodos",
    "Grado promedio",
    "Grado mÃ¡ximo",
    "TamaÃ±o mÃ­nimo de microcomunidad",
    "TamaÃ±o mÃ¡ximo de microcomunidad",
    "TamaÃ±o mÃ­nimo de macrocomunidad",
    "TamaÃ±o mÃ¡ximo de macrocomunidad",
    "Mezcla entre macrocomunidades (mu1)",
    "Mezcla entre microcomunidades (mu2)",
    "Pertenencia comunitaria superpuesta"
  ),
  valor = c(
    "5000",
    "20",
    "50",
    "20",
    "50",
    "500",
    "1000",
    "0.05",
    "0.25",
    "No"
  ),
  descripcion = c(
    "Cantidad total de nodos de la red",
    "Promedio esperado de conexiones por nodo",
    "MÃ¡ximo permitido de conexiones por nodo",
    "LÃ­mite inferior para comunidades de menor escala",
    "LÃ­mite superior para comunidades de menor escala",
    "LÃ­mite inferior para comunidades de nivel superior",
    "LÃ­mite superior para comunidades de nivel superior",
    "ProporciÃ³n de conexiones hacia otras macrocomunidades",
    "ProporciÃ³n de conexiones hacia otras microcomunidades del mismo nivel superior",
    "Cada nodo pertenece a una Ãºnica comunidad en cada nivel"
  ),
  stringsAsFactors = FALSE
)

write.csv(
  parametros_lfr,
  archivo_csv,
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

envolver_texto <- function(texto, ancho) {
  vapply(
    texto,
    function(x) {
      paste(
        strwrap(
          x,
          width = ancho
        ),
        collapse = "\n"
      )
    },
    character(1)
  )
}

parametros_lfr$parametro <- envolver_texto(
  parametros_lfr$parametro,
  35
)

parametros_lfr$descripcion <- envolver_texto(
  parametros_lfr$descripcion,
  54
)

encabezados <- c(
  "ParÃ¡metro",
  "Valor",
  "DescripciÃ³n"
)

n_filas <- nrow(parametros_lfr)

alto_encabezado <- 0.075
alto_nota <- 0.09
alto_fila <- (1 - alto_encabezado - alto_nota) / n_filas

x_limites <- c(
  0.03,
  0.36,
  0.48,
  0.97
)

png(
  filename = archivo_png,
  width = 2600,
  height = 1800,
  res = 220
)

par(
  mar = c(1, 1, 4, 1),
  xpd = NA
)

plot.new()
plot.window(
  xlim = c(0, 1),
  ylim = c(0, 1)
)

title(
  main = "ParÃ¡metros utilizados para generar la red jerÃ¡rquica LFR",
  cex.main = 1.45,
  font.main = 2,
  line = 1.4
)

y_superior <- 0.94
y_inferior_encabezado <- y_superior - alto_encabezado

rect(
  xleft = x_limites[-length(x_limites)],
  ybottom = y_inferior_encabezado,
  xright = x_limites[-1],
  ytop = y_superior,
  col = "gray88",
  border = "gray35",
  lwd = 1.4
)

for (j in seq_along(encabezados)) {
  text(
    x = (x_limites[j] + x_limites[j + 1]) / 2,
    y = (y_superior + y_inferior_encabezado) / 2,
    labels = encabezados[j],
    font = 2,
    cex = 1.0
  )
}

for (i in seq_len(n_filas)) {
  y_top <- y_inferior_encabezado - (i - 1) * alto_fila
  y_bottom <- y_top - alto_fila

  color_fila <- if (i %% 2 == 1) "white" else "gray96"

  rect(
    xleft = x_limites[-length(x_limites)],
    ybottom = y_bottom,
    xright = x_limites[-1],
    ytop = y_top,
    col = color_fila,
    border = "gray55",
    lwd = 1
  )

  text(
    x = x_limites[1] + 0.012,
    y = (y_top + y_bottom) / 2,
    labels = parametros_lfr$parametro[i],
    adj = c(0, 0.5),
    cex = 0.82
  )

  text(
    x = (x_limites[2] + x_limites[3]) / 2,
    y = (y_top + y_bottom) / 2,
    labels = parametros_lfr$valor[i],
    font = 2,
    cex = 0.88
  )

  text(
    x = x_limites[3] + 0.012,
    y = (y_top + y_bottom) / 2,
    labels = parametros_lfr$descripcion[i],
    adj = c(0, 0.5),
    cex = 0.78
  )
}

text(
  x = 0.5,
  y = 0.035,
  labels = paste0(
    "Con mu1 = 0.05 y mu2 = 0.25, aproximadamente el 70 % restante ",
    "de las conexiones permanece dentro de la microcomunidad de cada nodo."
  ),
  cex = 0.82,
  font = 3
)

dev.off()

cat(
  "Figura generada correctamente:\n",
  archivo_png,
  "\n\n",
  "Tabla CSV generada correctamente:\n",
  archivo_csv,
  "\n",
  sep = ""
)
