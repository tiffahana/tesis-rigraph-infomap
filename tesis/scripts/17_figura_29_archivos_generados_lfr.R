# Figura 29: archivos generados por el benchmark jerÃ¡rquico LFR
# Tesis de Daniela Salinas Castro
#
# El script busca automÃ¡ticamente la carpeta que contiene network.dat,
# community_first_level.dat y community_second_level.dat. Luego genera:
#   1) una tabla en formato PNG;
#   2) una tabla CSV;
#   3) un resumen de texto con la ruta utilizada.

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

lfr_dir <- file.path(
  base_dir,
  "datos", "lfr"
)

if (!dir.exists(lfr_dir)) {
  stop(
    paste0(
      "No se encontrÃ³ la carpeta del benchmark LFR:\n",
      lfr_dir
    )
  )
}

# ============================================================
# 1. LOCALIZAR LA CARPETA DE LOS ARCHIVOS ORIGINALES DE LFR
# ============================================================

archivos_network <- list.files(
  path = lfr_dir,
  pattern = "^network\\.dat$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)

if (length(archivos_network) == 0) {
  stop(
    paste0(
      "No se encontrÃ³ network.dat dentro de:\n",
      lfr_dir
    )
  )
}

carpetas_candidatas <- unique(
  dirname(archivos_network)
)

archivos_requeridos <- c(
  "network.dat",
  "community_first_level.dat",
  "community_second_level.dat"
)

puntuar_carpeta <- function(carpeta) {
  existentes <- file.exists(
    file.path(
      carpeta,
      archivos_requeridos
    )
  )

  sum(existentes)
}

puntajes <- vapply(
  carpetas_candidatas,
  puntuar_carpeta,
  numeric(1)
)

mejor_puntaje <- max(puntajes)

if (mejor_puntaje < length(archivos_requeridos)) {
  warning(
    paste0(
      "Ninguna carpeta contiene simultÃ¡neamente los tres archivos principales. ",
      "Se utilizarÃ¡ la carpeta con mayor coincidencia."
    )
  )
}

mejores_carpetas <- carpetas_candidatas[
  puntajes == mejor_puntaje
]

if (length(mejores_carpetas) > 1) {
  fechas_network <- file.info(
    file.path(
      mejores_carpetas,
      "network.dat"
    )
  )$mtime

  carpeta_lfr <- mejores_carpetas[
    which.max(fechas_network)
  ]
} else {
  carpeta_lfr <- mejores_carpetas[1]
}

# ============================================================
# 2. OBTENER LOS ARCHIVOS .DAT GENERADOS
# ============================================================

archivos_dat <- list.files(
  path = carpeta_lfr,
  pattern = "\\.dat$",
  full.names = TRUE,
  ignore.case = TRUE
)

if (length(archivos_dat) == 0) {
  stop(
    paste0(
      "La carpeta seleccionada no contiene archivos .dat:\n",
      carpeta_lfr
    )
  )
}

orden_preferido <- c(
  "network.dat",
  "community_first_level.dat",
  "community_second_level.dat",
  "statistics.dat",
  "time_seed.dat"
)

nombres_archivos <- basename(
  archivos_dat
)

orden <- c(
  match(
    orden_preferido,
    nombres_archivos,
    nomatch = 0
  ),
  which(
    !nombres_archivos %in% orden_preferido
  )
)

orden <- orden[
  orden > 0
]

archivos_dat <- archivos_dat[
  unique(orden)
]

nombres_archivos <- basename(
  archivos_dat
)

# ============================================================
# 3. CONSTRUIR EL RESUMEN DE CADA ARCHIVO
# ============================================================

descripcion_archivo <- function(nombre) {
  nombre_minuscula <- tolower(nombre)

  if (nombre_minuscula == "network.dat") {
    return("Lista de aristas que define la red artificial")
  }

  if (nombre_minuscula == "community_first_level.dat") {
    return("Pertenencia conocida de los nodos a microcomunidades")
  }

  if (nombre_minuscula == "community_second_level.dat") {
    return("Pertenencia conocida de los nodos a macrocomunidades")
  }

  if (nombre_minuscula == "statistics.dat") {
    return("EstadÃ­sticas generales producidas por el generador")
  }

  if (nombre_minuscula == "time_seed.dat") {
    return("Semilla temporal registrada durante la generaciÃ³n")
  }

  "Archivo auxiliar producido por el benchmark"
}

contar_registros <- function(archivo) {
  lineas <- readLines(
    archivo,
    warn = FALSE,
    encoding = "UTF-8"
  )

  lineas_validas <- trimws(
    lineas
  )

  lineas_validas <- lineas_validas[
    nzchar(lineas_validas) &
      !startsWith(lineas_validas, "#")
  ]

  length(lineas_validas)
}

registros <- vapply(
  archivos_dat,
  contar_registros,
  integer(1)
)

tamano_kb <- round(
  file.info(archivos_dat)$size / 1024,
  1
)

tabla_archivos <- data.frame(
  archivo = nombres_archivos,
  contenido = vapply(
    nombres_archivos,
    descripcion_archivo,
    character(1)
  ),
  registros = registros,
  tamano_kb = tamano_kb,
  stringsAsFactors = FALSE
)

names(tabla_archivos) <- c(
  "Archivo",
  "Contenido",
  "Registros",
  "TamaÃ±o (KB)"
)

# ============================================================
# 4. VALIDACIONES DE LOS ARCHIVOS PRINCIPALES
# ============================================================

buscar_registros <- function(nombre) {
  indice <- which(
    tolower(tabla_archivos$Archivo) ==
      tolower(nombre)
  )

  if (length(indice) == 0) {
    return(NA_integer_)
  }

  tabla_archivos$Registros[indice[1]]
}

registros_network <- buscar_registros(
  "network.dat"
)

registros_micro <- buscar_registros(
  "community_first_level.dat"
)

registros_macro <- buscar_registros(
  "community_second_level.dat"
)

if (!is.na(registros_network) && registros_network != 48631) {
  warning(
    paste0(
      "network.dat contiene ",
      registros_network,
      " registros; se esperaban 48631 aristas para la red final."
    )
  )
}

if (!is.na(registros_micro) && registros_micro != 5000) {
  warning(
    paste0(
      "community_first_level.dat contiene ",
      registros_micro,
      " registros; se esperaban 5000 nodos."
    )
  )
}

if (!is.na(registros_macro) && registros_macro != 5000) {
  warning(
    paste0(
      "community_second_level.dat contiene ",
      registros_macro,
      " registros; se esperaban 5000 nodos."
    )
  )
}

# ============================================================
# 5. CREAR LOS ARCHIVOS DE SALIDA
# ============================================================

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
  "figura_29_archivos_generados_lfr.png"
)

archivo_csv <- file.path(
  salida_dir,
  "figura_29_archivos_generados_lfr.csv"
)

archivo_txt <- file.path(
  salida_dir,
  "figura_29_resumen_archivos_lfr.txt"
)

write.csv(
  tabla_archivos,
  archivo_csv,
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# ============================================================
# 6. DIBUJAR LA TABLA EN PNG
# ============================================================

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

tabla_dibujo <- tabla_archivos

tabla_dibujo$Contenido <- envolver_texto(
  tabla_dibujo$Contenido,
  48
)

encabezados <- names(
  tabla_dibujo
)

n_filas <- nrow(
  tabla_dibujo
)

alto_encabezado <- 0.10
alto_nota <- 0.15
alto_fila <- (
  0.88 -
    alto_encabezado -
    alto_nota
) / n_filas

x_limites <- c(
  0.025,
  0.28,
  0.73,
  0.86,
  0.975
)

png(
  filename = archivo_png,
  width = 2700,
  height = 1500,
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
  main = "Archivos generados por el benchmark jerÃ¡rquico LFR",
  cex.main = 1.45,
  font.main = 2,
  line = 1.4
)

y_superior <- 0.91
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
    x = (
      x_limites[j] +
        x_limites[j + 1]
    ) / 2,
    y = (
      y_superior +
        y_inferior_encabezado
    ) / 2,
    labels = encabezados[j],
    font = 2,
    cex = 0.95
  )
}

for (i in seq_len(n_filas)) {
  y_top <- y_inferior_encabezado -
    (i - 1) * alto_fila

  y_bottom <- y_top - alto_fila

  color_fila <- if (
    i %% 2 == 1
  ) {
    "white"
  } else {
    "gray96"
  }

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
    labels = tabla_dibujo$Archivo[i],
    adj = c(0, 0.5),
    cex = 0.82,
    font = 2
  )

  text(
    x = x_limites[2] + 0.012,
    y = (y_top + y_bottom) / 2,
    labels = tabla_dibujo$Contenido[i],
    adj = c(0, 0.5),
    cex = 0.79
  )

  text(
    x = (
      x_limites[3] +
        x_limites[4]
    ) / 2,
    y = (y_top + y_bottom) / 2,
    labels = tabla_dibujo$Registros[i],
    cex = 0.84
  )

  text(
    x = (
      x_limites[4] +
        x_limites[5]
    ) / 2,
    y = (y_top + y_bottom) / 2,
    labels = tabla_dibujo$`TamaÃ±o (KB)`[i],
    cex = 0.84
  )
}

ruta_mostrada <- gsub(
  "\\\\",
  "/",
  carpeta_lfr
)

ruta_envuelta <- paste(
  strwrap(
    paste0(
      "Carpeta utilizada: ",
      ruta_mostrada
    ),
    width = 115
  ),
  collapse = "\n"
)

text(
  x = 0.5,
  y = 0.055,
  labels = ruta_envuelta,
  cex = 0.78,
  font = 3
)

dev.off()

capture.output(
  {
    cat("ARCHIVOS GENERADOS POR EL BENCHMARK LFR\n")
    cat("=======================================\n\n")

    cat("Carpeta seleccionada:\n")
    cat(carpeta_lfr, "\n\n")

    print(
      tabla_archivos,
      row.names = FALSE
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
  "\n\n",
  "Carpeta LFR seleccionada:\n",
  carpeta_lfr,
  "\n",
  sep = ""
)
