# Figura 30: distribuciÃ³n conocida de las macrocomunidades LFR
# Tesis de Daniela Salinas Castro
#
# El script busca automÃ¡ticamente community_second_level.dat,
# identifica la versiÃ³n final con 5000 nodos y 6 macrocomunidades,
# y genera:
#   1) un grÃ¡fico de barras en PNG;
#   2) una tabla CSV;
#   3) un resumen de texto.

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
# 1. BUSCAR EL ARCHIVO DE MACROCOMUNIDADES
# ============================================================

archivos_candidatos <- list.files(
  path = lfr_dir,
  pattern = "^community_second_level\\.dat$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)

if (length(archivos_candidatos) == 0) {
  stop(
    paste0(
      "No se encontrÃ³ community_second_level.dat dentro de:\n",
      lfr_dir
    )
  )
}

leer_macrocomunidades <- function(archivo) {
  tabla <- tryCatch(
    read.table(
      archivo,
      header = FALSE,
      comment.char = "#",
      stringsAsFactors = FALSE,
      fill = TRUE
    ),
    error = function(e) NULL
  )

  if (is.null(tabla) || ncol(tabla) < 2) {
    return(NULL)
  }

  tabla <- tabla[, 1:2, drop = FALSE]
  names(tabla) <- c("node_id", "macrocomunidad")

  tabla$node_id <- suppressWarnings(
    as.integer(tabla$node_id)
  )

  tabla$macrocomunidad <- suppressWarnings(
    as.integer(tabla$macrocomunidad)
  )

  tabla <- tabla[
    complete.cases(tabla),
    ,
    drop = FALSE
  ]

  if (nrow(tabla) == 0) {
    return(NULL)
  }

  tabla
}

resultados <- lapply(
  archivos_candidatos,
  function(archivo) {
    tabla <- leer_macrocomunidades(archivo)

    if (is.null(tabla)) {
      return(NULL)
    }

    list(
      archivo = archivo,
      tabla = tabla,
      nodos = nrow(tabla),
      comunidades = length(unique(tabla$macrocomunidad))
    )
  }
)

resultados <- Filter(
  Negate(is.null),
  resultados
)

if (length(resultados) == 0) {
  stop(
    "No fue posible interpretar ningÃºn archivo de macrocomunidades."
  )
}

coincide_final <- vapply(
  resultados,
  function(x) {
    x$nodos == 5000 &&
      x$comunidades == 6
  },
  logical(1)
)

if (!any(coincide_final)) {
  detalle <- vapply(
    resultados,
    function(x) {
      paste0(
        basename(x$archivo),
        " -> nodos=",
        x$nodos,
        ", macrocomunidades=",
        x$comunidades
      )
    },
    character(1)
  )

  stop(
    paste0(
      "Se encontraron archivos, pero ninguno coincide con la red final.\n",
      "Se esperaba: 5000 nodos y 6 macrocomunidades.\n\n",
      "Archivos revisados:\n",
      paste(detalle, collapse = "\n")
    )
  )
}

seleccionados <- resultados[
  coincide_final
]

if (length(seleccionados) > 1) {
  fechas <- file.info(
    vapply(
      seleccionados,
      function(x) x$archivo,
      character(1)
    )
  )$mtime

  seleccionado <- seleccionados[[which.max(fechas)]]
} else {
  seleccionado <- seleccionados[[1]]
}

archivo_macro <- seleccionado$archivo
macro_lfr <- seleccionado$tabla

# ============================================================
# 2. CALCULAR LA DISTRIBUCIÃ“N
# ============================================================

distribucion <- as.data.frame(
  table(macro_lfr$macrocomunidad),
  stringsAsFactors = FALSE
)

names(distribucion) <- c(
  "macrocomunidad",
  "cantidad_nodos"
)

distribucion$macrocomunidad <- as.integer(
  as.character(distribucion$macrocomunidad)
)

distribucion$cantidad_nodos <- as.integer(
  distribucion$cantidad_nodos
)

distribucion <- distribucion[
  order(distribucion$macrocomunidad),
  ,
  drop = FALSE
]

if (sum(distribucion$cantidad_nodos) != 5000) {
  stop(
    "La suma de nodos por macrocomunidad no es igual a 5000."
  )
}

# ============================================================
# 3. CREAR ARCHIVOS DE SALIDA
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
  "figura_30_macrocomunidades_conocidas_lfr.png"
)

archivo_csv <- file.path(
  salida_dir,
  "figura_30_distribucion_macrocomunidades_lfr.csv"
)

archivo_txt <- file.path(
  salida_dir,
  "figura_30_resumen_macrocomunidades_lfr.txt"
)

write.csv(
  distribucion,
  archivo_csv,
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# ============================================================
# 4. GENERAR EL GRÃFICO
# ============================================================

paleta <- grDevices::hcl.colors(
  n = nrow(distribucion),
  palette = "Set 2"
)

png(
  filename = archivo_png,
  width = 2200,
  height = 1400,
  res = 220
)

par(
  mar = c(6, 6, 5, 2)
)

posiciones <- barplot(
  height = distribucion$cantidad_nodos,
  names.arg = paste(
    "Macro",
    distribucion$macrocomunidad
  ),
  col = paleta,
  border = "gray30",
  ylim = c(
    0,
    max(distribucion$cantidad_nodos) * 1.16
  ),
  ylab = "Cantidad de nodos",
  xlab = "Macrocomunidades conocidas",
  main = paste0(
    "DistribuciÃ³n conocida de las macrocomunidades LFR\n",
    "5000 nodos | 6 macrocomunidades"
  ),
  las = 1,
  cex.names = 1.0,
  cex.axis = 1.0,
  cex.lab = 1.1,
  cex.main = 1.2
)

text(
  x = posiciones,
  y = distribucion$cantidad_nodos,
  labels = distribucion$cantidad_nodos,
  pos = 3,
  cex = 1.0
)

mtext(
  "Fuente de referencia: community_second_level.dat",
  side = 1,
  line = 4.5,
  cex = 0.85,
  font = 3
)

dev.off()

# ============================================================
# 5. GUARDAR RESUMEN
# ============================================================

capture.output(
  {
    cat("DISTRIBUCIÃ“N CONOCIDA DE MACROCOMUNIDADES LFR\n")
    cat("============================================\n\n")

    cat("Archivo utilizado:\n")
    cat(archivo_macro, "\n\n")

    cat("Cantidad total de nodos:", nrow(macro_lfr), "\n")
    cat(
      "Cantidad de macrocomunidades:",
      length(unique(macro_lfr$macrocomunidad)),
      "\n\n"
    )

    print(
      distribucion,
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
  "Archivo de macrocomunidades utilizado:\n",
  archivo_macro,
  "\n",
  sep = ""
)
