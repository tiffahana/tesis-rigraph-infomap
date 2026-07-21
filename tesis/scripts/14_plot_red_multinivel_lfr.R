# Plot multinivel de la red jerÃ¡rquica LFR
# Tesis de Daniela Salinas Castro
#
# Genera una figura de dos paneles con la misma disposiciÃ³n:
#   izquierda -> mÃ³dulos superiores detectados en level_2
#   derecha   -> comunidades finales detectadas en final_module
#
# Resultados finales esperados:
#   5000 nodos
#   48631 aristas Ãºnicas
#   6 mÃ³dulos superiores
#   147 comunidades finales

library(igraph)

set.seed(2028)

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

resultado_rds <- file.path(
  lfr_dir,
  "salida_tesis_lfr_jerarquia",
  "resultado_lfr_igraph_modificado.rds"
)

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
  "figura_24_red_multinivel_lfr.png"
)

archivo_resumen <- file.path(
  salida_dir,
  "figura_24_resumen_lfr.txt"
)

archivo_layout <- file.path(
  salida_dir,
  "layout_lfr_5000_nodos.rds"
)

# ============================================================
# 1. CARGAR EL RESULTADO MULTINIVEL
# ============================================================

if (!file.exists(resultado_rds)) {
  stop(
    paste0(
      "No se encontrÃ³ el resultado final de LFR:\n",
      resultado_rds
    )
  )
}

resultado_lfr <- readRDS(
  resultado_rds
)

multinivel <- resultado_lfr$multilevel_modules

if (is.null(multinivel) || NROW(multinivel) == 0) {
  stop(
    paste0(
      "El RDS no contiene filas en multilevel_modules.\n",
      "Campos disponibles: ",
      paste(names(resultado_lfr), collapse = ", ")
    )
  )
}

multinivel <- as.data.frame(
  multinivel,
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
      "Faltan columnas necesarias en multilevel_modules.\n",
      "Columnas encontradas: ",
      paste(names(multinivel), collapse = ", ")
    )
  )
}

multinivel$node_id <- as.character(
  multinivel$node_id
)

cantidad_nodos_resultado <- nrow(
  multinivel
)

cantidad_superiores <- length(
  unique(multinivel$level_2)
)

cantidad_finales <- length(
  unique(multinivel$final_module)
)

# ============================================================
# 2. BUSCAR AUTOMÃTICAMENTE EL ARCHIVO DE ARISTAS LFR
# ============================================================

leer_archivo_aristas <- function(archivo) {
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
    tabla <- tryCatch(
      read.csv(
        archivo,
        header = FALSE,
        comment.char = "#",
        stringsAsFactors = FALSE
      ),
      error = function(e) NULL
    )
  }

  if (is.null(tabla) || ncol(tabla) < 2) {
    return(NULL)
  }

  aristas <- tabla[, 1:2, drop = FALSE]

  aristas[[1]] <- suppressWarnings(
    as.integer(aristas[[1]])
  )

  aristas[[2]] <- suppressWarnings(
    as.integer(aristas[[2]])
  )

  aristas <- aristas[
    complete.cases(aristas),
    ,
    drop = FALSE
  ]

  aristas <- aristas[
    aristas[[1]] != aristas[[2]],
    ,
    drop = FALSE
  ]

  if (nrow(aristas) == 0) {
    return(NULL)
  }

  # Ajustar Ãºnicamente si los identificadores comienzan en cero.
  if (min(as.matrix(aristas)) == 0) {
    aristas[[1]] <- aristas[[1]] + 1L
    aristas[[2]] <- aristas[[2]] + 1L
  }

  grafo <- graph_from_edgelist(
    as.matrix(aristas),
    directed = FALSE
  )

  grafo <- simplify(
    grafo,
    remove.multiple = TRUE,
    remove.loops = TRUE
  )

  grafo
}

archivos_candidatos <- list.files(
  path = lfr_dir,
  pattern = "\\.(dat|txt|csv|edges|edgelist)$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)

# Priorizar nombres tÃ­picos del benchmark.
prioridad <- grepl(
  "network|edge|arista|lfr",
  basename(archivos_candidatos),
  ignore.case = TRUE
)

archivos_candidatos <- c(
  archivos_candidatos[prioridad],
  archivos_candidatos[!prioridad]
)

archivos_candidatos <- unique(
  archivos_candidatos
)

if (length(archivos_candidatos) == 0) {
  stop(
    paste0(
      "No se encontraron archivos de aristas dentro de:\n",
      lfr_dir
    )
  )
}

informacion_candidatos <- list()

for (archivo in archivos_candidatos) {
  grafo_candidato <- leer_archivo_aristas(
    archivo
  )

  if (is.null(grafo_candidato)) {
    next
  }

  informacion_candidatos[[length(informacion_candidatos) + 1L]] <- list(
    archivo = archivo,
    grafo = grafo_candidato,
    nodos = vcount(grafo_candidato),
    aristas = ecount(grafo_candidato)
  )
}

if (length(informacion_candidatos) == 0) {
  stop(
    "No fue posible interpretar ningÃºn archivo como lista de aristas."
  )
}

coincide_final <- vapply(
  informacion_candidatos,
  function(x) {
    x$nodos == 5000 &&
      x$aristas == 48631
  },
  logical(1)
)

if (!any(coincide_final)) {
  detalle <- vapply(
    informacion_candidatos,
    function(x) {
      paste0(
        basename(x$archivo),
        " -> nodos=",
        x$nodos,
        ", aristas=",
        x$aristas
      )
    },
    character(1)
  )

  stop(
    paste0(
      "No se encontrÃ³ la red final de 5000 nodos y 48631 aristas.\n\n",
      "Archivos interpretados:\n",
      paste(detalle, collapse = "\n")
    )
  )
}

seleccionados <- informacion_candidatos[
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

grafo_lfr <- seleccionado$grafo
archivo_aristas <- seleccionado$archivo

# ============================================================
# 3. ALINEAR LA TABLA MULTINIVEL CON LOS VÃ‰RTICES
# ============================================================

if (is.null(V(grafo_lfr)$name)) {
  V(grafo_lfr)$name <- as.character(
    seq_len(vcount(grafo_lfr))
  )
}

indice_multinivel <- match(
  V(grafo_lfr)$name,
  multinivel$node_id
)

if (anyNA(indice_multinivel)) {
  stop(
    paste0(
      "No fue posible asociar todos los vÃ©rtices con node_id.\n",
      "Cantidad sin correspondencia: ",
      sum(is.na(indice_multinivel))
    )
  )
}

modulos_superiores <- multinivel$level_2[
  indice_multinivel
]

comunidades_finales <- multinivel$final_module[
  indice_multinivel
]

# ============================================================
# 4. COLORES Y DISPOSICIÃ“N
# ============================================================

crear_colores <- function(
  particion,
  paleta
) {
  indices <- as.integer(
    factor(particion)
  )

  colores_base <- grDevices::hcl.colors(
    n = max(indices),
    palette = paleta
  )

  colores_base[indices]
}

colores_superiores <- crear_colores(
  modulos_superiores,
  "Set 2"
)

colores_finales <- crear_colores(
  comunidades_finales,
  "Dynamic"
)

# Calcular la disposiciÃ³n solo una vez.
if (file.exists(archivo_layout)) {
  disposicion <- readRDS(
    archivo_layout
  )

  if (
    !is.matrix(disposicion) ||
    nrow(disposicion) != vcount(grafo_lfr)
  ) {
    message(
      "El layout guardado no corresponde a esta red; se calcularÃ¡ nuevamente."
    )

    disposicion <- NULL
  }
} else {
  disposicion <- NULL
}

if (is.null(disposicion)) {
  message(
    "Calculando la disposiciÃ³n de 5000 nodos. Esto puede tardar algunos minutos..."
  )

  set.seed(2028)

  disposicion <- layout_with_lgl(
    grafo_lfr,
    maxiter = 150
  )

  saveRDS(
    disposicion,
    archivo_layout
  )
}

# ============================================================
# 5. GENERAR LA FIGURA
# ============================================================

png(
  filename = archivo_png,
  width = 2800,
  height = 1550,
  res = 200
)

par(
  mfrow = c(1, 2),
  mar = c(2, 2, 5, 2),
  oma = c(2, 1, 4, 1)
)

plot(
  grafo_lfr,
  layout = disposicion,
  vertex.color = colores_superiores,
  vertex.frame.color = NA,
  vertex.label = NA,
  vertex.size = 1.7,
  edge.color = adjustcolor(
    "gray45",
    alpha.f = 0.035
  ),
  edge.width = 0.18,
  main = paste0(
    "MÃ³dulos superiores (level_2)\n",
    cantidad_superiores,
    " mÃ³dulos"
  )
)

plot(
  grafo_lfr,
  layout = disposicion,
  vertex.color = colores_finales,
  vertex.frame.color = NA,
  vertex.label = NA,
  vertex.size = 1.7,
  edge.color = adjustcolor(
    "gray45",
    alpha.f = 0.035
  ),
  edge.width = 0.18,
  main = paste0(
    "Comunidades finales\n",
    cantidad_finales,
    " comunidades"
  )
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
    vcount(grafo_lfr),
    " nodos | ",
    ecount(grafo_lfr),
    " aristas Ãºnicas"
  ),
  outer = TRUE,
  side = 3,
  line = 0.25,
  cex = 1
)

dev.off()

# ============================================================
# 6. GUARDAR RESUMEN
# ============================================================

capture.output(
  {
    cat("RESULTADO MULTINIVEL DE LA RED LFR\n")
    cat("=================================\n\n")

    cat("Archivo de aristas utilizado:\n")
    cat(archivo_aristas, "\n\n")

    cat("Archivo RDS utilizado:\n")
    cat(resultado_rds, "\n\n")

    cat("Nodos:", vcount(grafo_lfr), "\n")
    cat("Aristas Ãºnicas:", ecount(grafo_lfr), "\n")
    cat("MÃ³dulos superiores en level_2:", cantidad_superiores, "\n")
    cat("Comunidades finales:", cantidad_finales, "\n")
    cat(
      "final_module coincide con membership():",
      all(
        multinivel$final_module ==
          membership(resultado_lfr)
      ),
      "\n"
    )
  },
  file = archivo_resumen
)

cat(
  "\nFigura generada correctamente:\n",
  archivo_png,
  "\n\n",
  "Resumen generado correctamente:\n",
  archivo_resumen,
  "\n\n",
  "Archivo de aristas utilizado:\n",
  archivo_aristas,
  "\n",
  sep = ""
)
