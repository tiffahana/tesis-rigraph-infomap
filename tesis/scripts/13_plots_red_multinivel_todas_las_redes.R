# Plots multinivel de las redes de prueba
# Zachary Karate Club, USairports y Yeast
# Tesis de Daniela Salinas Castro
#
# Genera, para cada red, una figura de dos paneles:
#   izquierda  -> módulos superiores del primer nivel efectivo
#   derecha    -> comunidades finales de final_module
#
# Los dos paneles usan exactamente la misma disposición de nodos.
#
# Zachary se vuelve a ejecutar con cluster_infomap() modificado.
# USairports y Yeast utilizan los CSV finales ya guardados para conservar
# los resultados documentados en la tesis.

library(igraph)

if (!requireNamespace("igraphdata", quietly = TRUE)) {
  stop(
    paste0(
      "Falta instalar el paquete igraphdata.\n",
      "Ejecuta en R: install.packages('igraphdata')"
    )
  )
}

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
salida_dir <- file.path(base_dir, "figuras")
dir.create(salida_dir, recursive = TRUE, showWarnings = FALSE)

# ============================================================
# FUNCIONES AUXILIARES
# ============================================================

cargar_red_igraphdata <- function(nombre) {
  entorno <- new.env()

  data(
    list = nombre,
    package = "igraphdata",
    envir = entorno
  )

  if (!exists(nombre, envir = entorno, inherits = FALSE)) {
    stop("No se pudo cargar la red ", nombre, " desde igraphdata.")
  }

  get(nombre, envir = entorno, inherits = FALSE)
}

preparar_red <- function(grafo) {
  if ("upgrade_graph" %in% getNamespaceExports("igraph")) {
    grafo <- igraph::upgrade_graph(grafo)
  }

  if (!"weight" %in% edge_attr_names(grafo)) {
    E(grafo)$weight <- 1
  }

  if (is_directed(grafo)) {
    grafo <- as_undirected(
      grafo,
      mode = "collapse",
      edge.attr.comb = list(weight = "sum", "ignore")
    )
  }

  grafo <- simplify(
    grafo,
    remove.multiple = TRUE,
    remove.loops = TRUE,
    edge.attr.comb = list(weight = "sum", "ignore")
  )

  componentes <- components(grafo)

  if (componentes$no > 1) {
    componente_gigante <- which.max(componentes$csize)

    grafo <- induced_subgraph(
      grafo,
      which(componentes$membership == componente_gigante)
    )
  }

  grafo
}

ordenar_multinivel <- function(multinivel) {
  multinivel <- as.data.frame(
    multinivel,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  if ("node_id" %in% names(multinivel)) {
    id_numerico <- suppressWarnings(
      as.integer(multinivel$node_id)
    )

    if (!anyNA(id_numerico)) {
      multinivel <- multinivel[
        order(id_numerico),
        ,
        drop = FALSE
      ]
    }
  }

  row.names(multinivel) <- NULL
  multinivel
}

obtener_nivel_superior <- function(multinivel) {
  columnas_nivel <- grep(
    "^level_[0-9]+$",
    names(multinivel),
    value = TRUE
  )

  if (length(columnas_nivel) == 0) {
    stop("No se encontraron columnas level_i en multilevel_modules.")
  }

  numero_nivel <- as.integer(
    sub("level_", "", columnas_nivel)
  )

  columnas_nivel <- columnas_nivel[
    order(numero_nivel)
  ]

  niveles_efectivos <- columnas_nivel[
    vapply(
      multinivel[columnas_nivel],
      function(x) length(unique(x)) > 1,
      logical(1)
    )
  ]

  if (length(niveles_efectivos) == 0) {
    stop("No se encontr\u00f3 un nivel jer\u00e1rquico con m\u00e1s de un m\u00f3dulo.")
  }

  niveles_efectivos[1]
}

resumen_candidato <- function(archivo) {
  tabla <- tryCatch(
    read.csv(
      archivo,
      stringsAsFactors = FALSE,
      check.names = FALSE
    ),
    error = function(e) NULL
  )

  if (is.null(tabla)) {
    return(NULL)
  }

  if (!"final_module" %in% names(tabla)) {
    return(NULL)
  }

  nivel <- tryCatch(
    obtener_nivel_superior(tabla),
    error = function(e) NA_character_
  )

  if (is.na(nivel)) {
    return(NULL)
  }

  list(
    archivo = archivo,
    tabla = ordenar_multinivel(tabla),
    nivel = nivel,
    nodos = nrow(tabla),
    superiores = length(unique(tabla[[nivel]])),
    finales = length(unique(tabla$final_module))
  )
}

buscar_csv_final <- function(
  patron,
  nodos_esperados,
  superiores_esperados,
  finales_esperados
) {
  candidatos <- list.files(
    path = base_dir,
    pattern = patron,
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )

  if (length(candidatos) == 0) {
    stop(
      "No se encontraron archivos que coincidan con: ",
      patron
    )
  }

  resultados <- lapply(
    candidatos,
    resumen_candidato
  )

  resultados <- Filter(
    Negate(is.null),
    resultados
  )

  coincide <- vapply(
    resultados,
    function(x) {
      x$nodos == nodos_esperados &&
        x$superiores == superiores_esperados &&
        x$finales == finales_esperados
    },
    logical(1)
  )

  if (!any(coincide)) {
    detalle <- vapply(
      resultados,
      function(x) {
        paste0(
          basename(x$archivo),
          " -> nodos=",
          x$nodos,
          ", superiores=",
          x$superiores,
          ", finales=",
          x$finales
        )
      },
      character(1)
    )

    stop(
      paste0(
        "Se encontraron CSV, pero ninguno coincide con los resultados finales.\n",
        "Se esperaba: nodos=",
        nodos_esperados,
        ", superiores=",
        superiores_esperados,
        ", finales=",
        finales_esperados,
        "\n\nCandidatos revisados:\n",
        paste(detalle, collapse = "\n")
      )
    )
  }

  seleccionados <- resultados[coincide]

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

  seleccionado
}

crear_colores <- function(particion, paleta = "Set 2") {
  indices <- as.integer(factor(particion))

  colores_base <- grDevices::hcl.colors(
    n = max(indices),
    palette = paleta
  )

  colores_base[indices]
}

crear_disposicion <- function(grafo, semilla) {
  set.seed(semilla)

  if (vcount(grafo) <= 100) {
    return(
      layout_with_fr(
        grafo,
        niter = 2000,
        weights = E(grafo)$weight
      )
    )
  }

  # LGL es más adecuado para USairports y Yeast que FR,
  # porque evita tiempos excesivos en redes grandes.
  layout_with_lgl(
    grafo,
    maxiter = if (vcount(grafo) <= 1000) 300 else 200
  )
}

crear_plot_multinivel <- function(
  grafo,
  multinivel,
  nombre_red,
  archivo_png,
  semilla = 1234
) {
  multinivel <- ordenar_multinivel(multinivel)

  if (nrow(multinivel) != vcount(grafo)) {
    stop(
      paste0(
        "La red ",
        nombre_red,
        " tiene ",
        vcount(grafo),
        " nodos, pero multilevel_modules contiene ",
        nrow(multinivel),
        " filas."
      )
    )
  }

  if (!"final_module" %in% names(multinivel)) {
    stop(
      "La tabla de ",
      nombre_red,
      " no contiene final_module."
    )
  }

  nivel_superior <- obtener_nivel_superior(multinivel)

  modulos_superiores <- multinivel[[nivel_superior]]
  comunidades_finales <- multinivel$final_module

  cantidad_superiores <- length(
    unique(modulos_superiores)
  )

  cantidad_finales <- length(
    unique(comunidades_finales)
  )

  disposicion <- crear_disposicion(
    grafo,
    semilla
  )

  colores_superiores <- crear_colores(
    modulos_superiores,
    "Set 2"
  )

  colores_finales <- crear_colores(
    comunidades_finales,
    "Dynamic"
  )

  if (vcount(grafo) <= 50) {
    tamano_vertice <- 16
    etiqueta_vertice <- V(grafo)$name

    if (is.null(etiqueta_vertice)) {
      etiqueta_vertice <- seq_len(vcount(grafo))
    }

    tamano_etiqueta <- 0.75
    ancho_arista <- 1.2
    color_arista <- "gray70"
  } else if (vcount(grafo) <= 1000) {
    tamano_vertice <- 4.2
    etiqueta_vertice <- NA
    tamano_etiqueta <- 0
    ancho_arista <- 1.20
    color_arista <- adjustcolor(
      "gray35",
      alpha.f = 0.55
    )
  } else {
    tamano_vertice <- 2.5
    etiqueta_vertice <- NA
    tamano_etiqueta <- 0
    # Mismo grosor y visibilidad que USairports.
    ancho_arista <- 1.20
    color_arista <- adjustcolor(
      "gray35",
      alpha.f = 0.55
    )
  }

  png(
    filename = archivo_png,
    width = 2600,
    height = 1450,
    res = 200
  )

  par(
    mfrow = c(1, 2),
    mar = c(2, 2, 5, 2),
    oma = c(2, 1, 4, 1)
  )

  plot(
    grafo,
    layout = disposicion,
    vertex.color = colores_superiores,
    vertex.frame.color = if (vcount(grafo) <= 50) "gray25" else NA,
    vertex.label = etiqueta_vertice,
    vertex.label.color = "black",
    vertex.label.cex = tamano_etiqueta,
    vertex.size = tamano_vertice,
    edge.color = color_arista,
    edge.width = ancho_arista,
    main = paste0(
      "M\u00f3dulos superiores (",
      nivel_superior,
      ")\n",
      cantidad_superiores,
      " m\u00f3dulos"
    )
  )

  plot(
    grafo,
    layout = disposicion,
    vertex.color = colores_finales,
    vertex.frame.color = if (vcount(grafo) <= 50) "gray25" else NA,
    vertex.label = etiqueta_vertice,
    vertex.label.color = "black",
    vertex.label.cex = tamano_etiqueta,
    vertex.size = tamano_vertice,
    edge.color = color_arista,
    edge.width = ancho_arista,
    main = paste0(
      "Comunidades finales\n",
      cantidad_finales,
      " comunidades"
    )
  )

  mtext(
    paste0(
      "Resultado multinivel de ",
      nombre_red
    ),
    outer = TRUE,
    side = 3,
    line = 1.8,
    cex = 1.45,
    font = 2
  )

  mtext(
    paste0(
      vcount(grafo),
      " nodos | ",
      ecount(grafo),
      " aristas"
    ),
    outer = TRUE,
    side = 3,
    line = 0.25,
    cex = 1
  )

  dev.off()

  cat(
    "\nFigura generada:\n",
    archivo_png,
    "\n",
    "Nivel superior: ",
    nivel_superior,
    "\n",
    "M\u00f3dulos superiores: ",
    cantidad_superiores,
    "\n",
    "Comunidades finales: ",
    cantidad_finales,
    "\n",
    sep = ""
  )
}

# ============================================================
# 1. ZACHARY KARATE CLUB
# ============================================================

cat("\nProcesando Zachary Karate Club...\n")

grafo_zachary <- make_graph("Zachary")
V(grafo_zachary)$name <- as.character(
  seq_len(vcount(grafo_zachary))
)

set.seed(1234)

resultado_zachary <- cluster_infomap(
  grafo_zachary,
  nb.trials = 100
)

if (!"multilevel_modules" %in% names(resultado_zachary)) {
  stop(
    paste0(
      "La versi\u00f3n de igraph cargada no contiene multilevel_modules.\n",
      "Ruta cargada: ",
      find.package("igraph"),
      "\nVersi\u00f3n: ",
      as.character(packageVersion("igraph"))
    )
  )
}

# En la versión local modificada, el campo se recupera correctamente
# mediante el operador $, tal como se utilizó en las pruebas anteriores.
multinivel_zachary <- resultado_zachary$multilevel_modules

if (is.null(multinivel_zachary) || NROW(multinivel_zachary) == 0) {
  stop(
    paste0(
      "cluster_infomap() no devolvi\u00f3 filas en multilevel_modules para Zachary.\n",
      "Ruta de igraph cargada: ",
      find.package("igraph"),
      "\nVersi\u00f3n: ",
      as.character(packageVersion("igraph")),
      "\nCampos disponibles: ",
      paste(names(resultado_zachary), collapse = ", ")
    )
  )
}

multinivel_zachary <- as.data.frame(
  multinivel_zachary,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

crear_plot_multinivel(
  grafo = grafo_zachary,
  multinivel = multinivel_zachary,
  nombre_red = "Zachary Karate Club",
  archivo_png = file.path(
    salida_dir,
    "figura_21_red_multinivel_zachary.png"
  ),
  semilla = 1234
)

# ============================================================
# 2. USAIRPORTS
# Resultados finales esperados: 745 nodos, 5 módulos, 52 finales
# ============================================================

cat("\nProcesando USairports...\n")

grafo_usairports <- cargar_red_igraphdata(
  "USairports"
)

grafo_usairports <- preparar_red(
  grafo_usairports
)

csv_usairports <- buscar_csv_final(
  patron = "usairports.*multilevel.*modificado.*\\.csv$",
  nodos_esperados = 745,
  superiores_esperados = 5,
  finales_esperados = 52
)

cat(
  "CSV seleccionado para USairports:\n",
  csv_usairports$archivo,
  "\n"
)

crear_plot_multinivel(
  grafo = grafo_usairports,
  multinivel = csv_usairports$tabla,
  nombre_red = "la red USairports",
  archivo_png = file.path(
    salida_dir,
    "figura_22_red_multinivel_usairports.png"
  ),
  semilla = 2026
)

# ============================================================
# 3. YEAST
# Resultados finales esperados: 2375 nodos, 15 módulos, 203 finales
# ============================================================

cat("\nProcesando Yeast...\n")

grafo_yeast <- cargar_red_igraphdata(
  "yeast"
)

grafo_yeast <- preparar_red(
  grafo_yeast
)

csv_yeast <- buscar_csv_final(
  patron = "yeast.*multilevel.*modificado.*\\.csv$",
  nodos_esperados = 2375,
  superiores_esperados = 15,
  finales_esperados = 203
)

cat(
  "CSV seleccionado para Yeast:\n",
  csv_yeast$archivo,
  "\n"
)

crear_plot_multinivel(
  grafo = grafo_yeast,
  multinivel = csv_yeast$tabla,
  nombre_red = "la red Yeast",
  archivo_png = file.path(
    salida_dir,
    "figura_23_red_multinivel_yeast.png"
  ),
  semilla = 2027
)

cat(
  "\n============================================\n",
  "SE GENERARON LOS TRES PLOTS MULTINIVEL\n",
  "Carpeta de salida:\n",
  salida_dir,
  "\n============================================\n",
  sep = ""
)