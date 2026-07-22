# Analisis de estocasticidad para la red Yeast
#
# Objetivo:
#   - Repetir la comparacion entre igraph modificado e Infomap oficial
#     sin regularizacion usando varias semillas.
#   - Comparar niveles equivalentes: top_module y final_module.
#   - Cuantificar NMI, ARI, numero de comunidades y coincidencia exacta.
#   - Verificar reproducibilidad repitiendo la semilla 123.
#
# Ejecutar desde la raiz del repositorio:
# Rscript tesis/scripts/comparaciones/26_estocasticidad_yeast_semillas.R

cat("============================================================\n")
cat("ESTOCASTICIDAD DE INFOMAP EN YEAST\n")
cat("============================================================\n\n")

suppressPackageStartupMessages(library(igraph))

if (!requireNamespace("igraphdata", quietly = TRUE)) {
  stop("Falta instalar el paquete igraphdata.")
}

if (!requireNamespace("infomap", quietly = TRUE)) {
  stop(
    "Falta instalar el paquete infomap oficial. Instale con:\n",
    "install.packages('infomap', repos = c(",
    "'https://mapequation.r-universe.dev', ",
    "'https://cloud.r-project.org'))"
  )
}

repo_dir <- normalizePath(".", winslash = "/", mustWork = TRUE)

if (!file.exists(file.path(repo_dir, "DESCRIPTION")) ||
    !dir.exists(file.path(repo_dir, "tesis"))) {
  stop("Ejecute este script desde la raiz del repositorio.")
}

out_dir <- file.path(
  repo_dir,
  "tesis",
  "resultados",
  "comparaciones",
  "revision_estocasticidad_yeast"
)

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Configuracion por defecto para el analisis final.
# Puede hacerse una prueba rapida mediante variables de entorno:
#   SEMILLAS_YEAST=1
#   NB_TRIALS_YEAST=5
#   REPETIR_123=FALSE
semillas_texto <- Sys.getenv(
  "SEMILLAS_YEAST",
  unset = "1,2,3,4,5,6,7,8,9,123"
)

semillas <- as.integer(
  trimws(strsplit(semillas_texto, ",", fixed = TRUE)[[1]])
)

if (anyNA(semillas) || length(semillas) == 0L) {
  stop("SEMILLAS_YEAST debe contener enteros separados por comas.")
}

ensayos <- as.integer(
  Sys.getenv("NB_TRIALS_YEAST", unset = "500")
)

if (is.na(ensayos) || ensayos < 1L) {
  stop("NB_TRIALS_YEAST debe ser un entero positivo.")
}

repetir_123 <- toupper(
  Sys.getenv("REPETIR_123", unset = "TRUE")
) %in% c("TRUE", "T", "1", "SI", "SÍ")

normalizar_membresia <- function(x) {
  as.integer(factor(x, levels = unique(x)))
}

calcular_metricas <- function(a, b) {
  ok <- complete.cases(a, b)
  a <- normalizar_membresia(as.integer(a[ok]))
  b <- normalizar_membresia(as.integer(b[ok]))

  data.frame(
    nodos = length(a),
    comunidades_a = length(unique(a)),
    comunidades_b = length(unique(b)),
    misma_particion = isTRUE(all(a == b)),
    nmi = as.numeric(igraph::compare(a, b, method = "nmi")),
    ari = as.numeric(igraph::compare(a, b, method = "adjusted.rand"))
  )
}

cargar_yeast <- function() {
  env <- new.env()
  data("yeast", package = "igraphdata", envir = env)
  g <- get("yeast", envir = env)

  if (exists(
    "upgrade_graph",
    where = asNamespace("igraph"),
    mode = "function"
  )) {
    g <- igraph::upgrade_graph(g)
  }

  if (!"weight" %in% igraph::edge_attr_names(g)) {
    g <- igraph::set_edge_attr(
      g,
      name = "weight",
      value = rep(1, igraph::ecount(g))
    )
  }

  if (igraph::is_directed(g)) {
    g <- igraph::as_undirected(
      g,
      mode = "collapse",
      edge.attr.comb = list(weight = "sum", "ignore")
    )
  }

  g <- igraph::simplify(
    g,
    remove.multiple = TRUE,
    remove.loops = TRUE,
    edge.attr.comb = list(weight = "sum", "ignore")
  )

  comp <- igraph::components(g)

  if (comp$no > 1L) {
    gigante <- which.max(comp$csize)
    g <- igraph::induced_subgraph(
      g,
      which(comp$membership == gigante)
    )
  }

  g
}

extraer_modificado <- function(resultado) {
  if (!"multilevel_modules" %in% names(resultado)) {
    stop("La version de igraph no contiene multilevel_modules.")
  }

  tabla <- as.data.frame(resultado$multilevel_modules)

  node_id <- if ("node_id" %in% names(tabla)) {
    as.integer(tabla$node_id)
  } else {
    seq_len(nrow(tabla))
  }

  columnas_nivel <- grep(
    "^level_[0-9]+$",
    names(tabla),
    value = TRUE
  )

  if (length(columnas_nivel) > 0L) {
    numero_nivel <- as.integer(
      sub("^level_", "", columnas_nivel)
    )

    columnas_nivel <- columnas_nivel[
      order(numero_nivel)
    ]
  }

  # La version modificada puede almacenar solamente level_1, level_2, ...
  # en vez de los alias top_module y final_module.
  if ("top_module" %in% names(tabla)) {
    top_modificado <- as.integer(tabla$top_module)
    columna_top <- "top_module"
  } else {
    nivel_top <- suppressWarnings(
      as.integer(resultado$top_module_level)
    )

    candidata <- paste0("level_", nivel_top)

    if (length(nivel_top) == 1L &&
        !is.na(nivel_top) &&
        candidata %in% names(tabla)) {
      columna_top <- candidata
    } else {
      # level_1 suele representar la raiz unica. Se elige el primer
      # nivel no trivial como nivel superior.
      no_triviales <- columnas_nivel[
        vapply(
          tabla[columnas_nivel],
          function(x) {
            length(unique(x[!is.na(x)])) > 1L
          },
          logical(1)
        )
      ]

      if (length(no_triviales) == 0L) {
        stop(
          "No fue posible identificar el nivel superior en ",
          "multilevel_modules. Columnas disponibles: ",
          paste(names(tabla), collapse = ", ")
        )
      }

      columna_top <- no_triviales[[1]]
    }

    top_modificado <- as.integer(tabla[[columna_top]])
  }

  # membership() es la particion final compatible con cluster_infomap().
  # Se utiliza como fuente principal para evitar depender de un alias
  # final_module que no existe en todas las versiones del prototipo.
  final_modificado <- as.integer(
    igraph::membership(resultado)
  )

  if (length(final_modificado) != nrow(tabla)) {
    stop(
      "La membresia final y multilevel_modules tienen longitudes distintas."
    )
  }

  cat(
    "  Columnas multinivel disponibles: ",
    paste(names(tabla), collapse = ", "),
    "\n",
    sep = ""
  )
  cat(
    "  Nivel superior modificado identificado con: ",
    columna_top,
    "\n",
    sep = ""
  )
  cat(
    "  Particion final modificada obtenida con membership().\n"
  )

  data.frame(
    node_id = node_id,
    top_modificado = top_modificado,
    final_modificado = final_modificado
  )
}

extraer_oficial <- function(resultado) {
  top <- as.data.frame(
    resultado$model,
    states = FALSE,
    depth_level = 1L,
    tibble = FALSE
  )

  final <- as.data.frame(
    resultado$model,
    states = FALSE,
    depth_level = -1L,
    tibble = FALSE
  )

  top <- top[, c("node_id", "module_id"), drop = FALSE]
  final <- final[, c("node_id", "module_id"), drop = FALSE]

  top <- top[!duplicated(top$node_id), , drop = FALSE]
  final <- final[!duplicated(final$node_id), , drop = FALSE]

  names(top)[2] <- "top_oficial"
  names(final)[2] <- "final_oficial"

  merge(top, final, by = "node_id", all = TRUE, sort = TRUE)
}

ejecutar_par <- function(g, semilla, repeticion = 1L) {
  cat(
    sprintf(
      "\nSemilla %d, repeticion %d\n",
      semilla,
      repeticion
    )
  )

  inicio_mod <- proc.time()[["elapsed"]]

  set.seed(semilla)

  modificado <- igraph::cluster_infomap(
    g,
    e.weights = igraph::edge_attr(g, "weight"),
    nb.trials = ensayos
  )

  tiempo_mod <- proc.time()[["elapsed"]] - inicio_mod

  inicio_oficial <- proc.time()[["elapsed"]]

  oficial <- infomap::cluster_infomap(
    g,
    weight = "weight",
    nb.trials = ensayos,
    seed = semilla,
    regularized = FALSE,
    silent = TRUE,
    tibble = FALSE
  )

  tiempo_oficial <- proc.time()[["elapsed"]] - inicio_oficial

  tabla <- merge(
    extraer_modificado(modificado),
    extraer_oficial(oficial),
    by = "node_id",
    all = TRUE,
    sort = TRUE
  )

  met_top <- calcular_metricas(
    tabla$top_modificado,
    tabla$top_oficial
  )

  met_final <- calcular_metricas(
    tabla$final_modificado,
    tabla$final_oficial
  )

  resultado <- rbind(
    data.frame(
      semilla = semilla,
      repeticion = repeticion,
      nivel = "superior",
      tiempo_igraph_seg = tiempo_mod,
      tiempo_oficial_seg = tiempo_oficial,
      met_top
    ),
    data.frame(
      semilla = semilla,
      repeticion = repeticion,
      nivel = "final",
      tiempo_igraph_seg = tiempo_mod,
      tiempo_oficial_seg = tiempo_oficial,
      met_final
    )
  )

  archivo_nodos <- file.path(
    out_dir,
    sprintf(
      "yeast_semilla_%d_repeticion_%d.csv",
      semilla,
      repeticion
    )
  )

  write.csv(tabla, archivo_nodos, row.names = FALSE)

  cat(
    sprintf(
      "  Superior: %d vs %d comunidades; NMI %.4f; ARI %.4f\n",
      met_top$comunidades_a,
      met_top$comunidades_b,
      met_top$nmi,
      met_top$ari
    )
  )

  cat(
    sprintf(
      "  Final:    %d vs %d comunidades; NMI %.4f; ARI %.4f\n",
      met_final$comunidades_a,
      met_final$comunidades_b,
      met_final$nmi,
      met_final$ari
    )
  )

  list(
    resumen = resultado,
    tabla = tabla
  )
}

g <- cargar_yeast()

cat("Version igraph:", as.character(packageVersion("igraph")), "\n")
cat("Version Infomap oficial:", as.character(packageVersion("infomap")), "\n")
cat("Nodos:", igraph::vcount(g), "\n")
cat("Aristas:", igraph::ecount(g), "\n")
cat("Ensayos por ejecucion:", ensayos, "\n")
cat("Semillas:", paste(semillas, collapse = ", "), "\n")
cat("Salida:", out_dir, "\n")

ejecuciones <- list()
particiones <- list()

for (i in seq_along(semillas)) {
  semilla <- semillas[[i]]
  ejecucion <- ejecutar_par(g, semilla, repeticion = 1L)

  ejecuciones[[length(ejecuciones) + 1L]] <- ejecucion$resumen
  particiones[[paste0(semilla, "_1")]] <- ejecucion$tabla

  # Guardar avance después de cada semilla.
  write.csv(
    do.call(rbind, ejecuciones),
    file.path(out_dir, "comparacion_semillas_parcial.csv"),
    row.names = FALSE
  )
}

# Repeticion adicional para comprobar reproducibilidad con la misma semilla.
if (repetir_123) {
  if (!123L %in% semillas) {
    ejecucion_123_inicial <- ejecutar_par(
      g,
      123L,
      repeticion = 1L
    )
    ejecuciones[[length(ejecuciones) + 1L]] <-
      ejecucion_123_inicial$resumen
    particiones[["123_1"]] <- ejecucion_123_inicial$tabla
  }

  repeticion_123 <- ejecutar_par(g, 123L, repeticion = 2L)
  ejecuciones[[length(ejecuciones) + 1L]] <-
    repeticion_123$resumen
  particiones[["123_2"]] <- repeticion_123$tabla
}

resultados <- do.call(rbind, ejecuciones)

write.csv(
  resultados,
  file.path(out_dir, "comparacion_semillas_yeast.csv"),
  row.names = FALSE
)

resumir_nivel <- function(datos) {
  data.frame(
    ejecuciones = nrow(datos),
    comunidades_igraph_min = min(datos$comunidades_a),
    comunidades_igraph_max = max(datos$comunidades_a),
    comunidades_oficial_min = min(datos$comunidades_b),
    comunidades_oficial_max = max(datos$comunidades_b),
    coincidencias_exactas = sum(datos$misma_particion),
    nmi_min = min(datos$nmi),
    nmi_media = mean(datos$nmi),
    nmi_max = max(datos$nmi),
    ari_min = min(datos$ari),
    ari_media = mean(datos$ari),
    ari_max = max(datos$ari)
  )
}

resumen_niveles <- do.call(
  rbind,
  lapply(split(resultados, resultados$nivel), resumir_nivel)
)

resumen_niveles$nivel <- rownames(resumen_niveles)
rownames(resumen_niveles) <- NULL

resumen_niveles <- resumen_niveles[
  ,
  c("nivel", setdiff(names(resumen_niveles), "nivel")),
  drop = FALSE
]

write.csv(
  resumen_niveles,
  file.path(out_dir, "resumen_estocasticidad_yeast.csv"),
  row.names = FALSE
)

# Comparar las dos ejecuciones de la semilla 123 dentro de cada implementacion.
if (repetir_123) {
  tabla_123_a <- particiones[["123_1"]]
  tabla_123_b <- particiones[["123_2"]]

  reproducibilidad <- rbind(
    data.frame(
      implementacion = "igraph_modificado",
      nivel = "superior",
      calcular_metricas(
        tabla_123_a$top_modificado,
        tabla_123_b$top_modificado
      )
    ),
    data.frame(
      implementacion = "igraph_modificado",
      nivel = "final",
      calcular_metricas(
        tabla_123_a$final_modificado,
        tabla_123_b$final_modificado
      )
    ),
    data.frame(
      implementacion = "infomap_oficial",
      nivel = "superior",
      calcular_metricas(
        tabla_123_a$top_oficial,
        tabla_123_b$top_oficial
      )
    ),
    data.frame(
      implementacion = "infomap_oficial",
      nivel = "final",
      calcular_metricas(
        tabla_123_a$final_oficial,
        tabla_123_b$final_oficial
      )
    )
  )
} else {
  reproducibilidad <- data.frame(
    implementacion = character(),
    nivel = character(),
    nodos = integer(),
    comunidades_a = integer(),
    comunidades_b = integer(),
    misma_particion = logical(),
    nmi = numeric(),
    ari = numeric()
  )
}

write.csv(
  reproducibilidad,
  file.path(out_dir, "reproducibilidad_semilla_123.csv"),
  row.names = FALSE
)

capture.output(
  {
    cat("============================================================\n")
    cat("ESTOCASTICIDAD DE INFOMAP EN YEAST\n")
    cat("============================================================\n\n")
    cat("Version igraph:", as.character(packageVersion("igraph")), "\n")
    cat("Version Infomap oficial:", as.character(packageVersion("infomap")), "\n")
    cat("Nodos:", igraph::vcount(g), "\n")
    cat("Aristas:", igraph::ecount(g), "\n")
    cat("Ensayos por ejecucion:", ensayos, "\n")
    cat("Semillas:", paste(semillas, collapse = ", "), "\n\n")

    cat("RESULTADOS POR SEMILLA\n\n")
    print(resultados)

    cat("\nRESUMEN POR NIVEL\n\n")
    print(resumen_niveles)

    cat("\nREPRODUCIBILIDAD DE LA SEMILLA 123\n\n")
    print(reproducibilidad)
  },
  file = file.path(out_dir, "resumen_estocasticidad_yeast.txt")
)

cat("\n============================================================\n")
cat("ANALISIS TERMINADO\n")
cat("============================================================\n")
cat(
  file.path(out_dir, "comparacion_semillas_yeast.csv"),
  "\n"
)
cat(
  file.path(out_dir, "resumen_estocasticidad_yeast.csv"),
  "\n"
)
cat(
  file.path(out_dir, "reproducibilidad_semilla_123.csv"),
  "\n"
)
cat(
  file.path(out_dir, "resumen_estocasticidad_yeast.txt"),
  "\n"
)
