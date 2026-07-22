# Prueba controlada: Infomap jerarquico frente a two-level en Yeast
#
# Objetivo:
#   Evaluar si la discrepancia entre igraph modificado e Infomap oficial
#   disminuye cuando la implementacion oficial se restringe a dos niveles.
#
# Ejecutar desde la raiz del repositorio:
# Rscript tesis/scripts/comparaciones/27_yeast_hierarquico_vs_two_level.R

cat("============================================================\n")
cat("YEAST: INFOMAP JERARQUICO VS TWO-LEVEL\n")
cat("============================================================\n\n")

suppressPackageStartupMessages(library(igraph))

if (!requireNamespace("igraphdata", quietly = TRUE)) {
  stop("Falta instalar el paquete igraphdata.")
}

if (!requireNamespace("infomap", quietly = TRUE)) {
  stop("Falta instalar el paquete infomap oficial.")
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
  "revision_two_level_yeast"
)

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

semilla <- 123L
ensayos <- 500L

normalizar <- function(x) {
  as.integer(factor(x, levels = unique(x)))
}

metricas <- function(a, b) {
  ok <- complete.cases(a, b)
  a <- normalizar(as.integer(a[ok]))
  b <- normalizar(as.integer(b[ok]))

  data.frame(
    nodos = length(a),
    comunidades_a = length(unique(a)),
    comunidades_b = length(unique(b)),
    misma_particion = isTRUE(all(a == b)),
    nmi = as.numeric(igraph::compare(a, b, method = "nmi")),
    ari = as.numeric(
      igraph::compare(a, b, method = "adjusted.rand")
    )
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

  numeros <- as.integer(sub("^level_", "", columnas_nivel))
  columnas_nivel <- columnas_nivel[order(numeros)]

  nivel_top <- suppressWarnings(
    as.integer(resultado$top_module_level)
  )

  candidata <- paste0("level_", nivel_top)

  if (length(nivel_top) == 1L &&
      !is.na(nivel_top) &&
      candidata %in% names(tabla)) {
    columna_top <- candidata
  } else {
    no_triviales <- columnas_nivel[
      vapply(
        tabla[columnas_nivel],
        function(x) length(unique(x[!is.na(x)])) > 1L,
        logical(1)
      )
    ]

    if (length(no_triviales) == 0L) {
      stop("No se pudo identificar el nivel superior modificado.")
    }

    columna_top <- no_triviales[[1]]
  }

  data.frame(
    node_id = node_id,
    modificado_top = as.integer(tabla[[columna_top]]),
    modificado_final = as.integer(igraph::membership(resultado))
  )
}

extraer_oficial <- function(resultado, prefijo) {
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

  names(top)[2] <- paste0(prefijo, "_top")
  names(final)[2] <- paste0(prefijo, "_final")

  merge(top, final, by = "node_id", all = TRUE, sort = TRUE)
}

g <- cargar_yeast()

cat("Version igraph:", as.character(packageVersion("igraph")), "\n")
cat("Version Infomap oficial:", as.character(packageVersion("infomap")), "\n")
cat("Nodos:", igraph::vcount(g), "\n")
cat("Aristas:", igraph::ecount(g), "\n")
cat("Semilla:", semilla, "\n")
cat("Ensayos:", ensayos, "\n\n")

cat("1/3 Ejecutando igraph modificado...\n")
set.seed(semilla)

resultado_modificado <- igraph::cluster_infomap(
  g,
  e.weights = igraph::edge_attr(g, "weight"),
  nb.trials = ensayos
)

cat("2/3 Ejecutando Infomap oficial jerarquico...\n")
resultado_jerarquico <- infomap::cluster_infomap(
  g,
  weight = "weight",
  nb.trials = ensayos,
  seed = semilla,
  regularized = FALSE,
  two_level = FALSE,
  silent = TRUE,
  tibble = FALSE
)

cat("3/3 Ejecutando Infomap oficial two-level...\n")
resultado_two_level <- infomap::cluster_infomap(
  g,
  weight = "weight",
  nb.trials = ensayos,
  seed = semilla,
  regularized = FALSE,
  two_level = TRUE,
  silent = TRUE,
  tibble = FALSE
)

tabla <- Reduce(
  function(x, y) merge(x, y, by = "node_id", all = TRUE, sort = TRUE),
  list(
    extraer_modificado(resultado_modificado),
    extraer_oficial(resultado_jerarquico, "oficial_jerarquico"),
    extraer_oficial(resultado_two_level, "oficial_two_level")
  )
)

write.csv(
  tabla,
  file.path(out_dir, "yeast_comparacion_por_nodo.csv"),
  row.names = FALSE
)

comparaciones <- rbind(
  data.frame(
    comparacion = "modificado_top_vs_oficial_jerarquico_top",
    metricas(
      tabla$modificado_top,
      tabla$oficial_jerarquico_top
    )
  ),
  data.frame(
    comparacion = "modificado_final_vs_oficial_jerarquico_final",
    metricas(
      tabla$modificado_final,
      tabla$oficial_jerarquico_final
    )
  ),
  data.frame(
    comparacion = "modificado_final_vs_oficial_two_level",
    metricas(
      tabla$modificado_final,
      tabla$oficial_two_level_final
    )
  ),
  data.frame(
    comparacion = "modificado_top_vs_oficial_two_level",
    metricas(
      tabla$modificado_top,
      tabla$oficial_two_level_final
    )
  ),
  data.frame(
    comparacion = "oficial_jerarquico_final_vs_two_level",
    metricas(
      tabla$oficial_jerarquico_final,
      tabla$oficial_two_level_final
    )
  )
)

write.csv(
  comparaciones,
  file.path(out_dir, "metricas_two_level_yeast.csv"),
  row.names = FALSE
)

resumen <- data.frame(
  configuracion = c(
    "igraph_modificado_final",
    "igraph_modificado_top",
    "oficial_jerarquico_top",
    "oficial_jerarquico_final",
    "oficial_two_level"
  ),
  comunidades = c(
    length(unique(tabla$modificado_final)),
    length(unique(tabla$modificado_top)),
    length(unique(tabla$oficial_jerarquico_top)),
    length(unique(tabla$oficial_jerarquico_final)),
    length(unique(tabla$oficial_two_level_final))
  ),
  stringsAsFactors = FALSE
)

write.csv(
  resumen,
  file.path(out_dir, "resumen_comunidades.csv"),
  row.names = FALSE
)

capture.output(
  {
    cat("============================================================\n")
    cat("YEAST: INFOMAP JERARQUICO VS TWO-LEVEL\n")
    cat("============================================================\n\n")
    cat("Version igraph:", as.character(packageVersion("igraph")), "\n")
    cat(
      "Version Infomap oficial:",
      as.character(packageVersion("infomap")),
      "\n"
    )
    cat("Semilla:", semilla, "\n")
    cat("Ensayos:", ensayos, "\n")
    cat("Nodos:", igraph::vcount(g), "\n")
    cat("Aristas:", igraph::ecount(g), "\n\n")

    cat("NUMERO DE COMUNIDADES\n\n")
    print(resumen)

    cat("\nCOMPARACIONES NMI Y ARI\n\n")
    print(comparaciones)

    cat("\nINTERPRETACION\n\n")

    nmi_jerarquico <- comparaciones$nmi[
      comparaciones$comparacion ==
        "modificado_final_vs_oficial_jerarquico_final"
    ]

    nmi_two_level <- comparaciones$nmi[
      comparaciones$comparacion ==
        "modificado_final_vs_oficial_two_level"
    ]

    if (nmi_two_level > nmi_jerarquico) {
      cat(
        "La solucion two-level oficial es mas cercana a la ",
        "particion final del igraph modificado.\n",
        sep = ""
      )
      cat(
        "Esto aporta evidencia experimental a favor de que ",
        "la diferencia generacional y la estrategia jerarquica ",
        "influyen en la discrepancia.\n",
        sep = ""
      )
    } else if (nmi_two_level < nmi_jerarquico) {
      cat(
        "La solucion two-level oficial no es mas cercana a la ",
        "particion final del igraph modificado.\n",
        sep = ""
      )
      cat(
        "La hipotesis sobre la estrategia jerarquica no queda ",
        "respaldada por esta prueba aislada.\n",
        sep = ""
      )
    } else {
      cat(
        "Ambas configuraciones oficiales presentan el mismo NMI ",
        "respecto de la particion final modificada.\n",
        sep = ""
      )
    }
  },
  file = file.path(out_dir, "resumen_two_level_yeast.txt")
)

cat("\n============================================================\n")
cat("PRUEBA TERMINADA\n")
cat("============================================================\n")
cat(file.path(out_dir, "resumen_two_level_yeast.txt"), "\n")
