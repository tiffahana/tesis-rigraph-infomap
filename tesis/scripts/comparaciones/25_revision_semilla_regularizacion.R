# Revision metodologica: semilla, niveles equivalentes y regularizacion
#
# Ejecutar desde la raiz del repositorio:
# Rscript tesis/scripts/comparaciones/25_revision_semilla_regularizacion.R
#
# Este script NO reemplaza los resultados anteriores. Genera una carpeta nueva
# para revisar:
#   1. igraph modificado frente a Infomap oficial sin regularizacion.
#   2. Correspondencia entre niveles jerarquicos.
#   3. Sensibilidad de Infomap oficial a Regularized Map Equation.
#   4. NMI y ARI para cada comparacion.

cat("============================================================\n")
cat("REVISION: SEMILLA, NIVELES Y REGULARIZACION\n")
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

repo_dir <- normalizePath(
  ".",
  winslash = "/",
  mustWork = TRUE
)

if (!file.exists(file.path(repo_dir, "DESCRIPTION")) ||
    !dir.exists(file.path(repo_dir, "tesis"))) {
  stop("Ejecute este script desde la raiz del repositorio.")
}

out_dir <- file.path(
  repo_dir,
  "tesis",
  "resultados",
  "comparaciones",
  "revision_semilla_regularizacion"
)

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

semilla <- 123L
ensayos <- 500L
redes <- c("Zachary", "USairports", "yeast")

cat("Semilla:", semilla, "\n")
cat("Ensayos por ejecucion:", ensayos, "\n")
cat("Version igraph:", as.character(packageVersion("igraph")), "\n")
cat("Version Infomap oficial:", as.character(packageVersion("infomap")), "\n")
cat("Salida:", out_dir, "\n\n")

safe_num <- function(expr) {
  valor <- tryCatch(
    as.numeric(expr),
    error = function(e) NA_real_
  )

  if (length(valor) == 0L) {
    return(NA_real_)
  }

  valor[[1]]
}

normalizar_membresia <- function(x) {
  as.integer(factor(x, levels = unique(x)))
}

calcular_metricas <- function(a, b) {
  ok <- complete.cases(a, b)
  a <- as.integer(a[ok])
  b <- as.integer(b[ok])

  if (length(a) == 0L) {
    return(data.frame(
      nodos_comparados = 0L,
      comunidades_a = NA_integer_,
      comunidades_b = NA_integer_,
      misma_particion = NA,
      nmi = NA_real_,
      ari = NA_real_
    ))
  }

  a_norm <- normalizar_membresia(a)
  b_norm <- normalizar_membresia(b)

  data.frame(
    nodos_comparados = length(a_norm),
    comunidades_a = length(unique(a_norm)),
    comunidades_b = length(unique(b_norm)),
    misma_particion = all(a_norm == b_norm),
    nmi = safe_num(
      igraph::compare(a_norm, b_norm, method = "nmi")
    ),
    ari = safe_num(
      igraph::compare(a_norm, b_norm, method = "adjusted.rand")
    )
  )
}

cargar_red <- function(nombre) {
  if (nombre == "Zachary") {
    return(igraph::make_graph("Zachary"))
  }

  env <- new.env()
  data(
    list = nombre,
    package = "igraphdata",
    envir = env
  )

  g <- get(nombre, envir = env)

  if (exists(
    "upgrade_graph",
    where = asNamespace("igraph"),
    mode = "function"
  )) {
    g <- igraph::upgrade_graph(g)
  }

  g
}

tomar_componente_gigante <- function(g) {
  comp <- igraph::components(igraph::as_undirected(g))
  gigante <- which.max(comp$csize)

  igraph::induced_subgraph(
    g,
    which(comp$membership == gigante)
  )
}

preparar_red <- function(g) {
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
    g <- tomar_componente_gigante(g)
  }

  g
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

  tabla <- merge(
    top,
    final,
    by = "node_id",
    all = TRUE,
    sort = TRUE
  )

  tabla[order(tabla$node_id), , drop = FALSE]
}

extraer_modificado <- function(resultado) {
  if (!"multilevel_modules" %in% names(resultado)) {
    stop(
      "La version cargada de igraph no contiene ",
      "el campo multilevel_modules."
    )
  }

  tabla <- as.data.frame(resultado$multilevel_modules)

  if (!"node_id" %in% names(tabla)) {
    tabla$node_id <- seq_len(nrow(tabla))
  }

  tabla$membership_igraph <- as.integer(
    igraph::membership(resultado)
  )

  tabla[order(tabla$node_id), , drop = FALSE]
}

resumenes <- list()
metricas_niveles <- list()
metricas_regularizacion <- list()

for (nombre in redes) {
  cat("\n------------------------------------------------------------\n")
  cat("Red:", nombre, "\n")
  cat("------------------------------------------------------------\n")

  g_original <- cargar_red(nombre)
  g <- preparar_red(g_original)

  cat("Nodos usados:", igraph::vcount(g), "\n")
  cat("Aristas usadas:", igraph::ecount(g), "\n")

  # Igraph modificado. set.seed() controla el RNG usado por igraph desde R.
  set.seed(semilla)

  resultado_modificado <- igraph::cluster_infomap(
    g,
    e.weights = igraph::edge_attr(g, "weight"),
    nb.trials = ensayos
  )

  # Infomap oficial sin regularizacion: comparacion principal equivalente.
  resultado_oficial <- infomap::cluster_infomap(
    g,
    weight = "weight",
    nb.trials = ensayos,
    seed = semilla,
    regularized = FALSE,
    silent = TRUE,
    tibble = FALSE
  )

  # Infomap oficial regularizado: analisis complementario.
  resultado_regularizado <- infomap::cluster_infomap(
    g,
    weight = "weight",
    nb.trials = ensayos,
    seed = semilla,
    regularized = TRUE,
    silent = TRUE,
    tibble = FALSE
  )

  tabla_mod <- extraer_modificado(resultado_modificado)
  tabla_oficial <- extraer_oficial(
    resultado_oficial,
    "oficial_no_regularizado"
  )
  tabla_regularizada <- extraer_oficial(
    resultado_regularizado,
    "oficial_regularizado"
  )

  tabla_nodos <- merge(
    tabla_mod,
    tabla_oficial,
    by = "node_id",
    all = TRUE,
    sort = TRUE
  )

  tabla_nodos <- merge(
    tabla_nodos,
    tabla_regularizada,
    by = "node_id",
    all = TRUE,
    sort = TRUE
  )

  archivo_nodos <- file.path(
    out_dir,
    paste0(nombre, "_comparacion_por_nodo.csv")
  )

  write.csv(
    tabla_nodos,
    archivo_nodos,
    row.names = FALSE
  )

  columnas_modificadas <- names(tabla_mod)[
    grepl("^level_[0-9]+$", names(tabla_mod)) |
      names(tabla_mod) %in% c(
        "top_module",
        "final_module",
        "membership_igraph"
      )
  ]

  columnas_oficiales <- c(
    "oficial_no_regularizado_top",
    "oficial_no_regularizado_final",
    "oficial_regularizado_top",
    "oficial_regularizado_final"
  )

  for (col_oficial in columnas_oficiales) {
    for (col_mod in columnas_modificadas) {
      m <- calcular_metricas(
        tabla_nodos[[col_mod]],
        tabla_nodos[[col_oficial]]
      )

      metricas_niveles[[length(metricas_niveles) + 1L]] <- data.frame(
        red = nombre,
        semilla = semilla,
        ensayos = ensayos,
        particion_modificada = col_mod,
        particion_oficial = col_oficial,
        m,
        stringsAsFactors = FALSE
      )
    }
  }

  m_top_reg <- calcular_metricas(
    tabla_nodos$oficial_no_regularizado_top,
    tabla_nodos$oficial_regularizado_top
  )

  m_final_reg <- calcular_metricas(
    tabla_nodos$oficial_no_regularizado_final,
    tabla_nodos$oficial_regularizado_final
  )

  metricas_regularizacion[[length(metricas_regularizacion) + 1L]] <-
    data.frame(
      red = nombre,
      nivel = "top",
      semilla = semilla,
      ensayos = ensayos,
      m_top_reg,
      stringsAsFactors = FALSE
    )

  metricas_regularizacion[[length(metricas_regularizacion) + 1L]] <-
    data.frame(
      red = nombre,
      nivel = "final",
      semilla = semilla,
      ensayos = ensayos,
      m_final_reg,
      stringsAsFactors = FALSE
    )

  resumenes[[length(resumenes) + 1L]] <- data.frame(
    red = nombre,
    nodos = igraph::vcount(g),
    aristas = igraph::ecount(g),
    semilla = semilla,
    ensayos = ensayos,

    comunidades_modificado_membership =
      length(unique(igraph::membership(resultado_modificado))),
    num_levels_modificado =
      safe_num(resultado_modificado$num_levels),
    num_top_modules_modificado =
      safe_num(resultado_modificado$num_top_modules),
    max_tree_depth_modificado =
      safe_num(resultado_modificado$max_tree_depth),
    codelength_modificado =
      safe_num(resultado_modificado$codelength),

    comunidades_oficial_no_regularizado_top =
      length(unique(tabla_oficial$oficial_no_regularizado_top)),
    comunidades_oficial_no_regularizado_final =
      length(unique(tabla_oficial$oficial_no_regularizado_final)),
    num_levels_oficial_no_regularizado =
      safe_num(resultado_oficial$model$num_levels),
    num_top_modules_oficial_no_regularizado =
      safe_num(resultado_oficial$model$num_top_modules),
    max_tree_depth_oficial_no_regularizado =
      safe_num(resultado_oficial$model$max_tree_depth),
    codelength_oficial_no_regularizado =
      safe_num(resultado_oficial$codelength),

    comunidades_oficial_regularizado_top =
      length(unique(tabla_regularizada$oficial_regularizado_top)),
    comunidades_oficial_regularizado_final =
      length(unique(tabla_regularizada$oficial_regularizado_final)),
    num_levels_oficial_regularizado =
      safe_num(resultado_regularizado$model$num_levels),
    num_top_modules_oficial_regularizado =
      safe_num(resultado_regularizado$model$num_top_modules),
    max_tree_depth_oficial_regularizado =
      safe_num(resultado_regularizado$model$max_tree_depth),
    codelength_oficial_regularizado =
      safe_num(resultado_regularizado$codelength),

    stringsAsFactors = FALSE
  )

  cat("Archivo por nodo:", archivo_nodos, "\n")
}

resumen <- do.call(rbind, resumenes)
comparacion_niveles <- do.call(rbind, metricas_niveles)
comparacion_regularizacion <- do.call(
  rbind,
  metricas_regularizacion
)

comparacion_niveles <- comparacion_niveles[
  order(
    comparacion_niveles$red,
    comparacion_niveles$particion_oficial,
    -comparacion_niveles$nmi
  ),
  ,
  drop = FALSE
]

write.csv(
  resumen,
  file.path(out_dir, "resumen_ejecuciones.csv"),
  row.names = FALSE
)

write.csv(
  comparacion_niveles,
  file.path(out_dir, "comparacion_niveles_nmi_ari.csv"),
  row.names = FALSE
)

write.csv(
  comparacion_regularizacion,
  file.path(out_dir, "efecto_regularizacion_nmi_ari.csv"),
  row.names = FALSE
)

# Registrar los valores por defecto informados por la interfaz instalada.
opciones <- infomap::infomap_options()

valor_opcion <- function(nombre) {
  valor <- opciones[[nombre]]

  if (is.null(valor) || length(valor) == 0L) {
    return(NA_character_)
  }

  paste(valor, collapse = ",")
}

configuracion <- data.frame(
  opcion = c("regularized", "regularization_strength", "seed", "num_trials"),
  valor_por_defecto_interfaz = c(
    valor_opcion("regularized"),
    valor_opcion("regularization_strength"),
    valor_opcion("seed"),
    valor_opcion("num_trials")
  ),
  stringsAsFactors = FALSE
)

write.csv(
  configuracion,
  file.path(out_dir, "opciones_infomap_instalado.csv"),
  row.names = FALSE
)

capture.output(
  {
    cat("============================================================\n")
    cat("REVISION: SEMILLA, NIVELES Y REGULARIZACION\n")
    cat("============================================================\n\n")
    cat("Version igraph:", as.character(packageVersion("igraph")), "\n")
    cat(
      "Version Infomap oficial:",
      as.character(packageVersion("infomap")),
      "\n"
    )
    cat("Semilla:", semilla, "\n")
    cat("Ensayos:", ensayos, "\n\n")

    cat("RESUMEN DE EJECUCIONES\n\n")
    print(resumen)

    cat("\nMEJOR CORRESPONDENCIA POR PARTICION OFICIAL\n\n")

    grupos <- split(
      comparacion_niveles,
      interaction(
        comparacion_niveles$red,
        comparacion_niveles$particion_oficial,
        drop = TRUE
      )
    )

    mejores <- do.call(
      rbind,
      lapply(grupos, function(x) {
        x[which.max(ifelse(is.na(x$nmi), -Inf, x$nmi)), , drop = FALSE]
      })
    )

    print(mejores)

    cat("\nEFECTO DE LA REGULARIZACION EN INFOMAP OFICIAL\n\n")
    print(comparacion_regularizacion)

    cat("\nOPCIONES POR DEFECTO DE LA INTERFAZ INSTALADA\n\n")
    print(configuracion)
  },
  file = file.path(out_dir, "resumen_revision.txt")
)

cat("\n============================================================\n")
cat("ARCHIVOS GENERADOS\n")
cat("============================================================\n")
cat(file.path(out_dir, "resumen_ejecuciones.csv"), "\n")
cat(file.path(out_dir, "comparacion_niveles_nmi_ari.csv"), "\n")
cat(file.path(out_dir, "efecto_regularizacion_nmi_ari.csv"), "\n")
cat(file.path(out_dir, "opciones_infomap_instalado.csv"), "\n")
cat(file.path(out_dir, "resumen_revision.txt"), "\n")
cat("\nRevision terminada correctamente.\n")
