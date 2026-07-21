cat("====================================\n")
cat("COMPARACION YEAST REFORZADA - NB.TRIALS 500\n")
cat("====================================\n\n")

base_dir <- normalizePath(
  ".",
  mustWork = TRUE
)

out_dir <- file.path(base_dir, "tesis", "resultados", "comparaciones")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

original_lib <- Sys.getenv("IGRAPH_ORIGINAL_LIB", unset = file.path(base_dir, "tesis", "lib", "igraph_original"))

if (!dir.exists(original_lib)) {
  stop(paste("No existe original_igraph_lib en:", original_lib))
}

rscript <- file.path(R.home("bin"), "Rscript.exe")

if (!file.exists(rscript)) {
  rscript <- file.path(R.home("bin"), "Rscript")
}

cat("Rscript usado:\n")
cat(rscript, "\n\n")

cat("Carpeta de salida:\n")
cat(out_dir, "\n\n")

# ============================================================
# SCRIPT 1: YEAST CON IGRAPH ORIGINAL
# ============================================================

script_original <- file.path(out_dir, "yeast_original_500_runner.R")

codigo_original <- r"(
cat("====================================\n")
cat("YEAST - IGRAPH ORIGINAL - NB.TRIALS 500\n")
cat("====================================\n\n")

base_dir <- normalizePath(
  ".",
  mustWork = TRUE
)

out_dir <- file.path(base_dir, "tesis", "resultados", "comparaciones")
original_lib <- Sys.getenv("IGRAPH_ORIGINAL_LIB", unset = file.path(base_dir, "tesis", "lib", "igraph_original"))

library(igraph, lib.loc = original_lib)

if (!requireNamespace("igraphdata", quietly = TRUE)) {
  stop("Falta instalar igraphdata.")
}

cat("igraph original cargado desde:\n")
print(find.package("igraph"))

cat("\nVersion de igraph original:\n")
print(packageVersion("igraph"))

cargar_yeast <- function() {
  env <- new.env()
  data(list = "yeast", package = "igraphdata", envir = env)
  g <- get("yeast", envir = env)

  if (exists("upgrade_graph", where = asNamespace("igraph"), mode = "function")) {
    g <- upgrade_graph(g)
  }

  return(g)
}

tomar_componente_gigante <- function(g) {
  comp <- components(as.undirected(g))
  gigante <- which.max(comp$csize)
  induced_subgraph(g, which(comp$membership == gigante))
}

preparar_red <- function(g) {
  if (exists("upgrade_graph", where = asNamespace("igraph"), mode = "function")) {
    g <- upgrade_graph(g)
  }

  if (!"weight" %in% edge_attr_names(g)) {
    E(g)$weight <- 1
  }

  if (is_directed(g)) {
    g <- as.undirected(
      g,
      mode = "collapse",
      edge.attr.comb = list(weight = "sum", "ignore")
    )
  }

  g <- simplify(
    g,
    remove.multiple = TRUE,
    remove.loops = TRUE,
    edge.attr.comb = list(weight = "sum", "ignore")
  )

  comp <- components(g)

  if (comp$no > 1) {
    g <- tomar_componente_gigante(g)
  }

  return(g)
}

g_original <- cargar_yeast()

nodos_originales <- vcount(g_original)
aristas_originales <- ecount(g_original)
componentes_originales <- components(as.undirected(g_original))$no

g <- preparar_red(g_original)

set.seed(123)

res <- cluster_infomap(
  g,
  e.weights = E(g)$weight,
  nb.trials = 500
)

membership_original <- as.integer(membership(res))

tabla_membership <- data.frame(
  node_id = seq_len(vcount(g)),
  membership_original = membership_original
)

write.csv(
  tabla_membership,
  file.path(out_dir, "yeast_membership_original_500.csv"),
  row.names = FALSE
)

resumen_original <- data.frame(
  red = "yeast",
  implementacion = "igraph_original",
  nb_trials = 500,
  nodos_originales = nodos_originales,
  aristas_originales = aristas_originales,
  componentes_originales = componentes_originales,
  nodos_usados = vcount(g),
  aristas_usadas = ecount(g),
  comunidades_finales = length(unique(membership_original)),
  codelength = res$codelength,
  modularity = modularity(res),
  stringsAsFactors = FALSE
)

write.csv(
  resumen_original,
  file.path(out_dir, "yeast_resumen_original_500.csv"),
  row.names = FALSE
)

cat("\nResumen original:\n")
print(resumen_original)

cat("\nFIN YEAST ORIGINAL 500\n")
)"

writeLines(codigo_original, script_original)

# ============================================================
# SCRIPT 2: YEAST CON IGRAPH MODIFICADO
# ============================================================

script_modificado <- file.path(out_dir, "yeast_modificado_500_runner.R")

codigo_modificado <- r"(
cat("====================================\n")
cat("YEAST - IGRAPH MODIFICADO - NB.TRIALS 500\n")
cat("====================================\n\n")

base_dir <- normalizePath(
  ".",
  mustWork = TRUE
)

out_dir <- file.path(base_dir, "tesis", "resultados", "comparaciones")

library(igraph)

if (!requireNamespace("igraphdata", quietly = TRUE)) {
  stop("Falta instalar igraphdata.")
}

cat("igraph modificado cargado desde:\n")
print(find.package("igraph"))

cat("\nVersion de igraph modificado:\n")
print(packageVersion("igraph"))

cargar_yeast <- function() {
  env <- new.env()
  data(list = "yeast", package = "igraphdata", envir = env)
  g <- get("yeast", envir = env)

  if (exists("upgrade_graph", where = asNamespace("igraph"), mode = "function")) {
    g <- upgrade_graph(g)
  }

  return(g)
}

tomar_componente_gigante <- function(g) {
  comp <- components(as_undirected(g))
  gigante <- which.max(comp$csize)
  induced_subgraph(g, which(comp$membership == gigante))
}

preparar_red <- function(g) {
  g <- upgrade_graph(g)

  if (!"weight" %in% edge_attr_names(g)) {
    E(g)$weight <- 1
  }

  if (is_directed(g)) {
    g <- as_undirected(
      g,
      mode = "collapse",
      edge.attr.comb = list(weight = "sum", "ignore")
    )
  }

  g <- simplify(
    g,
    remove.multiple = TRUE,
    remove.loops = TRUE,
    edge.attr.comb = list(weight = "sum", "ignore")
  )

  comp <- components(g)

  if (comp$no > 1) {
    g <- tomar_componente_gigante(g)
  }

  return(g)
}

g_original <- cargar_yeast()

nodos_originales <- vcount(g_original)
aristas_originales <- ecount(g_original)
componentes_originales <- components(as_undirected(g_original))$no

g <- preparar_red(g_original)

set.seed(123)

res <- cluster_infomap(
  g,
  e.weights = E(g)$weight,
  nb.trials = 500
)

if (!"multilevel_modules" %in% names(res)) {
  stop("Este igraph no parece ser el modificado: falta multilevel_modules.")
}

membership_modificado <- as.integer(membership(res))

tabla_membership <- data.frame(
  node_id = seq_len(vcount(g)),
  membership_modificado = membership_modificado,
  final_module = res$multilevel_modules[, "final_module"]
)

write.csv(
  tabla_membership,
  file.path(out_dir, "yeast_membership_modificado_500.csv"),
  row.names = FALSE
)

write.csv(
  res$multilevel_modules,
  file.path(out_dir, "yeast_multilevel_modules_modificado_500.csv"),
  row.names = FALSE
)

resumen_modificado <- data.frame(
  red = "yeast",
  implementacion = "igraph_modificado",
  nb_trials = 500,
  nodos_originales = nodos_originales,
  aristas_originales = aristas_originales,
  componentes_originales = componentes_originales,
  nodos_usados = vcount(g),
  aristas_usadas = ecount(g),
  comunidades_finales = length(unique(membership_modificado)),
  codelength = res$codelength,
  modularity = modularity(res),
  num_levels = res$num_levels,
  num_top_modules = res$num_top_modules,
  max_tree_depth = res$max_tree_depth,
  final_module_ok = all(res$multilevel_modules[, "final_module"] == membership_modificado),
  hierarchical_codelength_presente = "hierarchical_codelength" %in% names(res),
  stringsAsFactors = FALSE
)

write.csv(
  resumen_modificado,
  file.path(out_dir, "yeast_resumen_modificado_500.csv"),
  row.names = FALSE
)

cat("\nResumen modificado:\n")
print(resumen_modificado)

cat("\nFIN YEAST MODIFICADO 500\n")
)"

writeLines(codigo_modificado, script_modificado)

# ============================================================
# EJECUTAR AMBOS SCRIPTS EN SESIONES SEPARADAS
# ============================================================

cat("Ejecutando Yeast con igraph original...\n\n")
estado_original <- system2(
  rscript,
  args = c("--vanilla", shQuote(script_original))
)

if (estado_original != 0) {
  stop("Fallo la ejecucion con igraph original.")
}

cat("\nEjecutando Yeast con igraph modificado...\n\n")
estado_modificado <- system2(
  rscript,
  args = c("--vanilla", shQuote(script_modificado))
)

if (estado_modificado != 0) {
  stop("Fallo la ejecucion con igraph modificado.")
}

# ============================================================
# COMPARACION FINAL
# ============================================================

normalizar_membresia <- function(x) {
  as.integer(factor(x, levels = unique(x)))
}

comparar_particiones <- function(a, b) {
  a_norm <- normalizar_membresia(a)
  b_norm <- normalizar_membresia(b)

  all(a_norm == b_norm)
}

resumen_original <- read.csv(
  file.path(out_dir, "yeast_resumen_original_500.csv")
)

resumen_modificado <- read.csv(
  file.path(out_dir, "yeast_resumen_modificado_500.csv")
)

memb_original <- read.csv(
  file.path(out_dir, "yeast_membership_original_500.csv")
)

memb_modificado <- read.csv(
  file.path(out_dir, "yeast_membership_modificado_500.csv")
)

a <- memb_original$membership_original
b <- memb_modificado$final_module

comparacion <- data.frame(
  red = "yeast",
  nb_trials = 500,
  nodos_usados_original = resumen_original$nodos_usados,
  aristas_usadas_original = resumen_original$aristas_usadas,
  comunidades_original = resumen_original$comunidades_finales,
  comunidades_modificado = resumen_modificado$comunidades_finales,
  misma_cantidad_comunidades = resumen_original$comunidades_finales == resumen_modificado$comunidades_finales,
  membership_exactamente_igual = all(a == b),
  misma_particion = comparar_particiones(a, b),
  codelength_original = resumen_original$codelength,
  codelength_modificado = resumen_modificado$codelength,
  diferencia_codelength = abs(resumen_original$codelength - resumen_modificado$codelength),
  modularity_original = resumen_original$modularity,
  modularity_modificado = resumen_modificado$modularity,
  diferencia_modularity = abs(resumen_original$modularity - resumen_modificado$modularity),
  num_levels = resumen_modificado$num_levels,
  num_top_modules = resumen_modificado$num_top_modules,
  max_tree_depth = resumen_modificado$max_tree_depth,
  final_module_ok = resumen_modificado$final_module_ok,
  hierarchical_codelength_presente = resumen_modificado$hierarchical_codelength_presente,
  stringsAsFactors = FALSE
)

cat("\n====================================\n")
cat("COMPARACION YEAST REFORZADA\n")
cat("====================================\n\n")

print(comparacion)

write.csv(
  comparacion,
  file.path(out_dir, "comparacion_yeast_reforzado_500.csv"),
  row.names = FALSE
)

capture.output(
  {
    cat("====================================\n")
    cat("COMPARACION YEAST REFORZADA - NB.TRIALS 500\n")
    cat("====================================\n\n")

    print(comparacion)

    cat("\nInterpretacion rapida:\n\n")
    cat("- Misma cantidad de comunidades:", comparacion$misma_cantidad_comunidades, "\n")
    cat("- Membership exactamente igual:", comparacion$membership_exactamente_igual, "\n")
    cat("- Misma particion:", comparacion$misma_particion, "\n")
    cat("- Diferencia codelength:", comparacion$diferencia_codelength, "\n")
    cat("- Diferencia modularity:", comparacion$diferencia_modularity, "\n")
    cat("- num_levels:", comparacion$num_levels, "\n")
    cat("- num_top_modules:", comparacion$num_top_modules, "\n")
    cat("- max_tree_depth:", comparacion$max_tree_depth, "\n")
    cat("- final_module_ok:", comparacion$final_module_ok, "\n")
    cat("- hierarchical_codelength presente:", comparacion$hierarchical_codelength_presente, "\n")
  },
  file = file.path(out_dir, "comparacion_yeast_reforzado_500.txt")
)

cat("\nArchivos generados:\n")
cat(file.path(out_dir, "comparacion_yeast_reforzado_500.csv"), "\n")
cat(file.path(out_dir, "comparacion_yeast_reforzado_500.txt"), "\n")
cat(file.path(out_dir, "yeast_multilevel_modules_modificado_500.csv"), "\n")

cat("\nFIN COMPARACION YEAST REFORZADA\n")