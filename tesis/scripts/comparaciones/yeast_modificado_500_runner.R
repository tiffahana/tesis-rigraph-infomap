
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

