
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

