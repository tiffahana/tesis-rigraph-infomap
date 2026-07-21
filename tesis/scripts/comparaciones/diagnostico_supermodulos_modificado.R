cat("====================================\n")
cat("DIAGNOSTICO DE SUPERMODULOS - IGRAPH MODIFICADO\n")
cat("====================================\n\n")

library(igraph)

if (!requireNamespace("igraphdata", quietly = TRUE)) {
  stop("Falta instalar igraphdata.")
}

base_dir <- normalizePath(
  ".",
  mustWork = TRUE
)

out_dir <- file.path(base_dir, "tesis", "resultados", "comparaciones")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

cat("igraph cargado desde:\n")
print(find.package("igraph"))

cat("\nVersion de igraph:\n")
print(packageVersion("igraph"))

cargar_red <- function(nombre) {
  if (nombre == "Zachary") {
    return(make_graph("Zachary"))
  }

  env <- new.env()
  data(list = nombre, package = "igraphdata", envir = env)
  g <- get(nombre, envir = env)

  if (inherits(g, "igraph")) {
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

  # Si no tiene pesos, se asigna peso 1 a cada arista.
  # Esto permite conservar multiplicidad cuando se colapsan aristas repetidas.
  if (!"weight" %in% edge_attr_names(g)) {
    E(g)$weight <- 1
  }

  # Convertir a no dirigida, conservando pesos.
  if (is_directed(g)) {
    g <- as_undirected(
      g,
      mode = "collapse",
      edge.attr.comb = list(weight = "sum", "ignore")
    )
  }

  # Eliminar loops y aristas mÃºltiples conservando suma de pesos.
  g <- simplify(
    g,
    remove.multiple = TRUE,
    remove.loops = TRUE,
    edge.attr.comb = list(weight = "sum", "ignore")
  )

  # Usar componente gigante si hay mÃ¡s de una componente.
  comp <- components(g)
  if (comp$no > 1) {
    g <- tomar_componente_gigante(g)
  }

  return(g)
}

ejecutar_infomap_modificado <- function(nombre) {
  cat("\n------------------------------------\n")
  cat("Red:", nombre, "\n")
  cat("------------------------------------\n")

  g_original <- cargar_red(nombre)

  nodos_originales <- vcount(g_original)
  aristas_originales <- ecount(g_original)
  dirigida_original <- is_directed(g_original)
  componentes_originales <- components(as_undirected(g_original))$no

  cat("Nodos originales:", nodos_originales, "\n")
  cat("Aristas originales:", aristas_originales, "\n")
  cat("Dirigida original:", dirigida_original, "\n")
  cat("Componentes originales:", componentes_originales, "\n")

  g <- preparar_red(g_original)

  cat("Nodos usados:", vcount(g), "\n")
  cat("Aristas usadas:", ecount(g), "\n")
  cat("Dirigida usada:", is_directed(g), "\n")
  cat("Componentes usadas:", components(g)$no, "\n")
  cat("Ponderada usada:", "weight" %in% edge_attr_names(g), "\n")

  set.seed(123)

  res <- cluster_infomap(
    g,
    e.weights = E(g)$weight,
    nb.trials = 100
  )

  if (!"multilevel_modules" %in% names(res)) {
    stop("Este igraph no parece ser el modificado: falta multilevel_modules.")
  }

  comunidades_finales <- length(unique(membership(res)))

  final_module_ok <- all(
    res$multilevel_modules[, "final_module"] == membership(res)
  )

  tiene_hierarchical_codelength <- "hierarchical_codelength" %in% names(res)

  cat("Comunidades finales:", comunidades_finales, "\n")
  cat("num_levels:", res$num_levels, "\n")
  cat("num_top_modules:", res$num_top_modules, "\n")
  cat("max_tree_depth:", res$max_tree_depth, "\n")
  cat("final_module_ok:", final_module_ok, "\n")
  cat("codelength:", res$codelength, "\n")
  cat("modularity:", modularity(res), "\n")
  cat("hierarchical_codelength presente:", tiene_hierarchical_codelength, "\n")

  write.csv(
    res$multilevel_modules,
    file.path(out_dir, paste0(nombre, "_multilevel_modules_modificado.csv")),
    row.names = FALSE
  )

  data.frame(
    red = nombre,
    nodos_originales = nodos_originales,
    aristas_originales = aristas_originales,
    dirigida_original = dirigida_original,
    componentes_originales = componentes_originales,
    nodos_usados = vcount(g),
    aristas_usadas = ecount(g),
    comunidades_finales = comunidades_finales,
    num_levels = res$num_levels,
    num_top_modules = res$num_top_modules,
    max_tree_depth = res$max_tree_depth,
    codelength = res$codelength,
    modularity = modularity(res),
    final_module_ok = final_module_ok,
    hierarchical_codelength_presente = tiene_hierarchical_codelength,
    tiene_supermodulos = res$num_top_modules > 1,
    tiene_multinivel = res$max_tree_depth >= 2,
    stringsAsFactors = FALSE
  )
}

redes <- c(
  "Zachary",
  "USairports",
  "enron",
  "UKfaculty",
  "yeast"
)

resultados <- do.call(
  rbind,
  lapply(redes, ejecutar_infomap_modificado)
)

cat("\n====================================\n")
cat("RESUMEN FINAL\n")
cat("====================================\n\n")

print(resultados)

write.csv(
  resultados,
  file.path(out_dir, "diagnostico_supermodulos_modificado.csv"),
  row.names = FALSE
)

capture.output(
  {
    cat("====================================\n")
    cat("DIAGNOSTICO DE SUPERMODULOS - IGRAPH MODIFICADO\n")
    cat("====================================\n\n")

    print(resultados)

    cat("\nRedes con supermodulos:\n")
    print(resultados[resultados$tiene_supermodulos, ])

    cat("\nRedes con estructura multinivel:\n")
    print(resultados[resultados$tiene_multinivel, ])
  },
  file = file.path(out_dir, "diagnostico_supermodulos_modificado.txt")
)

cat("\nArchivos generados:\n")
cat(file.path(out_dir, "diagnostico_supermodulos_modificado.csv"), "\n")
cat(file.path(out_dir, "diagnostico_supermodulos_modificado.txt"), "\n")

cat("\nFIN DIAGNOSTICO\n")
