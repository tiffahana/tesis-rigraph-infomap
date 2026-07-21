cat("====================================\n")
cat("COMPARACION IGRAPH ORIGINAL VS MODIFICADO\n")
cat("====================================\n\n")

base_dir <- normalizePath(
  ".",
  mustWork = TRUE
)

out_dir <- file.path(base_dir, "tesis", "resultados", "comparaciones")
original_lib <- Sys.getenv("IGRAPH_ORIGINAL_LIB", unset = file.path(base_dir, "tesis", "lib", "igraph_original"))

if (!dir.exists(original_lib)) {
  stop(paste("No existe la carpeta original_igraph_lib en:", original_lib))
}

cat("Libreria original esperada en:\n")
cat(original_lib, "\n\n")

library(igraph, lib.loc = original_lib)

if (!requireNamespace("igraphdata", quietly = TRUE)) {
  stop("Falta instalar igraphdata.")
}

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

normalizar_membresia <- function(x) {
  as.integer(factor(x, levels = unique(x)))
}

comparar_particiones <- function(a, b) {
  a_norm <- normalizar_membresia(a)
  b_norm <- normalizar_membresia(b)

  all(a_norm == b_norm)
}

ejecutar_original <- function(nombre) {
  cat("\n------------------------------------\n")
  cat("Red:", nombre, "\n")
  cat("------------------------------------\n")

  g_original <- cargar_red(nombre)

  nodos_originales <- vcount(g_original)
  aristas_originales <- ecount(g_original)
  dirigida_original <- is_directed(g_original)
  componentes_originales <- components(as.undirected(g_original))$no

  g <- preparar_red(g_original)

  set.seed(123)

  res <- cluster_infomap(
    g,
    e.weights = E(g)$weight,
    nb.trials = 100
  )

  comunidades_finales <- length(unique(membership(res)))

  cat("Nodos usados:", vcount(g), "\n")
  cat("Aristas usadas:", ecount(g), "\n")
  cat("Comunidades finales original:", comunidades_finales, "\n")
  cat("Codelength original:", res$codelength, "\n")
  cat("Modularity original:", modularity(res), "\n")

  membresia_original <- data.frame(
    node_id = seq_len(vcount(g)),
    membership_original = as.integer(membership(res))
  )

  write.csv(
    membresia_original,
    file.path(out_dir, paste0(nombre, "_membership_original.csv")),
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
    comunidades_original = comunidades_finales,
    codelength_original = res$codelength,
    modularity_original = modularity(res),
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

resultados_original <- do.call(
  rbind,
  lapply(redes, ejecutar_original)
)

write.csv(
  resultados_original,
  file.path(out_dir, "resultados_igraph_original.csv"),
  row.names = FALSE
)

archivo_modificado <- file.path(
  out_dir,
  "diagnostico_supermodulos_modificado.csv"
)

if (!file.exists(archivo_modificado)) {
  stop("No existe diagnostico_supermodulos_modificado.csv. Ejecuta primero el diagnostico del igraph modificado.")
}

resultados_modificado <- read.csv(archivo_modificado)

comparacion <- merge(
  resultados_original,
  resultados_modificado,
  by = "red",
  suffixes = c("_original", "_modificado")
)

comparacion$misma_cantidad_comunidades <- comparacion$comunidades_original == comparacion$comunidades_finales
comparacion$diferencia_codelength <- abs(comparacion$codelength_original - comparacion$codelength)
comparacion$diferencia_modularity <- abs(comparacion$modularity_original - comparacion$modularity)

comparacion$misma_particion <- NA
comparacion$membership_exactamente_igual <- NA

for (i in seq_len(nrow(comparacion))) {
  red <- comparacion$red[i]

  archivo_original <- file.path(
    out_dir,
    paste0(red, "_membership_original.csv")
  )

  archivo_mod <- file.path(
    out_dir,
    paste0(red, "_multilevel_modules_modificado.csv")
  )

  if (!file.exists(archivo_original) || !file.exists(archivo_mod)) {
    next
  }

  memb_original <- read.csv(archivo_original)
  memb_mod <- read.csv(archivo_mod)

  if (!"final_module" %in% names(memb_mod)) {
    next
  }

  a <- memb_original$membership_original
  b <- memb_mod$final_module

  comparacion$membership_exactamente_igual[i] <- all(a == b)
  comparacion$misma_particion[i] <- comparar_particiones(a, b)
}

comparacion_resumida <- comparacion[, c(
  "red",
  "nodos_usados_original",
  "aristas_usadas_original",
  "comunidades_original",
  "comunidades_finales",
  "misma_cantidad_comunidades",
  "membership_exactamente_igual",
  "misma_particion",
  "codelength_original",
  "codelength",
  "diferencia_codelength",
  "modularity_original",
  "modularity",
  "diferencia_modularity",
  "num_levels",
  "num_top_modules",
  "max_tree_depth",
  "final_module_ok",
  "hierarchical_codelength_presente"
)]

cat("\n====================================\n")
cat("COMPARACION RESUMIDA\n")
cat("====================================\n\n")

print(comparacion_resumida)

write.csv(
  comparacion_resumida,
  file.path(out_dir, "comparacion_original_modificado.csv"),
  row.names = FALSE
)

capture.output(
  {
    cat("====================================\n")
    cat("COMPARACION IGRAPH ORIGINAL VS MODIFICADO\n")
    cat("====================================\n\n")

    print(comparacion_resumida)

    cat("\nInterpretacion rapida:\n\n")

    for (i in seq_len(nrow(comparacion_resumida))) {
      cat("Red:", comparacion_resumida$red[i], "\n")
      cat("- Misma cantidad de comunidades:", comparacion_resumida$misma_cantidad_comunidades[i], "\n")
      cat("- Misma particion:", comparacion_resumida$misma_particion[i], "\n")
      cat("- Diferencia codelength:", comparacion_resumida$diferencia_codelength[i], "\n")
      cat("- Diferencia modularity:", comparacion_resumida$diferencia_modularity[i], "\n")
      cat("- Campos multinivel: num_levels =", comparacion_resumida$num_levels[i],
          ", num_top_modules =", comparacion_resumida$num_top_modules[i],
          ", max_tree_depth =", comparacion_resumida$max_tree_depth[i], "\n\n")
    }
  },
  file = file.path(out_dir, "comparacion_original_modificado.txt")
)

cat("\nArchivos generados:\n")
cat(file.path(out_dir, "resultados_igraph_original.csv"), "\n")
cat(file.path(out_dir, "comparacion_original_modificado.csv"), "\n")
cat(file.path(out_dir, "comparacion_original_modificado.txt"), "\n")

cat("\nFIN COMPARACION\n")