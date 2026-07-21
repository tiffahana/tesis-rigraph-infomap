cat("====================================\n")
cat("COMPARACION IGRAPH MODIFICADO VS INFOMAP OFICIAL\n")
cat("====================================\n\n")

library(igraph)

if (!requireNamespace("igraphdata", quietly = TRUE)) {
  stop("Falta instalar igraphdata.")
}

if (!requireNamespace("infomap", quietly = TRUE)) {
  cat("Falta instalar infomap. Instalando desde R-universe...\n")
  install.packages(
    "infomap",
    repos = c("https://mapequation.r-universe.dev", "https://cloud.r-project.org")
  )
}

base_dir <- normalizePath(
  ".",
  mustWork = TRUE
)

out_dir <- file.path(base_dir, "tesis", "resultados", "comparaciones")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

cat("igraph cargado desde:\n")
print(find.package("igraph"))

cat("\nVersion igraph:\n")
print(packageVersion("igraph"))

cat("\nVersion infomap oficial:\n")
print(packageVersion("infomap"))

safe <- function(expr) {
  tryCatch(expr, error = function(e) NA)
}

normalizar_membresia <- function(x) {
  as.integer(factor(x, levels = unique(x)))
}

comparar_particiones <- function(a, b) {
  if (length(a) != length(b)) {
    return(FALSE)
  }

  a_norm <- normalizar_membresia(a)
  b_norm <- normalizar_membresia(b)

  all(a_norm == b_norm)
}

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

convertir_paths_a_tabla <- function(paths) {
  if (is.null(paths) || is.atomic(paths)) {
    return(NULL)
  }

  largos <- lengths(paths)

  if (length(largos) == 0) {
    return(NULL)
  }

  max_len <- max(largos)

  tabla <- data.frame(
    node_id = seq_along(paths),
    stringsAsFactors = FALSE
  )

  for (i in seq_len(max_len)) {
    tabla[[paste0("level_", i)]] <- sapply(
      paths,
      function(p) {
        if (length(p) >= i) {
          return(p[[i]])
        } else {
          return(NA)
        }
      }
    )
  }

  return(tabla)
}

ejecutar_infomap_oficial <- function(nombre) {
  cat("\n------------------------------------\n")
  cat("Red:", nombre, "\n")
  cat("------------------------------------\n")

  g_original <- cargar_red(nombre)

  nodos_originales <- vcount(g_original)
  aristas_originales <- ecount(g_original)
  componentes_originales <- components(as_undirected(g_original))$no

  g <- preparar_red(g_original)

  cat("Nodos usados:", vcount(g), "\n")
  cat("Aristas usadas:", ecount(g), "\n")

  set.seed(123)

  res_oficial <- infomap::cluster_infomap(
    g,
    weight = "weight",
    nb.trials = 100,
    silent = TRUE,
    tibble = FALSE
  )

  modelo <- res_oficial$model

  comunidades_oficial <- safe(
    infomap::as_communities(res_oficial, g)
  )

  if (inherits(comunidades_oficial, "communities")) {
    membership_oficial <- as.integer(membership(comunidades_oficial))
  } else {
    membership_oficial <- rep(NA_integer_, vcount(g))
  }

  tabla_membership <- data.frame(
    node_id = seq_len(vcount(g)),
    membership_oficial = membership_oficial
  )

  write.csv(
    tabla_membership,
    file.path(out_dir, paste0(nombre, "_membership_infomap_oficial.csv")),
    row.names = FALSE
  )

  nodes_top <- safe(as.data.frame(modelo, states = FALSE, depth_level = 1, tibble = FALSE))
  nodes_bottom <- safe(as.data.frame(modelo, states = FALSE, depth_level = -1, tibble = FALSE))

  if (is.data.frame(nodes_top)) {
    write.csv(
      nodes_top,
      file.path(out_dir, paste0(nombre, "_nodes_top_infomap_oficial.csv")),
      row.names = FALSE
    )
  }

  if (is.data.frame(nodes_bottom)) {
    write.csv(
      nodes_bottom,
      file.path(out_dir, paste0(nombre, "_nodes_bottom_infomap_oficial.csv")),
      row.names = FALSE
    )
  }

  paths <- safe(modelo$multilevel_modules)
  tabla_paths <- convertir_paths_a_tabla(paths)

  if (!is.null(tabla_paths)) {
    write.csv(
      tabla_paths,
      file.path(out_dir, paste0(nombre, "_multilevel_modules_infomap_oficial.csv")),
      row.names = FALSE
    )
  }

  comunidades_finales_oficial <- length(unique(membership_oficial))

  if (all(is.na(membership_oficial))) {
    comunidades_finales_oficial <- NA
  }

  codelength_oficial <- safe(res_oficial$codelength)
  num_top_modules_oficial <- safe(modelo$num_top_modules)
  num_non_trivial_top_modules_oficial <- safe(modelo$num_non_trivial_top_modules)
  num_levels_oficial <- safe(modelo$num_levels)
  max_tree_depth_oficial <- safe(modelo$max_tree_depth)
  hierarchical_codelength_oficial <- safe(modelo$hierarchical_codelength)
  one_level_codelength_oficial <- safe(modelo$one_level_codelength)
  relative_codelength_savings_oficial <- safe(modelo$relative_codelength_savings)

  cat("Comunidades finales oficial:", comunidades_finales_oficial, "\n")
  cat("Codelength oficial:", codelength_oficial, "\n")
  cat("num_top_modules oficial:", num_top_modules_oficial, "\n")
  cat("num_non_trivial_top_modules oficial:", num_non_trivial_top_modules_oficial, "\n")
  cat("num_levels oficial:", num_levels_oficial, "\n")
  cat("max_tree_depth oficial:", max_tree_depth_oficial, "\n")
  cat("hierarchical_codelength oficial:", hierarchical_codelength_oficial, "\n")

  data.frame(
    red = nombre,
    nodos_originales = nodos_originales,
    aristas_originales = aristas_originales,
    componentes_originales = componentes_originales,
    nodos_usados = vcount(g),
    aristas_usadas = ecount(g),
    comunidades_finales_oficial = comunidades_finales_oficial,
    codelength_oficial = codelength_oficial,
    num_top_modules_oficial = num_top_modules_oficial,
    num_non_trivial_top_modules_oficial = num_non_trivial_top_modules_oficial,
    num_levels_oficial = num_levels_oficial,
    max_tree_depth_oficial = max_tree_depth_oficial,
    hierarchical_codelength_oficial = hierarchical_codelength_oficial,
    one_level_codelength_oficial = one_level_codelength_oficial,
    relative_codelength_savings_oficial = relative_codelength_savings_oficial,
    stringsAsFactors = FALSE
  )
}

redes <- c(
  "Zachary",
  "USairports",
  "yeast"
)

resultados_oficial <- do.call(
  rbind,
  lapply(redes, ejecutar_infomap_oficial)
)

write.csv(
  resultados_oficial,
  file.path(out_dir, "resultados_infomap_oficial.csv"),
  row.names = FALSE
)

archivo_modificado <- file.path(
  out_dir,
  "diagnostico_supermodulos_modificado.csv"
)

if (!file.exists(archivo_modificado)) {
  stop("No existe diagnostico_supermodulos_modificado.csv.")
}

resultados_modificado <- read.csv(archivo_modificado)

comparacion <- merge(
  resultados_oficial,
  resultados_modificado,
  by = "red"
)

comparacion$misma_cantidad_comunidades <- comparacion$comunidades_finales_oficial == comparacion$comunidades_finales
comparacion$diferencia_codelength <- abs(comparacion$codelength_oficial - comparacion$codelength)

comparacion$misma_particion_oficial_vs_modificado <- NA
comparacion$membership_exactamente_igual <- NA

for (i in seq_len(nrow(comparacion))) {
  red <- comparacion$red[i]

  archivo_oficial <- file.path(
    out_dir,
    paste0(red, "_membership_infomap_oficial.csv")
  )

  archivo_modificado_red <- file.path(
    out_dir,
    paste0(red, "_multilevel_modules_modificado.csv")
  )

  if (!file.exists(archivo_oficial) || !file.exists(archivo_modificado_red)) {
    next
  }

  memb_oficial <- read.csv(archivo_oficial)
  memb_mod <- read.csv(archivo_modificado_red)

  if (!"final_module" %in% names(memb_mod)) {
    next
  }

  a <- memb_oficial$membership_oficial
  b <- memb_mod$final_module

  if (length(a) == length(b) && !all(is.na(a))) {
    comparacion$membership_exactamente_igual[i] <- all(a == b)
    comparacion$misma_particion_oficial_vs_modificado[i] <- comparar_particiones(a, b)
  }
}

comparacion_resumida <- comparacion[, c(
  "red",
  "nodos_usados.x",
  "aristas_usadas.x",
  "comunidades_finales",
  "comunidades_finales_oficial",
  "misma_cantidad_comunidades",
  "membership_exactamente_igual",
  "misma_particion_oficial_vs_modificado",
  "codelength",
  "codelength_oficial",
  "diferencia_codelength",
  "num_levels",
  "num_levels_oficial",
  "num_top_modules",
  "num_top_modules_oficial",
  "num_non_trivial_top_modules_oficial",
  "max_tree_depth",
  "max_tree_depth_oficial",
  "hierarchical_codelength_presente",
  "hierarchical_codelength_oficial"
)]

names(comparacion_resumida) <- c(
  "red",
  "nodos_usados",
  "aristas_usadas",
  "comunidades_modificado",
  "comunidades_oficial",
  "misma_cantidad_comunidades",
  "membership_exactamente_igual",
  "misma_particion_oficial_vs_modificado",
  "codelength_modificado",
  "codelength_oficial",
  "diferencia_codelength",
  "num_levels_modificado",
  "num_levels_oficial",
  "num_top_modules_modificado",
  "num_top_modules_oficial",
  "num_non_trivial_top_modules_oficial",
  "max_tree_depth_modificado",
  "max_tree_depth_oficial",
  "hierarchical_codelength_presente_modificado",
  "hierarchical_codelength_oficial"
)

cat("\n====================================\n")
cat("COMPARACION RESUMIDA\n")
cat("====================================\n\n")

print(comparacion_resumida)

write.csv(
  comparacion_resumida,
  file.path(out_dir, "comparacion_modificado_infomap_oficial.csv"),
  row.names = FALSE
)

capture.output(
  {
    cat("====================================\n")
    cat("COMPARACION IGRAPH MODIFICADO VS INFOMAP OFICIAL\n")
    cat("====================================\n\n")

    print(comparacion_resumida)

    cat("\nInterpretacion rapida:\n\n")

    for (i in seq_len(nrow(comparacion_resumida))) {
      cat("Red:", comparacion_resumida$red[i], "\n")
      cat("- Comunidades modificado:", comparacion_resumida$comunidades_modificado[i], "\n")
      cat("- Comunidades oficial:", comparacion_resumida$comunidades_oficial[i], "\n")
      cat("- Misma cantidad comunidades:", comparacion_resumida$misma_cantidad_comunidades[i], "\n")
      cat("- Misma particion:", comparacion_resumida$misma_particion_oficial_vs_modificado[i], "\n")
      cat("- Codelength modificado:", comparacion_resumida$codelength_modificado[i], "\n")
      cat("- Codelength oficial:", comparacion_resumida$codelength_oficial[i], "\n")
      cat("- num_levels modificado:", comparacion_resumida$num_levels_modificado[i], "\n")
      cat("- num_levels oficial:", comparacion_resumida$num_levels_oficial[i], "\n")
      cat("- max_tree_depth modificado:", comparacion_resumida$max_tree_depth_modificado[i], "\n")
      cat("- max_tree_depth oficial:", comparacion_resumida$max_tree_depth_oficial[i], "\n")
      cat("- hierarchical_codelength modificado presente:",
          comparacion_resumida$hierarchical_codelength_presente_modificado[i], "\n")
      cat("- hierarchical_codelength oficial:",
          comparacion_resumida$hierarchical_codelength_oficial[i], "\n\n")
    }
  },
  file = file.path(out_dir, "comparacion_modificado_infomap_oficial.txt")
)

cat("\nArchivos generados:\n")
cat(file.path(out_dir, "resultados_infomap_oficial.csv"), "\n")
cat(file.path(out_dir, "comparacion_modificado_infomap_oficial.csv"), "\n")
cat(file.path(out_dir, "comparacion_modificado_infomap_oficial.txt"), "\n")

cat("\nFIN COMPARACION OFICIAL\n")