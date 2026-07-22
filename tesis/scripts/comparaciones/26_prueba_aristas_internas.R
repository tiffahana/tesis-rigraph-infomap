# =============================================================================
# 26_prueba_aristas_internas.R
#
# Objetivo: evaluar si conservar las aristas internas (bucles) al contraer los
# modulos mejora la concordancia del NIVEL SUPERIOR con Infomap oficial.
#
# El prototipo actual, en build_module_graph_local(), descarta las aristas
# intramodulo mediante:
#     keep <- module_edges[, 1] != module_edges[, 2]
#
# Este script reimplementa la construccion jerarquica en R con un interruptor
# para conservar o descartar esas aristas, partiendo de la MISMA particion base
# en ambos casos, de modo que la unica diferencia sea la agregacion.
#
# Salida: consola + archivo de texto + CSV.
# =============================================================================

library(igraph)

# --- configuracion -----------------------------------------------------------

semilla <- 123L
ensayos <- 500L
redes   <- c("Zachary", "USairports", "yeast")

base_dir <- getwd()
out_dir  <- file.path(base_dir, "tesis", "resultados", "comparaciones",
                      "prueba_aristas_internas")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

archivo_txt <- file.path(out_dir, "resumen_aristas_internas.txt")
archivo_csv <- file.path(out_dir, "resumen_aristas_internas.csv")

linea <- function(ch = "-", n = 62) cat(strrep(ch, n), "\n")

# --- utilidades --------------------------------------------------------------

compactar <- function(x) as.integer(factor(x, levels = unique(x)))

misma_particion <- function(a, b) {
  if (length(a) != length(b)) return(FALSE)
  all(compactar(a) == compactar(b))
}

cargar_red <- function(nombre) {
  if (nombre == "Zachary") return(make_graph("Zachary"))
  env <- new.env()
  data(list = nombre, package = "igraphdata", envir = env)
  get(nombre, envir = env)
}

tomar_componente_gigante <- function(g) {
  comp <- components(as_undirected(g))
  induced_subgraph(g, which(comp$membership == which.max(comp$csize)))
}

# Misma preparacion usada en los scripts previos, para que las redes coincidan.
preparar_red <- function(g) {
  g <- upgrade_graph(g)
  if (!"weight" %in% edge_attr_names(g)) E(g)$weight <- 1
  if (is_directed(g)) {
    g <- as_undirected(g, mode = "collapse",
                       edge.attr.comb = list(weight = "sum", "ignore"))
  }
  g <- simplify(g, remove.multiple = TRUE, remove.loops = TRUE,
                edge.attr.comb = list(weight = "sum", "ignore"))
  if (components(g)$no > 1) g <- tomar_componente_gigante(g)
  g
}

# --- construccion del grafo de modulos ---------------------------------------
# conservar_internas = FALSE  -> replica el comportamiento actual del prototipo
# conservar_internas = TRUE   -> mantiene el flujo interno como bucles

construir_grafo_modulos <- function(g, grupos, pesos, conservar_internas) {
  extremos <- as_edgelist(g, names = FALSE)
  if (is.null(pesos)) pesos <- rep(1, nrow(extremos))

  aristas_mod <- cbind(grupos[extremos[, 1]], grupos[extremos[, 2]])

  if (!conservar_internas) {
    keep <- aristas_mod[, 1] != aristas_mod[, 2]
    aristas_mod <- aristas_mod[keep, , drop = FALSE]
    pesos <- pesos[keep]
  }

  n_modulos <- max(grupos)
  mg <- make_empty_graph(n = n_modulos, directed = FALSE)
  if (nrow(aristas_mod) == 0) return(mg)

  mg <- add_edges(mg, as.vector(t(aristas_mod)))
  E(mg)$weight <- pesos

  simplify(mg,
           remove.multiple = TRUE,
           remove.loops    = !conservar_internas,
           edge.attr.comb  = list(weight = "sum", "ignore"))
}

# --- construccion jerarquica de abajo hacia arriba ---------------------------
# Recibe la particion base ya calculada, para aislar el efecto de la agregacion.

construir_jerarquia <- function(g, particion_base, pesos, conservar_internas,
                                max_niveles = 10L) {
  actual  <- compactar(particion_base)
  niveles <- list(actual)

  for (paso in seq_len(max_niveles - 1L)) {
    mg <- tryCatch(
      construir_grafo_modulos(g, actual, pesos, conservar_internas),
      error = function(e) NULL
    )
    if (is.null(mg) || vcount(mg) <= 1 || ecount(mg) == 0) break

    padre_res <- tryCatch(
      igraph::cluster_infomap(mg,
                              e.weights = edge_attr(mg, "weight"),
                              nb.trials = ensayos),
      error = function(e) NULL
    )
    if (is.null(padre_res)) break

    padre_de_modulo <- compactar(as.integer(membership(padre_res)))
    nivel_padre     <- compactar(padre_de_modulo[actual])

    if (misma_particion(nivel_padre, actual)) break

    niveles[[length(niveles) + 1L]] <- nivel_padre
    actual <- nivel_padre

    if (length(unique(actual)) == 1L) break
  }

  niveles   # de abajo hacia arriba: [[1]] = base, ultimo = mas grueso
}

# Nivel superior no trivial: el mas grueso con mas de un modulo.
nivel_superior <- function(niveles) {
  for (i in rev(seq_along(niveles))) {
    if (length(unique(niveles[[i]])) > 1L) return(niveles[[i]])
  }
  niveles[[1]]
}

# --- extraccion de la salida oficial -----------------------------------------

extraer_oficial <- function(resultado) {
  top <- as.data.frame(resultado$model, states = FALSE,
                       depth_level = 1L, tibble = FALSE)
  fin <- as.data.frame(resultado$model, states = FALSE,
                       depth_level = -1L, tibble = FALSE)

  top <- top[!duplicated(top$node_id), c("node_id", "module_id")]
  fin <- fin[!duplicated(fin$node_id), c("node_id", "module_id")]

  top <- top[order(top$node_id), ]
  fin <- fin[order(fin$node_id), ]

  list(top = top$module_id, final = fin$module_id)
}

nmi <- function(a, b) igraph::compare(a, b, method = "nmi")
ari <- function(a, b) igraph::compare(a, b, method = "adjusted.rand")

# --- ejecucion ---------------------------------------------------------------

con <- file(archivo_txt, open = "wt", encoding = "UTF-8")
sink(con, split = TRUE)

cat("PRUEBA: EFECTO DE CONSERVAR LAS ARISTAS INTERNAS\n")
linea("=")
cat("Fecha:   ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("igraph:  ", as.character(packageVersion("igraph")), "\n")
cat("infomap: ", as.character(packageVersion("infomap")), "\n")
cat("Semilla: ", semilla, "   Ensayos:", ensayos, "\n\n")

filas <- list()

for (red in redes) {

  cat("\n"); linea("="); cat("RED:", red, "\n"); linea("=")

  g <- preparar_red(cargar_red(red))
  pesos <- edge_attr(g, "weight")

  cat("Nodos:", vcount(g), "  Aristas:", ecount(g), "\n\n")

  # --- particion base compartida por ambas variantes -------------------------
  set.seed(semilla)
  base_res <- igraph::cluster_infomap(g, e.weights = pesos, nb.trials = ensayos)
  particion_base <- as.integer(membership(base_res))

  cat("Particion base (Infomap de dos niveles):",
      length(unique(particion_base)), "comunidades\n\n")

  # --- verificacion: el paquete instalado reproduce la variante actual -------
  nivel_paquete <- NA
  mm <- tryCatch(base_res$multilevel_modules, error = function(e) NULL)
  if (!is.null(mm)) {
    mm <- as.data.frame(mm)
    cols_nivel <- grep("^level_", names(mm), value = TRUE)
    if (length(cols_nivel) >= 2) {
      # nivel superior no trivial segun el paquete
      for (cc in cols_nivel) {
        if (length(unique(mm[[cc]])) > 1L) { nivel_paquete <- mm[[cc]]; break }
      }
    }
  }

  # --- dos variantes ---------------------------------------------------------
  jer_sin <- construir_jerarquia(g, particion_base, pesos,
                                 conservar_internas = FALSE)
  jer_con <- construir_jerarquia(g, particion_base, pesos,
                                 conservar_internas = TRUE)

  top_sin <- nivel_superior(jer_sin)
  top_con <- nivel_superior(jer_con)

  cat("Variante SIN aristas internas (comportamiento actual):\n")
  cat("  niveles construidos:", length(jer_sin),
      " | modulos en nivel superior:", length(unique(top_sin)), "\n")

  cat("Variante CON aristas internas (propuesta):\n")
  cat("  niveles construidos:", length(jer_con),
      " | modulos en nivel superior:", length(unique(top_con)), "\n\n")

  if (!all(is.na(nivel_paquete)) && length(nivel_paquete) == length(top_sin)) {
    cat("Control: la reimplementacion SIN internas reproduce al paquete:",
        misma_particion(nivel_paquete, top_sin), "\n\n")
  }

  # --- oficial ---------------------------------------------------------------
  of <- tryCatch({
    r <- infomap::cluster_infomap(g, weight = "weight", nb.trials = ensayos,
                                  seed = semilla, regularized = FALSE,
                                  silent = TRUE, tibble = FALSE)
    extraer_oficial(r)
  }, error = function(e) { cat("Error en Infomap oficial:", conditionMessage(e), "\n"); NULL })

  if (is.null(of)) next

  cat("Infomap oficial: superior =", length(unique(of$top)),
      " | final =", length(unique(of$final)), "\n\n")

  linea()
  cat("CONCORDANCIA DEL NIVEL SUPERIOR CONTRA INFOMAP OFICIAL\n")
  linea()
  cat(sprintf("%-28s %8s %8s %10s\n", "variante", "NMI", "ARI", "modulos"))
  cat(sprintf("%-28s %8.4f %8.4f %10d\n", "SIN aristas internas",
              nmi(top_sin, of$top), ari(top_sin, of$top), length(unique(top_sin))))
  cat(sprintf("%-28s %8.4f %8.4f %10d\n", "CON aristas internas",
              nmi(top_con, of$top), ari(top_con, of$top), length(unique(top_con))))
  cat(sprintf("%-28s %8s %8s %10d\n", "(oficial)", "-", "-", length(unique(of$top))))

  cat("\nNivel final contra oficial (no depende de la agregacion):\n")
  cat(sprintf("  NMI = %.4f   ARI = %.4f\n",
              nmi(particion_base, of$final), ari(particion_base, of$final)))

  filas[[red]] <- data.frame(
    red                = red,
    nodos              = vcount(g),
    aristas            = ecount(g),
    semilla            = semilla,
    ensayos            = ensayos,
    modulos_top_sin    = length(unique(top_sin)),
    modulos_top_con    = length(unique(top_con)),
    modulos_top_ofic   = length(unique(of$top)),
    nmi_top_sin        = nmi(top_sin, of$top),
    ari_top_sin        = ari(top_sin, of$top),
    nmi_top_con        = nmi(top_con, of$top),
    ari_top_con        = ari(top_con, of$top),
    nmi_final          = nmi(particion_base, of$final),
    ari_final          = ari(particion_base, of$final),
    stringsAsFactors   = FALSE
  )
}

resumen <- do.call(rbind, filas)

cat("\n\n"); linea("="); cat("RESUMEN\n"); linea("=")
print(resumen, row.names = FALSE)

cat("\nLectura:\n")
cat("  Si nmi_top_con > nmi_top_sin, conservar las aristas internas mejora\n")
cat("  la concordancia del nivel superior y conviene reportarlo.\n")
cat("  Si son similares, la diferencia se explica por la optimizacion conjunta\n")
cat("  de la ecuacion del mapa jerarquica en Infomap oficial.\n")

sink(); close(con)

write.csv(resumen, archivo_csv, row.names = FALSE)

cat("\nArchivos generados:\n")
cat(" ", archivo_txt, "\n")
cat(" ", archivo_csv, "\n")
