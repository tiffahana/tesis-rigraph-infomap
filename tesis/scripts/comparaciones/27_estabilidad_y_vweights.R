# =============================================================================
# 27_estabilidad_y_vweights.R
#
# Dos pruebas en una sola ejecucion:
#
# PARTE A - ESTABILIDAD
#   Compara Infomap oficial CONTRA SI MISMA usando varias semillas, y contrasta
#   esa variabilidad con la distancia entre el prototipo y la oficial.
#   Si la oficial difiere de si misma tanto como difiere del prototipo, la
#   discrepancia del nivel superior no requiere mas explicacion.
#
# PARTE B - FLUJO DE LOS MODULOS (v.weights)
#   Al contraer los modulos, el prototipo pierde el flujo interno. Los bucles
#   no sirven porque cluster_infomap() probablemente los ignora. Aqui se prueba
#   la via correcta: pasar v.weights con el flujo agregado de cada modulo.
#
# Salida: consola + archivos de texto y CSV.
# =============================================================================

library(igraph)

# --- configuracion (ajustable) -----------------------------------------------

semillas <- c(11L, 23L, 42L, 77L, 123L)   # reduce esta lista si demora mucho
ensayos  <- 500L
redes    <- c("Zachary", "USairports", "yeast")   # yeast al final: es la lenta

out_dir <- file.path(getwd(), "tesis", "resultados", "comparaciones",
                     "estabilidad_y_vweights")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

archivo_txt <- file.path(out_dir, "resumen_estabilidad_vweights.txt")

linea <- function(ch = "-", n = 66) cat(strrep(ch, n), "\n")
avanzar <- function(...) { cat(..., "\n"); flush.console() }

# --- utilidades --------------------------------------------------------------

compactar <- function(x) as.integer(factor(x, levels = unique(x)))
nmi <- function(a, b) igraph::compare(a, b, method = "nmi")
ari <- function(a, b) igraph::compare(a, b, method = "adjusted.rand")

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

preparar_red <- function(g) {
  g <- upgrade_graph(g)
  if (!"weight" %in% edge_attr_names(g)) E(g)$weight <- 1
  if (is_directed(g)) {
    g <- as_undirected(g, mode = "collapse",
                       edge.attr.comb = list(weight = "sum", "ignore"))
  }
  g <- simplify(g, remove.multiple = TRUE, remove.loops = TRUE,
                edge.attr.comb = list(weight = "sum", "ignore"))
  comp <- components(g)
  if (comp$no > 1) {
    g <- induced_subgraph(g, which(comp$membership == which.max(comp$csize)))
  }
  g
}

# --- extraccion --------------------------------------------------------------

extraer_oficial <- function(res) {
  top <- as.data.frame(res$model, states = FALSE, depth_level = 1L, tibble = FALSE)
  fin <- as.data.frame(res$model, states = FALSE, depth_level = -1L, tibble = FALSE)
  top <- top[!duplicated(top$node_id), c("node_id", "module_id")]
  fin <- fin[!duplicated(fin$node_id), c("node_id", "module_id")]
  list(top   = top[order(top$node_id), "module_id"],
       final = fin[order(fin$node_id), "module_id"])
}

extraer_modificado <- function(res) {
  mm <- tryCatch(as.data.frame(res$multilevel_modules), error = function(e) NULL)
  if (is.null(mm)) return(NULL)
  cols <- grep("^level_", names(mm), value = TRUE)
  top <- NULL
  for (cc in cols) {
    if (length(unique(mm[[cc]])) > 1L) { top <- mm[[cc]]; break }
  }
  if (is.null(top)) top <- mm[[cols[length(cols)]]]
  list(top = top, final = mm$final_module)
}

correr_oficial <- function(g, semilla) {
  infomap::cluster_infomap(g, weight = "weight", nb.trials = ensayos,
                           seed = semilla, regularized = FALSE,
                           silent = TRUE, tibble = FALSE)
}

# --- construccion del grafo de modulos, con y sin v.weights ------------------

construir_grafo_modulos <- function(g, grupos, pesos) {
  extremos <- as_edgelist(g, names = FALSE)
  aristas_mod <- cbind(grupos[extremos[, 1]], grupos[extremos[, 2]])
  keep <- aristas_mod[, 1] != aristas_mod[, 2]
  aristas_mod <- aristas_mod[keep, , drop = FALSE]
  w <- pesos[keep]

  n <- max(grupos)
  mg <- make_empty_graph(n = n, directed = FALSE)
  if (nrow(aristas_mod) == 0) return(NULL)
  mg <- add_edges(mg, as.vector(t(aristas_mod)))
  E(mg)$weight <- w
  simplify(mg, remove.multiple = TRUE, remove.loops = TRUE,
           edge.attr.comb = list(weight = "sum", "ignore"))
}

# flujo agregado por modulo = suma de la fuerza (grado ponderado) de sus nodos
pesos_de_modulos <- function(g, grupos, pesos) {
  fuerza <- strength(g, weights = pesos)
  f <- factor(grupos, levels = seq_len(max(grupos)))
  v <- as.numeric(tapply(fuerza, f, sum))
  v[is.na(v)] <- 0
  v
}

construir_jerarquia <- function(g, particion_base, pesos, usar_vweights,
                                max_niveles = 10L) {
  actual  <- compactar(particion_base)
  niveles <- list(actual)

  for (paso in seq_len(max_niveles - 1L)) {
    mg <- construir_grafo_modulos(g, actual, pesos)
    if (is.null(mg) || vcount(mg) <= 1 || ecount(mg) == 0) break

    vw <- if (usar_vweights) pesos_de_modulos(g, actual, pesos) else NULL

    padre <- tryCatch(
      igraph::cluster_infomap(mg,
                              e.weights = edge_attr(mg, "weight"),
                              v.weights = vw,
                              nb.trials = ensayos),
      error = function(e) NULL
    )
    if (is.null(padre)) break

    padre_mod   <- compactar(as.integer(membership(padre)))
    nivel_padre <- compactar(padre_mod[actual])

    if (misma_particion(nivel_padre, actual)) break
    niveles[[length(niveles) + 1L]] <- nivel_padre
    actual <- nivel_padre
    if (length(unique(actual)) == 1L) break
  }
  niveles
}

nivel_superior <- function(niveles) {
  for (i in rev(seq_along(niveles))) {
    if (length(unique(niveles[[i]])) > 1L) return(niveles[[i]])
  }
  niveles[[1]]
}

# =============================================================================
# EJECUCION
# =============================================================================

con <- file(archivo_txt, open = "wt", encoding = "UTF-8")
sink(con, split = TRUE)

cat("ESTABILIDAD ENTRE SEMILLAS Y PRUEBA DE v.weights\n")
linea("=")
cat("Fecha:   ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("igraph:  ", as.character(packageVersion("igraph")), "\n")
cat("infomap: ", as.character(packageVersion("infomap")), "\n")
cat("Semillas:", paste(semillas, collapse = ", "), "  Ensayos:", ensayos, "\n\n")

filas_estab <- list()
filas_vw    <- list()

for (red in redes) {

  cat("\n"); linea("="); cat("RED:", red, "\n"); linea("=")
  avanzar("   preparando red...")

  g <- preparar_red(cargar_red(red))
  pesos <- edge_attr(g, "weight")
  cat("Nodos:", vcount(g), " Aristas:", ecount(g), "\n\n")

  # ---------------------------------------------------------------- PARTE A --
  ofic <- list(); modif <- list()

  for (s in semillas) {
    avanzar("   semilla", s, ": oficial...")
    ofic[[as.character(s)]] <- tryCatch(extraer_oficial(correr_oficial(g, s)),
                                        error = function(e) NULL)
    avanzar("   semilla", s, ": prototipo...")
    set.seed(s)
    rm_ <- tryCatch(igraph::cluster_infomap(g, e.weights = pesos, nb.trials = ensayos),
                    error = function(e) NULL)
    modif[[as.character(s)]] <- if (is.null(rm_)) NULL else extraer_modificado(rm_)
  }

  ok <- names(ofic)[!vapply(ofic, is.null, logical(1)) &
                    !vapply(modif, is.null, logical(1))]

  if (length(ok) >= 2) {

    # oficial contra si misma (todos los pares)
    pares <- combn(ok, 2, simplify = FALSE)
    oo_top <- vapply(pares, function(p) nmi(ofic[[p[1]]]$top,   ofic[[p[2]]]$top),   numeric(1))
    oo_fin <- vapply(pares, function(p) nmi(ofic[[p[1]]]$final, ofic[[p[2]]]$final), numeric(1))

    # prototipo contra oficial (misma semilla)
    mo_top <- vapply(ok, function(s) nmi(modif[[s]]$top,   ofic[[s]]$top),   numeric(1))
    mo_fin <- vapply(ok, function(s) nmi(modif[[s]]$final, ofic[[s]]$final), numeric(1))

    n_ofic  <- vapply(ok, function(s) length(unique(ofic[[s]]$top)),  integer(1))
    n_modif <- vapply(ok, function(s) length(unique(modif[[s]]$top)), integer(1))

    linea()
    cat("PARTE A - ESTABILIDAD (NMI)\n")
    linea()
    cat(sprintf("%-34s %7s %7s %7s\n", "comparacion", "min", "mediana", "max"))
    cat(sprintf("%-34s %7.4f %7.4f %7.4f\n", "oficial vs oficial  (superior)",
                min(oo_top), median(oo_top), max(oo_top)))
    cat(sprintf("%-34s %7.4f %7.4f %7.4f\n", "prototipo vs oficial (superior)",
                min(mo_top), median(mo_top), max(mo_top)))
    cat(sprintf("%-34s %7.4f %7.4f %7.4f\n", "oficial vs oficial  (final)",
                min(oo_fin), median(oo_fin), max(oo_fin)))
    cat(sprintf("%-34s %7.4f %7.4f %7.4f\n", "prototipo vs oficial (final)",
                min(mo_fin), median(mo_fin), max(mo_fin)))

    cat("\nModulos superiores por semilla:\n")
    cat("  oficial:  ", paste(n_ofic,  collapse = ", "), "\n")
    cat("  prototipo:", paste(n_modif, collapse = ", "), "\n")

    cat("\nLectura: si el rango de 'oficial vs oficial' cubre el valor de\n")
    cat("'prototipo vs oficial', la diferencia esta dentro de la variabilidad\n")
    cat("propia del algoritmo y no requiere explicacion adicional.\n\n")

    filas_estab[[red]] <- data.frame(
      red = red, semillas = length(ok), ensayos = ensayos,
      oo_top_min = min(oo_top), oo_top_med = median(oo_top), oo_top_max = max(oo_top),
      mo_top_min = min(mo_top), mo_top_med = median(mo_top), mo_top_max = max(mo_top),
      oo_fin_med = median(oo_fin), mo_fin_med = median(mo_fin),
      stringsAsFactors = FALSE
    )
  } else {
    cat("No hubo suficientes ejecuciones validas para la Parte A.\n\n")
  }

  # ---------------------------------------------------------------- PARTE B --
  avanzar("   parte B: v.weights...")

  s_ref <- 123L
  set.seed(s_ref)
  base_res <- tryCatch(igraph::cluster_infomap(g, e.weights = pesos, nb.trials = ensayos),
                       error = function(e) NULL)
  ofic_ref <- ofic[[as.character(s_ref)]]

  if (!is.null(base_res) && !is.null(ofic_ref)) {
    base_part <- as.integer(membership(base_res))

    jer_sin <- construir_jerarquia(g, base_part, pesos, usar_vweights = FALSE)
    jer_con <- construir_jerarquia(g, base_part, pesos, usar_vweights = TRUE)

    top_sin <- nivel_superior(jer_sin)
    top_con <- nivel_superior(jer_con)

    linea()
    cat("PARTE B - EFECTO DE v.weights (semilla", s_ref, ")\n")
    linea()
    cat(sprintf("%-28s %8s %8s %9s\n", "variante", "NMI", "ARI", "modulos"))
    cat(sprintf("%-28s %8.4f %8.4f %9d\n", "sin v.weights (actual)",
                nmi(top_sin, ofic_ref$top), ari(top_sin, ofic_ref$top),
                length(unique(top_sin))))
    cat(sprintf("%-28s %8.4f %8.4f %9d\n", "con v.weights (propuesta)",
                nmi(top_con, ofic_ref$top), ari(top_con, ofic_ref$top),
                length(unique(top_con))))
    cat(sprintf("%-28s %8s %8s %9d\n", "(oficial)", "-", "-",
                length(unique(ofic_ref$top))))
    cat("\n")

    filas_vw[[red]] <- data.frame(
      red = red, semilla = s_ref, ensayos = ensayos,
      nmi_sin = nmi(top_sin, ofic_ref$top), ari_sin = ari(top_sin, ofic_ref$top),
      nmi_con = nmi(top_con, ofic_ref$top), ari_con = ari(top_con, ofic_ref$top),
      modulos_sin = length(unique(top_sin)), modulos_con = length(unique(top_con)),
      modulos_ofic = length(unique(ofic_ref$top)),
      stringsAsFactors = FALSE
    )
  } else {
    cat("No fue posible ejecutar la Parte B en esta red.\n\n")
  }
}

cat("\n\n"); linea("="); cat("RESUMEN PARTE A\n"); linea("=")
if (length(filas_estab)) print(do.call(rbind, filas_estab), row.names = FALSE)

cat("\n"); linea("="); cat("RESUMEN PARTE B\n"); linea("=")
if (length(filas_vw)) print(do.call(rbind, filas_vw), row.names = FALSE)

sink(); close(con)

if (length(filas_estab)) {
  write.csv(do.call(rbind, filas_estab),
            file.path(out_dir, "estabilidad_semillas.csv"), row.names = FALSE)
}
if (length(filas_vw)) {
  write.csv(do.call(rbind, filas_vw),
            file.path(out_dir, "efecto_vweights.csv"), row.names = FALSE)
}

cat("\nListo. Resultados en:\n ", archivo_txt, "\n")
