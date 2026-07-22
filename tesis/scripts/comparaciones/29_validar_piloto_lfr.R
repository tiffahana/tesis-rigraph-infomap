# Auditoria estructural del piloto LFR jerarquico
#
# Verifica las 27 redes antes de ejecutar Infomap:
#   - archivos obligatorios;
#   - numero de nodos y aristas;
#   - conectividad, lazos y aristas multiples;
#   - cobertura de las particiones verdaderas;
#   - anidamiento micro -> macro;
#   - tamanos observados de micro y macrocomunidades;
#   - proporciones reales de enlaces mu1, mu2 e internos a micro.
#
# Ejecutar desde la raiz del repositorio:
#   Rscript tesis/scripts/comparaciones/29_validar_piloto_lfr.R

cat("============================================================\n")
cat("AUDITORIA DEL PILOTO LFR JERARQUICO\n")
cat("============================================================\n\n")

suppressPackageStartupMessages(library(igraph))

repo_dir <- normalizePath(".", winslash = "/", mustWork = TRUE)

if (!file.exists(file.path(repo_dir, "DESCRIPTION")) ||
    !dir.exists(file.path(repo_dir, "tesis"))) {
  stop("Ejecute este script desde la raiz del repositorio.")
}

pilot_dir <- file.path(
  repo_dir,
  "tesis",
  "experimentos",
  "barrido_lfr",
  "piloto"
)

manifest_path <- file.path(pilot_dir, "manifesto_piloto.csv")

if (!file.exists(manifest_path)) {
  stop("No se encontro: ", manifest_path)
}

manifest <- read.csv(
  manifest_path,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

if (!all(manifest$estado == "OK")) {
  stop("El manifiesto contiene redes con estado distinto de OK.")
}

parse_metadata <- function(path) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  parts <- strsplit(lines, "=", fixed = TRUE)

  keys <- vapply(parts, `[`, character(1), 1)
  values <- vapply(parts, function(x) {
    if (length(x) < 2L) "" else paste(x[-1], collapse = "=")
  }, character(1))

  stats::setNames(as.list(values), keys)
}

read_membership <- function(path, expected_name) {
  x <- read.table(
    path,
    header = FALSE,
    stringsAsFactors = FALSE,
    comment.char = "",
    col.names = c("node_id", expected_name)
  )

  x$node_id <- as.integer(x$node_id)
  x[[expected_name]] <- as.integer(x[[expected_name]])
  x
}

safe_num <- function(x) {
  as.numeric(x)
}

audit_case <- function(row, index, total) {
  case_dir <- file.path(repo_dir, row$directorio)

  cat(
    sprintf(
      "[%d/%d] %s, semilla %s\n",
      index,
      total,
      row$configuracion,
      row$semilla_inicial
    )
  )

  required <- c(
    "network.dat",
    "community_first_level.dat",
    "community_second_level.dat",
    "metadata.txt",
    "flags.dat",
    "time_seed.dat",
    "generacion.log"
  )

  missing <- required[
    !file.exists(file.path(case_dir, required))
  ]

  if (length(missing) > 0L) {
    return(data.frame(
      configuracion = row$configuracion,
      mu1_objetivo = safe_num(row$mu1),
      mu2_objetivo = safe_num(row$mu2),
      semilla_inicial = as.integer(row$semilla_inicial),
      estado_auditoria = "ERROR_ARCHIVOS",
      detalle_error = paste(missing, collapse = ", "),
      stringsAsFactors = FALSE
    ))
  }

  metadata <- parse_metadata(file.path(case_dir, "metadata.txt"))

  expected_n <- as.integer(metadata$N)
  micro_min_target <- as.integer(metadata$minc)
  micro_max_target <- as.integer(metadata$maxc)
  macro_min_target <- as.integer(metadata$minC)
  macro_max_target <- as.integer(metadata$maxC)

  edges_raw <- read.table(
    file.path(case_dir, "network.dat"),
    header = FALSE,
    stringsAsFactors = FALSE,
    comment.char = "",
    col.names = c("from", "to")
  )

  edges_raw$from <- as.integer(edges_raw$from)
  edges_raw$to <- as.integer(edges_raw$to)

  micro <- read_membership(
    file.path(case_dir, "community_first_level.dat"),
    "micro"
  )

  macro <- read_membership(
    file.path(case_dir, "community_second_level.dat"),
    "macro"
  )

  truth <- merge(
    micro,
    macro,
    by = "node_id",
    all = TRUE,
    sort = TRUE
  )

  coverage_ok <- (
    nrow(truth) == expected_n &&
      !anyDuplicated(truth$node_id) &&
      identical(truth$node_id, seq_len(expected_n)) &&
      !anyNA(truth$micro) &&
      !anyNA(truth$macro)
  )

  node_range_ok <- (
    all(edges_raw$from >= 1L & edges_raw$from <= expected_n) &&
      all(edges_raw$to >= 1L & edges_raw$to <= expected_n)
  )

  loops_raw <- sum(edges_raw$from == edges_raw$to)

  # El benchmark escribe cada arista en ambos sentidos.
  lo <- pmin(edges_raw$from, edges_raw$to)
  hi <- pmax(edges_raw$from, edges_raw$to)

  undirected_key <- paste(lo, hi, sep = "-")
  duplicate_rows_raw <- nrow(edges_raw) - length(unique(
    paste(edges_raw$from, edges_raw$to, sep = "->")
  ))

  edges_unique <- unique(data.frame(
    from = lo,
    to = hi
  ))

  edges_unique <- edges_unique[
    edges_unique$from != edges_unique$to,
    ,
    drop = FALSE
  ]

  g <- graph_from_data_frame(
    edges_unique,
    directed = FALSE,
    vertices = data.frame(name = seq_len(expected_n))
  )

  connected <- is_connected(g)
  degree_values <- degree(g)

  micro_sizes <- table(truth$micro)
  macro_sizes <- table(truth$macro)

  micro_to_macro <- split(truth$macro, truth$micro)
  nesting_ok <- all(vapply(
    micro_to_macro,
    function(x) length(unique(x)) == 1L,
    logical(1)
  ))

  edge_truth <- merge(
    edges_unique,
    truth,
    by.x = "from",
    by.y = "node_id",
    all.x = TRUE,
    sort = FALSE
  )

  names(edge_truth)[
    names(edge_truth) %in% c("micro", "macro")
  ] <- c("micro_from", "macro_from")

  edge_truth <- merge(
    edge_truth,
    truth,
    by.x = "to",
    by.y = "node_id",
    all.x = TRUE,
    sort = FALSE
  )

  names(edge_truth)[
    names(edge_truth) %in% c("micro", "macro")
  ] <- c("micro_to", "macro_to")

  category <- ifelse(
    edge_truth$macro_from != edge_truth$macro_to,
    "different_macro",
    ifelse(
      edge_truth$micro_from != edge_truth$micro_to,
      "same_macro_different_micro",
      "same_micro"
    )
  )

  category_counts <- table(factor(
    category,
    levels = c(
      "different_macro",
      "same_macro_different_micro",
      "same_micro"
    )
  ))

  category_props <- category_counts / sum(category_counts)

  # Proporciones medias por nodo, coherentes con la definicion por stubs.
  neighbors_from <- data.frame(
    node_id = edge_truth$from,
    category = category
  )

  neighbors_to <- data.frame(
    node_id = edge_truth$to,
    category = category
  )

  stubs <- rbind(neighbors_from, neighbors_to)
  stub_counts <- as.data.frame(table(
    factor(stubs$node_id, levels = seq_len(expected_n)),
    factor(
      stubs$category,
      levels = c(
        "different_macro",
        "same_macro_different_micro",
        "same_micro"
      )
    )
  ))

  names(stub_counts) <- c("node_id", "category", "count")
  stub_counts$node_id <- as.integer(as.character(stub_counts$node_id))

  wide <- reshape(
    stub_counts,
    idvar = "node_id",
    timevar = "category",
    direction = "wide"
  )

  names(wide) <- sub("^count\\.", "", names(wide))
  wide$degree <- (
    wide$different_macro +
      wide$same_macro_different_micro +
      wide$same_micro
  )

  nonzero <- wide$degree > 0L

  mean_mu1_node <- mean(
    wide$different_macro[nonzero] / wide$degree[nonzero]
  )

  mean_mu2_node <- mean(
    wide$same_macro_different_micro[nonzero] /
      wide$degree[nonzero]
  )

  mean_same_micro_node <- mean(
    wide$same_micro[nonzero] / wide$degree[nonzero]
  )

  checks <- c(
    coverage_ok,
    node_range_ok,
    connected,
    nesting_ok,
    loops_raw == 0L
  )

  data.frame(
    configuracion = row$configuracion,
    mu1_objetivo = safe_num(row$mu1),
    mu2_objetivo = safe_num(row$mu2),
    misma_micro_objetivo = safe_num(row$fraccion_misma_micro),
    semilla_inicial = as.integer(row$semilla_inicial),
    estado_auditoria = if (all(checks)) "OK" else "REVISAR",
    detalle_error = "",
    nodos_esperados = expected_n,
    nodos_grafo = vcount(g),
    aristas_filas_archivo = nrow(edges_raw),
    aristas_unicas = ecount(g),
    grado_medio_real = mean(degree_values),
    grado_maximo_real = max(degree_values),
    conectado = connected,
    componentes = components(g)$no,
    lazos_archivo = loops_raw,
    filas_duplicadas_direccion = duplicate_rows_raw,
    cobertura_particiones_ok = coverage_ok,
    anidamiento_micro_macro_ok = nesting_ok,
    microcomunidades = length(micro_sizes),
    micro_tamano_min = min(micro_sizes),
    micro_tamano_max = max(micro_sizes),
    macrocomunidades = length(macro_sizes),
    macro_tamano_min = min(macro_sizes),
    macro_tamano_max = max(macro_sizes),
    mu1_global_real = unname(category_props["different_macro"]),
    mu2_global_real = unname(
      category_props["same_macro_different_micro"]
    ),
    misma_micro_global_real = unname(category_props["same_micro"]),
    mu1_media_por_nodo = mean_mu1_node,
    mu2_media_por_nodo = mean_mu2_node,
    misma_micro_media_por_nodo = mean_same_micro_node,
    stringsAsFactors = FALSE
  )
}

results <- lapply(
  seq_len(nrow(manifest)),
  function(i) audit_case(manifest[i, , drop = FALSE], i, nrow(manifest))
)

audit <- do.call(rbind, results)

audit_path <- file.path(pilot_dir, "auditoria_piloto.csv")

write.csv(
  audit,
  audit_path,
  row.names = FALSE
)

valid_rows <- audit$estado_auditoria == "OK"

summary_by_config <- aggregate(
  cbind(
    mu1_global_real,
    mu2_global_real,
    misma_micro_global_real,
    mu1_media_por_nodo,
    mu2_media_por_nodo,
    misma_micro_media_por_nodo,
    grado_medio_real,
    microcomunidades,
    macrocomunidades
  ) ~ configuracion + mu1_objetivo + mu2_objetivo,
  data = audit[valid_rows, , drop = FALSE],
  FUN = function(x) {
    c(
      media = mean(x),
      min = min(x),
      max = max(x)
    )
  }
)

summary_path <- file.path(pilot_dir, "resumen_auditoria.txt")

capture.output(
  {
    cat("============================================================\n")
    cat("AUDITORIA DEL PILOTO LFR JERARQUICO\n")
    cat("============================================================\n\n")

    cat("Redes revisadas:", nrow(audit), "\n")
    cat(
      "Estado OK:",
      sum(audit$estado_auditoria == "OK"),
      "\n"
    )
    cat(
      "Estado REVISAR/ERROR:",
      sum(audit$estado_auditoria != "OK"),
      "\n\n"
    )

    cat("ESTADOS\n\n")
    print(table(audit$estado_auditoria))

    cat("\nRESUMEN POR CONFIGURACION\n\n")
    print(summary_by_config)

    if (any(audit$estado_auditoria != "OK")) {
      cat("\nCASOS QUE REQUIEREN REVISION\n\n")
      print(audit[
        audit$estado_auditoria != "OK",
        c(
          "configuracion",
          "semilla_inicial",
          "estado_auditoria",
          "detalle_error",
          "conectado",
          "cobertura_particiones_ok",
          "anidamiento_micro_macro_ok",
          "micro_tamano_min",
          "micro_tamano_max",
          "macro_tamano_min",
          "macro_tamano_max"
        ),
        drop = FALSE
      ])
    }
  },
  file = summary_path
)

cat("\n============================================================\n")
cat("AUDITORIA TERMINADA\n")
cat("============================================================\n")
cat("Detalle:", audit_path, "\n")
cat("Resumen:", summary_path, "\n")
