# Ejecuta el barrido piloto LFR sobre las 27 redes estructuralmente validas.
#
# Compara:
#   1. igraph modificado contra la verdad LFR.
#   2. Infomap oficial contra la verdad LFR.
#   3. igraph modificado contra Infomap oficial.
#
# Niveles:
#   - superior  -> macrocomunidades verdaderas;
#   - final     -> microcomunidades verdaderas.
#
# Configuracion por defecto:
#   NB_TRIALS_LFR = 100
#   MAX_REDES_LFR = 0       (0 significa todas)
#   REANUDAR_LFR = FALSE
#
# Prueba rapida:
#   $env:NB_TRIALS_LFR = "5"
#   $env:MAX_REDES_LFR = "1"
#   Rscript tesis/scripts/comparaciones/30_ejecutar_barrido_lfr.R
#
# Ejecucion completa:
#   Remove-Item Env:NB_TRIALS_LFR
#   Remove-Item Env:MAX_REDES_LFR
#   Rscript tesis/scripts/comparaciones/30_ejecutar_barrido_lfr.R

cat("============================================================\n")
cat("BARRIDO PILOTO LFR JERARQUICO\n")
cat("============================================================\n\n")

suppressPackageStartupMessages(library(igraph))

if (!requireNamespace("infomap", quietly = TRUE)) {
  stop("Falta instalar el paquete oficial infomap.")
}

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
audit_path <- file.path(pilot_dir, "auditoria_piloto_corregida.csv")

if (!file.exists(manifest_path)) {
  stop("No se encontro: ", manifest_path)
}

if (!file.exists(audit_path)) {
  stop("No se encontro: ", audit_path)
}

manifest <- read.csv(
  manifest_path,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

audit <- read.csv(
  audit_path,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

audit_ok <- audit[audit$estado_estructural == "OK", , drop = FALSE]

cases <- merge(
  manifest,
  audit_ok[, c(
    "configuracion",
    "semilla_inicial",
    "estado_estructural"
  )],
  by = c("configuracion", "semilla_inicial"),
  all = FALSE,
  sort = TRUE
)

if (nrow(cases) == 0L) {
  stop("No hay redes estructuralmente validas para analizar.")
}

nb_trials <- as.integer(
  Sys.getenv("NB_TRIALS_LFR", unset = "100")
)

max_networks <- as.integer(
  Sys.getenv("MAX_REDES_LFR", unset = "0")
)

resume_run <- toupper(
  Sys.getenv("REANUDAR_LFR", unset = "FALSE")
) %in% c("TRUE", "T", "1", "SI", "SÍ")

if (is.na(nb_trials) || nb_trials < 1L) {
  stop("NB_TRIALS_LFR debe ser un entero positivo.")
}

if (is.na(max_networks) || max_networks < 0L) {
  stop("MAX_REDES_LFR debe ser 0 o un entero positivo.")
}

if (max_networks > 0L) {
  cases <- head(cases, max_networks)
}

out_dir <- file.path(
  repo_dir,
  "tesis",
  "resultados",
  "comparaciones",
  "barrido_lfr_piloto"
)

node_dir <- file.path(out_dir, "asignaciones_por_nodo")

dir.create(node_dir, recursive = TRUE, showWarnings = FALSE)

summary_path <- file.path(out_dir, "resultados_por_red.csv")
checkpoint_path <- file.path(out_dir, "resultados_parciales.csv")
log_path <- file.path(out_dir, "salida_barrido.txt")

safe_scalar <- function(x) {
  value <- tryCatch(
    as.numeric(x),
    error = function(e) NA_real_
  )

  if (length(value) == 0L) {
    return(NA_real_)
  }

  value[[1]]
}

normalize_membership <- function(x) {
  as.integer(factor(x, levels = unique(x)))
}

safe_compare <- function(a, b, method) {
  tryCatch(
    as.numeric(
      igraph::compare(
        normalize_membership(a),
        normalize_membership(b),
        method = method
      )
    ),
    error = function(e) NA_real_
  )
}

partition_metrics <- function(a, b, prefix) {
  ok <- complete.cases(a, b)
  a <- as.integer(a[ok])
  b <- as.integer(b[ok])

  output <- data.frame(
    nodes = length(a),
    communities_a = length(unique(a)),
    communities_b = length(unique(b)),
    exact = isTRUE(all(
      normalize_membership(a) == normalize_membership(b)
    )),
    nmi = safe_compare(a, b, "nmi"),
    ari = safe_compare(a, b, "adjusted.rand")
  )

  names(output) <- paste0(prefix, "_", names(output))
  output
}

read_membership <- function(path, column_name) {
  x <- read.table(
    path,
    header = FALSE,
    stringsAsFactors = FALSE,
    comment.char = "",
    col.names = c("node_id", column_name)
  )

  x$node_id <- as.integer(x$node_id)
  x[[column_name]] <- as.integer(x[[column_name]])
  x
}

read_lfr_graph <- function(case_dir, n_nodes) {
  edges <- read.table(
    file.path(case_dir, "network.dat"),
    header = FALSE,
    stringsAsFactors = FALSE,
    comment.char = "",
    col.names = c("from", "to")
  )

  edges$from <- as.integer(edges$from)
  edges$to <- as.integer(edges$to)

  unique_edges <- unique(data.frame(
    from = pmin(edges$from, edges$to),
    to = pmax(edges$from, edges$to)
  ))

  unique_edges <- unique_edges[
    unique_edges$from != unique_edges$to,
    ,
    drop = FALSE
  ]

  graph <- graph_from_data_frame(
    unique_edges,
    directed = FALSE,
    vertices = data.frame(name = as.character(seq_len(n_nodes)))
  )

  graph <- set_edge_attr(
    graph,
    name = "weight",
    value = rep(1, ecount(graph))
  )

  graph
}

extract_modified <- function(result) {
  if (!"multilevel_modules" %in% names(result)) {
    stop("igraph modificado no devolvio multilevel_modules.")
  }

  table <- as.data.frame(result$multilevel_modules)

  node_id <- if ("node_id" %in% names(table)) {
    as.integer(table$node_id)
  } else {
    seq_len(nrow(table))
  }

  if ("top_module" %in% names(table)) {
    top_column <- "top_module"
  } else {
    level_columns <- grep(
      "^level_[0-9]+$",
      names(table),
      value = TRUE
    )

    level_numbers <- as.integer(
      sub("^level_", "", level_columns)
    )

    level_columns <- level_columns[order(level_numbers)]

    top_level <- suppressWarnings(
      as.integer(result$top_module_level)
    )

    candidate <- paste0("level_", top_level)

    if (length(top_level) == 1L &&
        !is.na(top_level) &&
        candidate %in% names(table)) {
      top_column <- candidate
    } else {
      nontrivial <- level_columns[
        vapply(
          table[level_columns],
          function(x) length(unique(x[!is.na(x)])) > 1L,
          logical(1)
        )
      ]

      if (length(nontrivial) == 0L) {
        stop("No fue posible identificar el nivel superior.")
      }

      top_column <- nontrivial[[1]]
    }
  }

  data.frame(
    node_id = node_id,
    modified_top = as.integer(table[[top_column]]),
    modified_final = as.integer(membership(result)),
    modified_top_source = top_column,
    stringsAsFactors = FALSE
  )
}

extract_official <- function(result) {
  top <- as.data.frame(
    result$model,
    states = FALSE,
    depth_level = 1L,
    tibble = FALSE
  )

  final <- as.data.frame(
    result$model,
    states = FALSE,
    depth_level = -1L,
    tibble = FALSE
  )

  top <- top[, c("node_id", "module_id"), drop = FALSE]
  final <- final[, c("node_id", "module_id"), drop = FALSE]

  top <- top[!duplicated(top$node_id), , drop = FALSE]
  final <- final[!duplicated(final$node_id), , drop = FALSE]

  names(top)[2] <- "official_top"
  names(final)[2] <- "official_final"

  merge(
    top,
    final,
    by = "node_id",
    all = TRUE,
    sort = TRUE
  )
}

completed_keys <- character()
previous_results <- NULL

if (resume_run && file.exists(summary_path)) {
  previous_results <- read.csv(
    summary_path,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  completed_keys <- paste(
    previous_results$configuracion,
    previous_results$semilla_red,
    sep = "::"
  )

  cat(
    "Reanudacion activada. Casos ya completados:",
    length(completed_keys),
    "\n\n"
  )
}

results <- list()

if (!is.null(previous_results) && nrow(previous_results) > 0L) {
  results[[1]] <- previous_results
}

sink(log_path, split = TRUE)
on.exit(sink(), add = TRUE)

cat("Version igraph:", as.character(packageVersion("igraph")), "\n")
cat("Version Infomap oficial:", as.character(packageVersion("infomap")), "\n")
cat("Ensayos por ejecucion:", nb_trials, "\n")
cat("Redes seleccionadas:", nrow(cases), "\n")
cat("Salida:", out_dir, "\n\n")

for (i in seq_len(nrow(cases))) {
  row <- cases[i, , drop = FALSE]
  key <- paste(row$configuracion, row$semilla_inicial, sep = "::")

  if (key %in% completed_keys) {
    cat(
      sprintf(
        "[%d/%d] Omitida por reanudacion: %s, semilla %s\n",
        i,
        nrow(cases),
        row$configuracion,
        row$semilla_inicial
      )
    )
    next
  }

  cat("\n------------------------------------------------------------\n")
  cat(
    sprintf(
      "[%d/%d] %s, semilla de red %s\n",
      i,
      nrow(cases),
      row$configuracion,
      row$semilla_inicial
    )
  )
  cat("------------------------------------------------------------\n")

  case_dir <- file.path(repo_dir, row$directorio)

  truth_micro <- read_membership(
    file.path(case_dir, "community_first_level.dat"),
    "truth_micro"
  )

  truth_macro <- read_membership(
    file.path(case_dir, "community_second_level.dat"),
    "truth_macro"
  )

  truth <- merge(
    truth_micro,
    truth_macro,
    by = "node_id",
    all = TRUE,
    sort = TRUE
  )

  graph <- read_lfr_graph(case_dir, nrow(truth))
  algorithm_seed <- as.integer(row$semilla_inicial)

  status <- "OK"
  error_message <- ""

  result_row <- tryCatch(
    {
      start_modified <- proc.time()[["elapsed"]]

      set.seed(algorithm_seed)

      modified <- igraph::cluster_infomap(
        graph,
        e.weights = edge_attr(graph, "weight"),
        nb.trials = nb_trials
      )

      time_modified <- proc.time()[["elapsed"]] - start_modified

      start_official <- proc.time()[["elapsed"]]

      official <- infomap::cluster_infomap(
        graph,
        weight = "weight",
        nb.trials = nb_trials,
        seed = algorithm_seed,
        regularized = FALSE,
        two_level = FALSE,
        silent = TRUE,
        tibble = FALSE
      )

      time_official <- proc.time()[["elapsed"]] - start_official

      modified_table <- extract_modified(modified)
      official_table <- extract_official(official)

      assignments <- Reduce(
        function(x, y) {
          merge(x, y, by = "node_id", all = TRUE, sort = TRUE)
        },
        list(truth, modified_table, official_table)
      )

      case_name <- paste0(
        row$configuracion,
        "_seed_",
        row$semilla_inicial
      )

      write.csv(
        assignments,
        file.path(node_dir, paste0(case_name, ".csv")),
        row.names = FALSE
      )

      metrics <- cbind(
        partition_metrics(
          assignments$modified_top,
          assignments$truth_macro,
          "modified_top_vs_truth_macro"
        ),
        partition_metrics(
          assignments$modified_final,
          assignments$truth_micro,
          "modified_final_vs_truth_micro"
        ),
        partition_metrics(
          assignments$official_top,
          assignments$truth_macro,
          "official_top_vs_truth_macro"
        ),
        partition_metrics(
          assignments$official_final,
          assignments$truth_micro,
          "official_final_vs_truth_micro"
        ),
        partition_metrics(
          assignments$modified_top,
          assignments$official_top,
          "modified_top_vs_official_top"
        ),
        partition_metrics(
          assignments$modified_final,
          assignments$official_final,
          "modified_final_vs_official_final"
        )
      )

      data.frame(
        configuracion = row$configuracion,
        mu1 = as.numeric(row$mu1),
        mu2 = as.numeric(row$mu2),
        misma_micro = as.numeric(row$fraccion_misma_micro),
        semilla_red = as.integer(row$semilla_inicial),
        semilla_algoritmo = algorithm_seed,
        ensayos = nb_trials,
        nodos = vcount(graph),
        aristas = ecount(graph),
        microcomunidades_reales = length(unique(truth$truth_micro)),
        macrocomunidades_reales = length(unique(truth$truth_macro)),
        tiempo_modificado_seg = time_modified,
        tiempo_oficial_seg = time_official,
        codelength_modificado = safe_scalar(modified$codelength),
        codelength_oficial = safe_scalar(official$codelength),
        num_levels_modificado = safe_scalar(modified$num_levels),
        max_tree_depth_modificado = safe_scalar(
          modified$max_tree_depth
        ),
        num_top_modules_modificado = safe_scalar(
          modified$num_top_modules
        ),
        num_levels_oficial = safe_scalar(
          official$model$num_levels
        ),
        max_tree_depth_oficial = safe_scalar(
          official$model$max_tree_depth
        ),
        num_top_modules_oficial = safe_scalar(
          official$model$num_top_modules
        ),
        status = status,
        error = error_message,
        metrics,
        stringsAsFactors = FALSE
      )
    },
    error = function(e) {
      status <<- "ERROR"
      error_message <<- conditionMessage(e)

      data.frame(
        configuracion = row$configuracion,
        mu1 = as.numeric(row$mu1),
        mu2 = as.numeric(row$mu2),
        misma_micro = as.numeric(row$fraccion_misma_micro),
        semilla_red = as.integer(row$semilla_inicial),
        semilla_algoritmo = algorithm_seed,
        ensayos = nb_trials,
        nodos = vcount(graph),
        aristas = ecount(graph),
        status = status,
        error = error_message,
        stringsAsFactors = FALSE
      )
    }
  )

  results[[length(results) + 1L]] <- result_row

  current_results <- do.call(rbind, results)

  write.csv(
    current_results,
    checkpoint_path,
    row.names = FALSE
  )

  cat("Estado:", status, "\n")

  if (status == "ERROR") {
    cat("Error:", error_message, "\n")
  } else {
    cat(
      "NMI modificado macro:",
      result_row$modified_top_vs_truth_macro_nmi,
      "\n"
    )
    cat(
      "NMI modificado micro:",
      result_row$modified_final_vs_truth_micro_nmi,
      "\n"
    )
    cat(
      "NMI oficial macro:",
      result_row$official_top_vs_truth_macro_nmi,
      "\n"
    )
    cat(
      "NMI oficial micro:",
      result_row$official_final_vs_truth_micro_nmi,
      "\n"
    )
  }
}

final_results <- do.call(rbind, results)

write.csv(
  final_results,
  summary_path,
  row.names = FALSE
)

cat("\n============================================================\n")
cat("BARRIDO TERMINADO\n")
cat("============================================================\n")
cat("Casos guardados:", nrow(final_results), "\n")
cat("Resultados:", summary_path, "\n")
cat("Asignaciones:", node_dir, "\n")
cat("Log:", log_path, "\n")
