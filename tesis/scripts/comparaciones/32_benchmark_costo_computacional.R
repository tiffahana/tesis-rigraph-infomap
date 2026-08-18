# Benchmark de costo computacional de cluster_infomap() original y modificado
#
# Ejecutar desde la raÃƒÂ­z del repositorio:
#   Rscript tesis/scripts/comparaciones/32_benchmark_costo_computacional.R
#
# Variables de entorno principales:
#   IGRAPH_ORIGINAL_LIB       Biblioteca que contiene igraph original.
#   IGRAPH_MODIFIED_LIB       Biblioteca que contiene igraph modificado.
#   BENCHMARK_REPETITIONS     Repeticiones medidas por red (mÃƒÂ­nimo 5; por defecto 5).
#   BENCHMARK_WARMUP          Ejecuciones de calentamiento por red (por defecto 1).
#   BENCHMARK_INCLUDE_SWEEP   TRUE para incluir las 27 redes LFR (por defecto TRUE).
#   BENCHMARK_MAX_SWEEP       0 para todas las redes; otro valor limita la prueba.
#   BENCHMARK_ALLOW_FEWER     TRUE permite menos de 5 repeticiones solo para pruebas rÃƒÂ¡pidas.
#
# La comparaciÃƒÂ³n total usa system.time(). En el prototipo tambiÃƒÂ©n activa la
# instrumentaciÃƒÂ³n opcional de R/community.R para registrar el costo de construir
# cada grafo agregado y de cada re-ejecuciÃƒÂ³n de Infomap por nivel.

args <- commandArgs(trailingOnly = TRUE)

arg_value <- function(name, default = NULL) {
  index <- match(name, args)
  if (is.na(index) || index == length(args)) {
    return(default)
  }
  args[[index + 1L]]
}

as_flag <- function(x, default = FALSE) {
  if (is.null(x) || is.na(x) || !nzchar(x)) {
    return(default)
  }
  toupper(x) %in% c("TRUE", "T", "1", "SI", "SÃƒÂ", "YES", "Y")
}

worker_mode <- "--worker" %in% args

safe_sd <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) <= 1L) {
    return(0)
  }
  stats::sd(x)
}

mean_sd_text <- function(x, digits = 3L) {
  x <- x[is.finite(x)]
  if (length(x) == 0L) {
    return(NA_character_)
  }
  sprintf(
    paste0("%.", digits, "f Ã‚Â± %.", digits, "f"),
    mean(x),
    safe_sd(x)
  )
}

normalize_repo <- function() {
  repo_dir <- normalizePath(".", winslash = "/", mustWork = TRUE)
  if (!file.exists(file.path(repo_dir, "DESCRIPTION")) ||
      !dir.exists(file.path(repo_dir, "tesis"))) {
    stop("Ejecute el script desde la raÃƒÂ­z del repositorio.")
  }
  repo_dir
}

if (worker_mode) {
  variant <- arg_value("--variant")
  library_path <- arg_value("--library")
  output_path <- arg_value("--output")
  level_output_path <- arg_value("--level-output")
  repetitions <- as.integer(arg_value("--repetitions", "5"))
  warmup <- as.integer(arg_value("--warmup", "1"))
  include_sweep <- as_flag(arg_value("--include-sweep", "TRUE"), TRUE)
  max_sweep <- as.integer(arg_value("--max-sweep", "0"))

  if (!variant %in% c("original", "prototipo")) {
    stop("--variant debe ser original o prototipo.")
  }
  if (is.null(library_path) || !dir.exists(library_path)) {
    stop("No existe la biblioteca indicada: ", library_path)
  }
  if (is.null(output_path)) {
    stop("Falta --output.")
  }

  repo_dir <- normalize_repo()
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)

  suppressPackageStartupMessages(
    library("igraph", lib.loc = library_path, character.only = TRUE)
  )

  if (!requireNamespace("igraphdata", quietly = TRUE)) {
    stop("Falta instalar el paquete igraphdata.")
  }

  package_version <- as.character(packageVersion("igraph"))
  package_path <- find.package("igraph")

  load_igraphdata_graph <- function(name) {
    env <- new.env(parent = emptyenv())
    data(list = name, package = "igraphdata", envir = env)
    graph <- get(name, envir = env)
    if (exists("upgrade_graph", where = asNamespace("igraph"), mode = "function")) {
      graph <- upgrade_graph(graph)
    }
    graph
  }

  largest_component <- function(graph) {
    comp <- components(as.undirected(graph))
    induced_subgraph(graph, which(comp$membership == which.max(comp$csize)))
  }

  prepare_real_graph <- function(graph) {
    if (exists("upgrade_graph", where = asNamespace("igraph"), mode = "function")) {
      graph <- upgrade_graph(graph)
    }
    if (!"weight" %in% edge_attr_names(graph)) {
      E(graph)$weight <- 1
    }
    if (is_directed(graph)) {
      graph <- as.undirected(
        graph,
        mode = "collapse",
        edge.attr.comb = list(weight = "sum", "ignore")
      )
    }
    graph <- simplify(
      graph,
      remove.multiple = TRUE,
      remove.loops = TRUE,
      edge.attr.comb = list(weight = "sum", "ignore")
    )
    if (components(graph)$no > 1L) {
      graph <- largest_component(graph)
    }
    graph
  }

  read_lfr_graph <- function(case_dir, n_nodes = NULL) {
    edge_path <- file.path(case_dir, "network.dat")
    if (!file.exists(edge_path)) {
      stop("No existe: ", edge_path)
    }
    edges <- read.table(
      edge_path,
      header = FALSE,
      col.names = c("from", "to"),
      stringsAsFactors = FALSE,
      comment.char = ""
    )
    edges <- unique(data.frame(
      from = pmin(as.integer(edges$from), as.integer(edges$to)),
      to = pmax(as.integer(edges$from), as.integer(edges$to))
    ))
    edges <- edges[edges$from != edges$to, , drop = FALSE]

    if (is.null(n_nodes)) {
      membership_path <- file.path(case_dir, "community_first_level.dat")
      membership_data <- read.table(
        membership_path,
        header = FALSE,
        stringsAsFactors = FALSE,
        comment.char = ""
      )
      n_nodes <- nrow(membership_data)
    }

    graph <- graph_from_data_frame(
      transform(edges, from = as.character(from), to = as.character(to)),
      directed = FALSE,
      vertices = data.frame(name = as.character(seq_len(n_nodes)))
    )
    E(graph)$weight <- 1
    simplify(
      graph,
      remove.multiple = TRUE,
      remove.loops = TRUE,
      edge.attr.comb = list(weight = "sum", "ignore")
    )
  }

  main_cases <- list(
    list(
      scope = "principal",
      red = "Zachary Karate Club",
      configuracion = "Zachary",
      semilla_red = NA_integer_,
      semilla_algoritmo = 123L,
      nb_trials = 500L,
      loader = function() prepare_real_graph(make_graph("Zachary"))
    ),
    list(
      scope = "principal",
      red = "USairports",
      configuracion = "USairports",
      semilla_red = NA_integer_,
      semilla_algoritmo = 123L,
      nb_trials = 500L,
      loader = function() prepare_real_graph(load_igraphdata_graph("USairports"))
    ),
    list(
      scope = "principal",
      red = "Yeast",
      configuracion = "yeast",
      semilla_red = NA_integer_,
      semilla_algoritmo = 123L,
      nb_trials = 500L,
      loader = function() prepare_real_graph(load_igraphdata_graph("yeast"))
    ),
    list(
      scope = "principal",
      red = "LFR 5000",
      configuracion = "LFR_5000_jerarquia_reforzada",
      semilla_red = NA_integer_,
      semilla_algoritmo = 123L,
      nb_trials = 100L,
      loader = function() read_lfr_graph(
        file.path(repo_dir, "tesis", "datos", "lfr", "jerarquia_reforzada"),
        5000L
      )
    )
  )

  sweep_cases <- list()
  if (include_sweep) {
    pilot_dir <- file.path(
      repo_dir,
      "tesis",
      "experimentos",
      "barrido_lfr",
      "piloto"
    )
    manifest_path <- file.path(pilot_dir, "manifesto_piloto.csv")
    audit_path <- file.path(pilot_dir, "auditoria_piloto.csv")
    if (!file.exists(manifest_path) || !file.exists(audit_path)) {
      stop("No se encontraron el manifiesto y la auditorÃƒÂ­a del barrido LFR.")
    }
    manifest <- read.csv(manifest_path, stringsAsFactors = FALSE, check.names = FALSE)
    audit <- read.csv(audit_path, stringsAsFactors = FALSE, check.names = FALSE)
    audit_ok <- audit[audit$estado_auditoria == "OK", , drop = FALSE]
    cases <- merge(
      manifest,
      audit_ok[, c("configuracion", "semilla_inicial", "estado_auditoria")],
      by = c("configuracion", "semilla_inicial"),
      all = FALSE,
      sort = TRUE
    )
    if (max_sweep > 0L) {
      cases <- head(cases, max_sweep)
    }

    for (i in seq_len(nrow(cases))) {
      row <- cases[i, , drop = FALSE]
      case_dir <- file.path(repo_dir, row$directorio)
      n_nodes <- nrow(read.table(
        file.path(case_dir, "community_first_level.dat"),
        header = FALSE,
        stringsAsFactors = FALSE,
        comment.char = ""
      ))
      local({
        row_local <- row
        case_dir_local <- case_dir
        n_nodes_local <- n_nodes
        sweep_cases[[length(sweep_cases) + 1L]] <<- list(
          scope = "barrido_lfr",
          red = paste0(
            "LFR ", row_local$configuracion,
            " semilla ", row_local$semilla_inicial
          ),
          configuracion = as.character(row_local$configuracion),
          semilla_red = as.integer(row_local$semilla_inicial),
          semilla_algoritmo = as.integer(row_local$semilla_inicial),
          nb_trials = 100L,
          mu1 = as.numeric(row_local$mu1),
          mu2 = as.numeric(row_local$mu2),
          loader = function() read_lfr_graph(case_dir_local, n_nodes_local)
        )
      })
    }
  }

  all_cases <- c(main_cases, sweep_cases)
  raw_rows <- list()
  level_rows <- list()

  run_once <- function(graph, case, repetition, measured = TRUE) {
    gc(verbose = FALSE)
    set.seed(case$semilla_algoritmo)

    timing_env <- NULL
    old_timing_option <- getOption("igraph.infomap.timing_env")
    if (variant == "prototipo" && measured) {
      timing_env <- new.env(parent = emptyenv())
      options(igraph.infomap.timing_env = timing_env)
    } else {
      options(igraph.infomap.timing_env = NULL)
    }
    on.exit(options(igraph.infomap.timing_env = old_timing_option), add = TRUE)

    elapsed <- system.time({
      result <- cluster_infomap(
        graph,
        e.weights = E(graph)$weight,
        nb.trials = case$nb_trials
      )
    })[["elapsed"]]

    if (variant == "prototipo" && is.null(result$multilevel_modules)) {
      stop(
        "La biblioteca marcada como prototipo no contiene multilevel_modules: ",
        package_path
      )
    }

    timing <- if (!is.null(timing_env) && exists("last", timing_env, inherits = FALSE)) {
      timing_env$last
    } else {
      NULL
    }

    if (variant == "prototipo" && measured && is.null(timing)) {
      stop(
        "No se recibiÃƒÂ³ la instrumentaciÃƒÂ³n por nivel. Reinstale el prototipo ",
        "despuÃƒÂ©s de aplicar instrumentacion_tiempos_community.patch."
      )
    }

    row <- data.frame(
      variante = variant,
      version_igraph = package_version,
      ruta_igraph = package_path,
      scope = case$scope,
      red = case$red,
      configuracion = case$configuracion,
      mu1 = if (!is.null(case$mu1)) case$mu1 else NA_real_,
      mu2 = if (!is.null(case$mu2)) case$mu2 else NA_real_,
      semilla_red = case$semilla_red,
      semilla_algoritmo = case$semilla_algoritmo,
      repeticion = as.integer(repetition),
      nb_trials = case$nb_trials,
      nodos = vcount(graph),
      aristas = ecount(graph),
      tiempo_total_seg = as.numeric(elapsed),
      comunidades_finales = length(unique(membership(result))),
      codelength = as.numeric(result$codelength),
      modularidad = as.numeric(modularity(result)),
      num_levels = if (!is.null(result$num_levels)) as.integer(result$num_levels) else NA_integer_,
      num_top_modules = if (!is.null(result$num_top_modules)) as.integer(result$num_top_modules) else NA_integer_,
      max_tree_depth = if (!is.null(result$max_tree_depth)) as.integer(result$max_tree_depth) else NA_integer_,
      tiempo_infomap_inicial_seg = if (!is.null(timing)) timing$initial_infomap_elapsed_sec else NA_real_,
      tiempo_reconstruccion_jerarquia_seg = if (!is.null(timing)) timing$hierarchy_elapsed_sec else NA_real_,
      reejecuciones_infomap = if (!is.null(timing)) timing$hierarchy_reexecutions else NA_integer_,
      stringsAsFactors = FALSE
    )

    levels <- NULL
    if (!is.null(timing) && nrow(timing$level_timings) > 0L) {
      levels <- cbind(
        row[, c(
          "variante", "scope", "red", "configuracion", "semilla_red",
          "semilla_algoritmo", "repeticion", "nb_trials", "nodos", "aristas"
        ), drop = FALSE][rep(1L, nrow(timing$level_timings)), , drop = FALSE],
        timing$level_timings
      )
    }

    list(row = row, levels = levels)
  }

  for (case_index in seq_along(all_cases)) {
    case <- all_cases[[case_index]]
    cat(sprintf(
      "[%d/%d] %s (%s)\n",
      case_index, length(all_cases), case$red, variant
    ))
    graph <- case$loader()

    if (warmup > 0L) {
      for (w in seq_len(warmup)) {
        invisible(run_once(graph, case, repetition = 0L, measured = FALSE))
      }
    }

    for (rep_index in seq_len(repetitions)) {
      output <- run_once(graph, case, repetition = rep_index, measured = TRUE)
      raw_rows[[length(raw_rows) + 1L]] <- output$row
      if (!is.null(output$levels)) {
        level_rows[[length(level_rows) + 1L]] <- output$levels
      }
      write.csv(do.call(rbind, raw_rows), output_path, row.names = FALSE)
      if (!is.null(level_output_path) && length(level_rows) > 0L) {
        write.csv(do.call(rbind, level_rows), level_output_path, row.names = FALSE)
      }
    }
  }

  write.csv(do.call(rbind, raw_rows), output_path, row.names = FALSE)
  if (!is.null(level_output_path)) {
    if (length(level_rows) > 0L) {
      write.csv(do.call(rbind, level_rows), level_output_path, row.names = FALSE)
    } else {
      write.csv(data.frame(), level_output_path, row.names = FALSE)
    }
  }

  session_path <- sub("\\.csv$", "_sessionInfo.txt", output_path)
  capture.output(sessionInfo(), file = session_path)
  quit(save = "no", status = 0L)
}

# -----------------------------------------------------------------------------
# Modo coordinador
# -----------------------------------------------------------------------------
repo_dir <- normalize_repo()
script_path <- normalizePath(
  file.path(repo_dir, "tesis", "scripts", "comparaciones", "32_benchmark_costo_computacional.R"),
  winslash = "/",
  mustWork = TRUE
)
out_dir <- file.path(
  repo_dir,
  "tesis",
  "resultados",
  "comparaciones",
  "costo_computacional"
)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

repetitions <- as.integer(Sys.getenv("BENCHMARK_REPETITIONS", unset = "5"))
warmup <- as.integer(Sys.getenv("BENCHMARK_WARMUP", unset = "1"))
include_sweep <- as_flag(Sys.getenv("BENCHMARK_INCLUDE_SWEEP", unset = "TRUE"), TRUE)
max_sweep <- as.integer(Sys.getenv("BENCHMARK_MAX_SWEEP", unset = "0"))
allow_fewer <- as_flag(Sys.getenv("BENCHMARK_ALLOW_FEWER", unset = "FALSE"), FALSE)

if (is.na(repetitions) || repetitions < 1L) {
  stop("BENCHMARK_REPETITIONS debe ser un entero positivo.")
}
if (repetitions < 5L && !allow_fewer) {
  stop(
    "Se requieren al menos 5 repeticiones. Para una prueba rÃƒÂ¡pida, defina ",
    "BENCHMARK_ALLOW_FEWER=TRUE."
  )
}

original_lib <- Sys.getenv(
  "IGRAPH_ORIGINAL_LIB",
  unset = file.path(repo_dir, "tesis", "lib", "igraph_original")
)
if (!dir.exists(original_lib)) {
  stop(
    "No se encontrÃƒÂ³ la biblioteca de igraph original. Defina IGRAPH_ORIGINAL_LIB. ",
    "Ruta evaluada: ", original_lib
  )
}

rscript <- file.path(R.home("bin"), "Rscript")
modified_lib <- Sys.getenv("IGRAPH_MODIFIED_LIB", unset = "")
if (!nzchar(modified_lib)) {
  detected <- system2(
    rscript,
    c("-e", shQuote("cat(dirname(find.package('igraph')))")),
    stdout = TRUE,
    stderr = TRUE
  )
  modified_lib <- detected[[length(detected)]]
}
if (!dir.exists(modified_lib)) {
  stop("No se encontrÃƒÂ³ IGRAPH_MODIFIED_LIB: ", modified_lib)
}

original_raw <- file.path(out_dir, "benchmark_costo_computacional_original_crudo.csv")
prototype_raw <- file.path(out_dir, "benchmark_costo_computacional_prototipo_crudo.csv")
level_raw <- file.path(out_dir, "benchmark_costo_computacional_por_nivel_crudo.csv")

run_worker <- function(variant, library_path, output_path, level_path = "") {
  library_path <- normalizePath(library_path, winslash = "/", mustWork = TRUE)
  output_path <- file.path(
    normalizePath(dirname(output_path), winslash = "/", mustWork = TRUE),
    basename(output_path)
  )
  if (nzchar(level_path)) {
    level_path <- file.path(
      normalizePath(dirname(level_path), winslash = "/", mustWork = TRUE),
      basename(level_path)
    )
  }

  command_args <- c(
    shQuote(script_path),
    "--worker",
    "--variant", variant,
    "--library", shQuote(library_path),
    "--output", shQuote(output_path),
    "--repetitions", as.character(repetitions),
    "--warmup", as.character(warmup),
    "--include-sweep", if (include_sweep) "TRUE" else "FALSE",
    "--max-sweep", as.character(max_sweep)
  )
  if (nzchar(level_path)) {
    command_args <- c(command_args, "--level-output", shQuote(level_path))
  }
  status <- system2(rscript, command_args)
  if (!identical(status, 0L)) {
    stop("FallÃƒÂ³ el worker ", variant, " con cÃƒÂ³digo ", status, ".")
  }
}

cat("Ejecutando benchmark con igraph original...\n")
run_worker("original", original_lib, original_raw)
cat("Ejecutando benchmark con el prototipo...\n")
run_worker("prototipo", modified_lib, prototype_raw, level_raw)

original <- read.csv(original_raw, stringsAsFactors = FALSE, check.names = FALSE)
prototype <- read.csv(prototype_raw, stringsAsFactors = FALSE, check.names = FALSE)

keys <- c(
  "scope", "red", "configuracion", "mu1", "mu2", "semilla_red",
  "semilla_algoritmo", "repeticion", "nb_trials", "nodos", "aristas"
)
paired <- merge(
  original,
  prototype,
  by = keys,
  suffixes = c("_original", "_prototipo"),
  all = FALSE,
  sort = TRUE
)
paired$overhead_absoluto_seg <-
  paired$tiempo_total_seg_prototipo - paired$tiempo_total_seg_original
paired$overhead_relativo_pct <- 100 * paired$overhead_absoluto_seg /
  paired$tiempo_total_seg_original

paired_path <- file.path(out_dir, "benchmark_costo_computacional_pareado_crudo.csv")
write.csv(paired, paired_path, row.names = FALSE)

summarize_groups <- function(data, group_columns) {
  split_key <- interaction(data[group_columns], drop = TRUE, lex.order = TRUE)
  groups <- split(data, split_key)
  rows <- lapply(groups, function(group) {
    first <- group[1L, group_columns, drop = FALSE]
    data.frame(
      first,
      repeticiones = nrow(group),
      nodos = group$nodos[[1]],
      aristas = group$aristas[[1]],
      nb_trials = group$nb_trials[[1]],
      tiempo_original_media_seg = mean(group$tiempo_total_seg_original),
      tiempo_original_sd_seg = safe_sd(group$tiempo_total_seg_original),
      tiempo_prototipo_media_seg = mean(group$tiempo_total_seg_prototipo),
      tiempo_prototipo_sd_seg = safe_sd(group$tiempo_total_seg_prototipo),
      overhead_absoluto_media_seg = mean(group$overhead_absoluto_seg),
      overhead_absoluto_sd_seg = safe_sd(group$overhead_absoluto_seg),
      overhead_relativo_media_pct = mean(group$overhead_relativo_pct),
      overhead_relativo_sd_pct = safe_sd(group$overhead_relativo_pct),
      max_tree_depth = round(mean(group$max_tree_depth_prototipo, na.rm = TRUE)),
      num_levels = round(mean(group$num_levels_prototipo, na.rm = TRUE)),
      reejecuciones_infomap_media = mean(group$reejecuciones_infomap_prototipo, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

main_data <- paired[paired$scope == "principal", , drop = FALSE]
main_summary <- summarize_groups(main_data, c("scope", "red", "configuracion"))
main_summary_path <- file.path(out_dir, "benchmark_costo_computacional_resumen_redes.csv")
write.csv(main_summary, main_summary_path, row.names = FALSE)

memory_table <- data.frame(
  Red = main_summary$red,
  Nodos = main_summary$nodos,
  Aristas = main_summary$aristas,
  `Tiempo igraph original (s)` = vapply(
    split(main_data$tiempo_total_seg_original, main_data$red),
    mean_sd_text,
    character(1)
  )[main_summary$red],
  `Tiempo prototipo (s)` = vapply(
    split(main_data$tiempo_total_seg_prototipo, main_data$red),
    mean_sd_text,
    character(1)
  )[main_summary$red],
  `Overhead absoluto (s)` = vapply(
    split(main_data$overhead_absoluto_seg, main_data$red),
    mean_sd_text,
    character(1)
  )[main_summary$red],
  `Overhead relativo (%)` = vapply(
    split(main_data$overhead_relativo_pct, main_data$red),
    mean_sd_text,
    character(1)
  )[main_summary$red],
  `Profundidad mÃƒÂ¡xima` = main_summary$max_tree_depth,
  `Reejecuciones de Infomap` = round(main_summary$reejecuciones_infomap_media, 1),
  check.names = FALSE,
  stringsAsFactors = FALSE
)
memory_table_path <- file.path(out_dir, "tabla_costo_computacional_memoria.csv")
write.csv(memory_table, memory_table_path, row.names = FALSE, fileEncoding = "UTF-8")

sweep_data <- paired[paired$scope == "barrido_lfr", , drop = FALSE]
if (nrow(sweep_data) > 0L) {
  sweep_by_network <- summarize_groups(
    sweep_data,
    c("scope", "red", "configuracion", "mu1", "mu2", "semilla_red")
  )
  write.csv(
    sweep_by_network,
    file.path(out_dir, "benchmark_costo_computacional_barrido_por_red.csv"),
    row.names = FALSE
  )
  sweep_by_configuration <- summarize_groups(
    sweep_data,
    c("scope", "configuracion", "mu1", "mu2")
  )
  write.csv(
    sweep_by_configuration,
    file.path(out_dir, "benchmark_costo_computacional_barrido_por_configuracion.csv"),
    row.names = FALSE
  )
}

all_summary <- summarize_groups(paired, c("scope", "red", "configuracion"))
valid_size <- complete.cases(all_summary$nodos, all_summary$overhead_relativo_media_pct)
valid_depth <- complete.cases(all_summary$max_tree_depth, all_summary$overhead_relativo_media_pct)
cor_size <- if (sum(valid_size) >= 3L) {
  suppressWarnings(cor(
    log10(all_summary$nodos[valid_size]),
    all_summary$overhead_relativo_media_pct[valid_size],
    method = "spearman"
  ))
} else {
  NA_real_
}
cor_depth <- if (sum(valid_depth) >= 3L) {
  suppressWarnings(cor(
    all_summary$max_tree_depth[valid_depth],
    all_summary$overhead_relativo_media_pct[valid_depth],
    method = "spearman"
  ))
} else {
  NA_real_
}

association_text <- function(value, subject) {
  if (!is.finite(value)) {
    return(paste0("no fue posible estimar la asociaciÃƒÂ³n con ", subject))
  }
  strength <- if (abs(value) >= 0.7) {
    "fuerte"
  } else if (abs(value) >= 0.4) {
    "moderada"
  } else {
    "dÃƒÂ©bil"
  }
  direction <- if (value >= 0) "positiva" else "negativa"
  paste0("se observÃƒÂ³ una asociaciÃƒÂ³n ", direction, " ", strength, " con ", subject)
}

largest <- all_summary[which.max(all_summary$overhead_relativo_media_pct), , drop = FALSE]
interpretation <- paste0(
  "La versiÃƒÂ³n modificada presentÃƒÂ³ un tiempo medio mayor que igraph original debido a la ",
  "construcciÃƒÂ³n iterativa de grafos de mÃƒÂ³dulos y a las re-ejecuciones adicionales de Infomap. ",
  "En el conjunto evaluado, ", association_text(cor_size, "el tamaÃƒÂ±o de la red"),
  " y ", association_text(cor_depth, "la profundidad mÃƒÂ¡xima de la jerarquÃƒÂ­a"), ". ",
  "El mayor overhead relativo se registrÃƒÂ³ en ", largest$red,
  " (", sprintf("%.2f", largest$overhead_relativo_media_pct), " %). ",
  "Estas asociaciones son descriptivas y deben interpretarse con cautela, ya que el tiempo ",
  "tambiÃƒÂ©n depende de la estructura de aristas, la cantidad de mÃƒÂ³dulos intermedios y la ",
  "variabilidad propia del entorno de ejecuciÃƒÂ³n."
)
interpretation_path <- file.path(out_dir, "interpretacion_costo_computacional.txt")
writeLines(interpretation, interpretation_path, useBytes = TRUE)

metadata_path <- file.path(out_dir, "benchmark_costo_computacional_metadatos.txt")
metadata <- c(
  paste("Fecha:", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
  paste("Repeticiones medidas:", repetitions),
  paste("Calentamientos por red:", warmup),
  paste("Incluye barrido LFR:", include_sweep),
  paste("LÃƒÂ­mite del barrido (0 = todas):", max_sweep),
  paste("Biblioteca igraph original:", normalizePath(original_lib, winslash = "/")),
  paste("Biblioteca igraph modificada:", normalizePath(modified_lib, winslash = "/")),
  paste("Sistema:", paste(Sys.info(), collapse = " | ")),
  paste("NÃƒÂºcleos lÃƒÂ³gicos detectados:", parallel::detectCores(logical = TRUE))
)
writeLines(metadata, metadata_path, useBytes = TRUE)

cat("\nBenchmark finalizado. Archivos principales:\n")
cat("- ", paired_path, "\n", sep = "")
cat("- ", main_summary_path, "\n", sep = "")
cat("- ", memory_table_path, "\n", sep = "")
cat("- ", level_raw, "\n", sep = "")
cat("- ", interpretation_path, "\n", sep = "")
cat("- ", metadata_path, "\n", sep = "")
