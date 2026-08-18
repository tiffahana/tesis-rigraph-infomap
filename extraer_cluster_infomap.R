out_dir <- "tesis/resultados/comparaciones/costo_computacional/diagnostico"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

extraer_cluster_infomap <- function(lib, nombre_archivo) {
  ns <- loadNamespace("igraph", lib.loc = lib)

  funcion <- get(
    "cluster_infomap",
    envir = ns,
    inherits = FALSE
  )

  writeLines(
    deparse(funcion, width.cutoff = 500L),
    file.path(out_dir, nombre_archivo)
  )
}

extraer_cluster_infomap(
  "C:/Users/lysit/Desktop/Tesis igraph/rigraph-main/tesis/lib/igraph_original",
  "cluster_infomap_original.R"
)

extraer_cluster_infomap(
  "C:/Users/lysit/AppData/Local/R/win-library/4.6",
  "cluster_infomap_prototipo.R"
)

cat("Archivos de diagn?stico creados correctamente.\n")
