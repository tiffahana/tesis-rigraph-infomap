# Figura 31 - validacion del prototipo multinivel (version corregida)
# Ejecutar desde PowerShell mediante Rscript.
#
# Esta version admite que multilevel_modules sea una matriz,
# un data.frame o una lista.

suppressPackageStartupMessages(
  library(igraph)
)

options(encoding = "UTF-8")

repo_dir <- normalizePath(
  getwd(),
  winslash = "/",
  mustWork = TRUE
)

if (!file.exists(file.path(repo_dir, "DESCRIPTION")) ||
    !dir.exists(file.path(repo_dir, "tesis"))) {
  stop(
    "Ejecute este script desde la raíz del repositorio ",
    "tesis-rigraph-infomap."
  )
}

base_dir <- file.path(repo_dir, "tesis")

archivo_rds <- file.path(
  base_dir,
  "datos", "lfr", "jerarquia_reforzada",
  "resultado_lfr_igraph_modificado.rds"
)

if (!file.exists(archivo_rds)) {
  stop(
    paste0(
      "No se encontro el resultado final LFR:\n",
      archivo_rds
    )
  )
}

resultado <- readRDS(archivo_rds)

campos_esperados <- c(
  "multilevel_modules",
  "num_levels",
  "num_top_modules",
  "top_module_level",
  "max_tree_depth"
)

campos_presentes <- campos_esperados %in% names(resultado)

cat("VALIDACION DEL PROTOTIPO MULTINIVEL\n")
cat("==================================\n\n")

cat("R:", R.version.string, "\n")
cat("igraph:", as.character(packageVersion("igraph")), "\n")
cat("Ruta de igraph:", find.package("igraph"), "\n\n")

cat("Clase del resultado:", paste(class(resultado), collapse = ", "), "\n")
cat("Campos multinivel presentes:", all(campos_presentes), "\n")

for (i in seq_along(campos_esperados)) {
  cat(
    "  ",
    campos_esperados[i],
    ": ",
    ifelse(campos_presentes[i], "PRESENTE", "AUSENTE"),
    "\n",
    sep = ""
  )
}

if (!all(campos_presentes)) {
  stop("Faltan uno o mas campos multinivel.")
}

cat("\nValores de resumen:\n")
cat("  num_levels =", resultado$num_levels, "\n")
cat("  num_top_modules =", resultado$num_top_modules, "\n")
cat("  top_module_level =", resultado$top_module_level, "\n")
cat("  max_tree_depth =", resultado$max_tree_depth, "\n")

multinivel <- resultado$multilevel_modules

# Extraer final_module de forma compatible con matriz, data.frame o lista.
if (
  (is.matrix(multinivel) || is.data.frame(multinivel)) &&
  !is.null(colnames(multinivel)) &&
  "final_module" %in% colnames(multinivel)
) {
  final_module <- multinivel[, "final_module"]
} else if (
  is.list(multinivel) &&
  !is.null(multinivel[["final_module"]])
) {
  final_module <- multinivel[["final_module"]]
} else {
  stop(
    paste0(
      "No fue posible extraer final_module. Clase de multilevel_modules: ",
      paste(class(multinivel), collapse = ", "),
      ". Columnas disponibles: ",
      paste(colnames(multinivel), collapse = ", ")
    )
  )
}

membership_final <- membership(resultado)

misma_longitud <- length(final_module) == length(membership_final)

coincide <- misma_longitud &&
  all(
    as.integer(final_module) ==
      as.integer(membership_final)
  )

cat(
  "\nClase de multilevel_modules:",
  paste(class(multinivel), collapse = ", "),
  "\n"
)

cat(
  "Cantidad de filas:",
  length(final_module),
  "\n"
)

cat(
  "final_module coincide con membership():",
  coincide,
  "\n"
)

if (!coincide) {
  stop("final_module no coincide con membership().")
}

cat("\nRESULTADO: PRUEBA COMPLETADA CORRECTAMENTE\n")
