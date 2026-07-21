# Figura 15: campos y resumen multinivel del prototipo
# Tesis de Daniela Salinas Castro

library(igraph)

# Carpeta principal del proyecto
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

# Resultado final de la red jerÃ¡rquica LFR (5000 nodos)
archivo_rds <- file.path(
  base_dir,
  "datos", "lfr", "jerarquia_reforzada",
  "resultado_lfr_igraph_modificado.rds"
)

if (!file.exists(archivo_rds)) {
  stop(
    paste0(
      "No se encontrÃ³ el archivo:\n",
      archivo_rds,
      "\nRevisa que la carpeta principal del proyecto sea correcta."
    )
  )
}

resultado_modificado <- readRDS(archivo_rds)

campos_requeridos <- c(
  "multilevel_modules",
  "num_levels",
  "num_top_modules",
  "top_module_level",
  "max_tree_depth"
)

campos_presentes <- campos_requeridos %in% names(resultado_modificado)

multinivel <- as.data.frame(resultado_modificado$multilevel_modules)
membership_final <- membership(resultado_modificado)

# Alinear membership() con node_id cuando existen nombres
if (!is.null(names(membership_final))) {
  indices <- match(as.character(multinivel$node_id), names(membership_final))
  membership_final <- membership_final[indices]
}

final_module_ok <- all(
  as.integer(multinivel$final_module) ==
    as.integer(membership_final)
)

modularity_ok <- tryCatch(
  {
    valor <- modularity(resultado_modificado)
    is.numeric(valor) && length(valor) == 1L && !is.na(valor)
  },
  error = function(e) FALSE
)

salida_dir <- file.path(base_dir, "figuras")
dir.create(salida_dir, showWarnings = FALSE, recursive = TRUE)

archivo_salida <- file.path(
  salida_dir,
  "figura_15_campos_resumen_multinivel.txt"
)

capture.output(
  {
    cat("VERIFICACIÃ“N DE LA SALIDA MULTINIVEL DEL PROTOTIPO\n")
    cat("=================================================\n\n")

    cat("VersiÃ³n de igraph:", as.character(packageVersion("igraph")), "\n")
    cat("Clase del objeto:", paste(class(resultado_modificado), collapse = ", "), "\n\n")

    cat("Campos incorporados:\n")
    for (i in seq_along(campos_requeridos)) {
      cat(
        sprintf(
          "  %-20s %s\n",
          campos_requeridos[i],
          if (campos_presentes[i]) "PRESENTE" else "AUSENTE"
        )
      )
    }

    cat("\nValores de resumen jerÃ¡rquico:\n")
    cat("  num_levels       =", resultado_modificado$num_levels, "\n")
    cat("  num_top_modules  =", resultado_modificado$num_top_modules, "\n")
    cat("  top_module_level =", resultado_modificado$top_module_level, "\n")
    cat("  max_tree_depth   =", resultado_modificado$max_tree_depth, "\n")

    cat("\nCompatibilidad:\n")
    cat("  final_module coincide con membership() =", final_module_ok, "\n")
    cat("  modularity() continÃºa disponible       =", modularity_ok, "\n")
    cat(
      "  hierarchical_codelength incorporado    =",
      "hierarchical_codelength" %in% names(resultado_modificado),
      "\n"
    )
  },
  file = archivo_salida
)

cat("Archivo generado correctamente:\n")
cat(archivo_salida, "\n")
