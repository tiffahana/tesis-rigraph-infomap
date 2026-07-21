# Figura 17: muestra de multilevel_modules en la red LFR
# Tesis de Daniela Salinas Castro
#
# La salida contiene:
# 1) las primeras 10 filas del archivo;
# 2) otras 10 filas seleccionadas para representar los seis mÃ³dulos superiores.

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

archivo_csv <- file.path(
  base_dir,
  "datos", "lfr", "jerarquia_reforzada",
  "multilevel_modules_lfr_modificado.csv"
)

if (!file.exists(archivo_csv)) {
  stop(
    paste0(
      "No se encontrÃ³ el archivo:\n",
      archivo_csv,
      "\nRevisa que la carpeta principal del proyecto sea correcta."
    )
  )
}

multilevel_lfr <- read.csv(
  archivo_csv,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

columnas_esperadas <- c(
  "node_id",
  "level_1",
  "level_2",
  "level_3",
  "final_module"
)

if (!all(columnas_esperadas %in% names(multilevel_lfr))) {
  stop(
    paste0(
      "El archivo no contiene todas las columnas esperadas.\n",
      "Columnas encontradas: ",
      paste(names(multilevel_lfr), collapse = ", ")
    )
  )
}

# -------------------------------------------------------------------
# Muestra 1: primeras 10 filas
# -------------------------------------------------------------------
muestra_inicial <- multilevel_lfr[
  1:10,
  columnas_esperadas,
  drop = FALSE
]

# -------------------------------------------------------------------
# Muestra 2: 10 filas que evidencian la jerarquÃ­a
#
# Se incluyen representantes de los seis mÃ³dulos superiores:
# - 2 nodos de level_2 = 1, 2, 3 y 4
# - 1 nodo de level_2 = 5 y 6
# Total: 10 filas
# -------------------------------------------------------------------
grupos_superiores <- split(
  multilevel_lfr,
  multilevel_lfr$level_2
)

modulos_esperados <- sort(unique(multilevel_lfr$level_2))

if (length(modulos_esperados) < 6) {
  stop(
    paste0(
      "Se esperaban al menos 6 mÃ³dulos superiores, pero se encontraron ",
      length(modulos_esperados),
      "."
    )
  )
}

cantidad_por_modulo <- c(2, 2, 2, 2, 1, 1)

muestra_jerarquia <- do.call(
  rbind,
  Map(
    function(modulo, cantidad) {
      head(
        grupos_superiores[[as.character(modulo)]],
        cantidad
      )
    },
    modulos_esperados[1:6],
    cantidad_por_modulo
  )
)

muestra_jerarquia <- muestra_jerarquia[
  ,
  columnas_esperadas,
  drop = FALSE
]

row.names(muestra_jerarquia) <- NULL

# Verificaciones para evitar una figura incorrecta
if (nrow(muestra_inicial) != 10) {
  stop("La muestra inicial no contiene exactamente 10 filas.")
}

if (nrow(muestra_jerarquia) != 10) {
  stop("La muestra jerÃ¡rquica no contiene exactamente 10 filas.")
}

if (length(unique(muestra_jerarquia$level_2)) != 6) {
  stop("La muestra jerÃ¡rquica no representa los seis mÃ³dulos superiores.")
}

# -------------------------------------------------------------------
# Generar salida limpia para abrir en VS Code y capturar
# -------------------------------------------------------------------
salida_dir <- file.path(base_dir, "figuras")
dir.create(salida_dir, showWarnings = FALSE, recursive = TRUE)

archivo_salida <- file.path(
  salida_dir,
  "figura_17_muestras_multilevel_modules_lfr.txt"
)

capture.output(
  {
    cat("MUESTRAS DE LA ESTRUCTURA MULTILEVEL_MODULES EN LA RED LFR\n")
    cat("==========================================================\n\n")

    cat("Dimensiones del resultado completo:\n")
    cat("  Nodos    =", nrow(multilevel_lfr), "\n")
    cat("  Columnas =", ncol(multilevel_lfr), "\n")
    cat(
      "  MÃ³dulos superiores distintos en level_2 =",
      length(unique(multilevel_lfr$level_2)),
      "\n\n"
    )

    cat("MUESTRA 1: PRIMERAS 10 FILAS\n")
    cat("----------------------------\n")
    print(muestra_inicial, row.names = FALSE)

    cat("\n\n")
    cat("MUESTRA 2: 10 FILAS REPRESENTATIVAS DE LA JERARQUÃA\n")
    cat("--------------------------------------------------\n")
    print(muestra_jerarquia, row.names = FALSE)

    cat("\nInterpretaciÃ³n:\n")
    cat("  level_1      = raÃ­z comÃºn de la jerarquÃ­a\n")
    cat("  level_2      = mÃ³dulos superiores (1 a 6)\n")
    cat("  level_3      = mÃ³dulos inferiores\n")
    cat("  final_module = comunidad final compatible con membership()\n")
  },
  file = archivo_salida
)

cat("Archivo generado correctamente:\n")
cat(archivo_salida, "\n")
