# Figura 21: resultado multinivel de Zachary Karate Club
# Tesis de Daniela Salinas Castro
#
# Requisito:
# Ejecutar este script con la versiÃ³n local modificada de igraph,
# que incorpora el campo multilevel_modules en cluster_infomap().

library(igraph)

set.seed(1234)

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
salida_dir <- file.path(base_dir, "figuras")
dir.create(salida_dir, showWarnings = FALSE, recursive = TRUE)

archivo_png <- file.path(
  salida_dir,
  "figura_21_resultado_multinivel_zachary.png"
)

archivo_txt <- file.path(
  salida_dir,
  "figura_21_resumen_zachary.txt"
)

# -------------------------------------------------------------------
# 1. Crear la red Zachary Karate Club
# -------------------------------------------------------------------
grafo_zachary <- make_graph("Zachary")
V(grafo_zachary)$name <- as.character(seq_len(vcount(grafo_zachary)))

# -------------------------------------------------------------------
# 2. Ejecutar Infomap con la versiÃ³n modificada
# -------------------------------------------------------------------
resultado_zachary <- cluster_infomap(
  grafo_zachary,
  nb.trials = 100
)

if (is.null(resultado_zachary$multilevel_modules)) {
  stop(
    paste0(
      "El resultado no contiene el campo multilevel_modules.\n",
      "Verifica que VS Code estÃ© usando la versiÃ³n local modificada de igraph."
    )
  )
}

multinivel <- as.data.frame(
  resultado_zachary$multilevel_modules,
  stringsAsFactors = FALSE
)

if (!"final_module" %in% names(multinivel)) {
  stop("No se encontrÃ³ la columna final_module.")
}

# -------------------------------------------------------------------
# 3. Detectar el primer nivel jerÃ¡rquico no trivial
# -------------------------------------------------------------------
columnas_nivel <- grep(
  "^level_[0-9]+$",
  names(multinivel),
  value = TRUE
)

if (length(columnas_nivel) > 0) {
  numero_nivel <- as.integer(sub("level_", "", columnas_nivel))
  columnas_nivel <- columnas_nivel[order(numero_nivel)]
}

niveles_no_triviales <- columnas_nivel[
  vapply(
    multinivel[columnas_nivel],
    function(x) length(unique(x)) > 1,
    logical(1)
  )
]

if (length(niveles_no_triviales) > 0) {
  nivel_superior <- niveles_no_triviales[1]
  modulos_superiores <- multinivel[[nivel_superior]]
} else {
  nivel_superior <- "final_module"
  modulos_superiores <- multinivel$final_module
}

comunidades_finales <- multinivel$final_module

# -------------------------------------------------------------------
# 4. Verificar compatibilidad con membership()
# -------------------------------------------------------------------
coincide_membership <- all(
  comunidades_finales == membership(resultado_zachary)
)

# -------------------------------------------------------------------
# 5. Preparar colores y disposiciÃ³n comÃºn para ambos paneles
# -------------------------------------------------------------------
crear_colores <- function(modulos) {
  indices <- as.integer(factor(modulos))
  paleta <- grDevices::hcl.colors(
    n = max(indices),
    palette = "Set 2"
  )
  paleta[indices]
}

colores_superiores <- crear_colores(modulos_superiores)
colores_finales <- crear_colores(comunidades_finales)

set.seed(1234)
disposicion <- layout_with_fr(
  grafo_zachary,
  niter = 2000
)

# -------------------------------------------------------------------
# 6. Generar figura con dos paneles
# -------------------------------------------------------------------
png(
  filename = archivo_png,
  width = 2400,
  height = 1250,
  res = 200
)

par(
  mfrow = c(1, 2),
  mar = c(2, 2, 5, 2),
  oma = c(1, 1, 3, 1)
)

plot(
  grafo_zachary,
  layout = disposicion,
  vertex.color = colores_superiores,
  vertex.frame.color = "gray25",
  vertex.label.color = "black",
  vertex.label.cex = 0.75,
  vertex.size = 16,
  edge.color = "gray70",
  edge.width = 1.2,
  main = paste0(
    "MÃ³dulos superiores (",
    nivel_superior,
    ")\n",
    length(unique(modulos_superiores)),
    " mÃ³dulos"
  )
)

plot(
  grafo_zachary,
  layout = disposicion,
  vertex.color = colores_finales,
  vertex.frame.color = "gray25",
  vertex.label.color = "black",
  vertex.label.cex = 0.75,
  vertex.size = 16,
  edge.color = "gray70",
  edge.width = 1.2,
  main = paste0(
    "Comunidades finales\n",
    length(unique(comunidades_finales)),
    " comunidades"
  )
)

mtext(
  "Resultado multinivel de Zachary Karate Club",
  outer = TRUE,
  side = 3,
  line = 1,
  cex = 1.35,
  font = 2
)

dev.off()

# -------------------------------------------------------------------
# 7. Guardar un resumen textual para comprobar los valores
# -------------------------------------------------------------------
capture.output(
  {
    cat("RESULTADO MULTINIVEL DE ZACHARY KARATE CLUB\n")
    cat("==========================================\n\n")
    cat("Nodos:", vcount(grafo_zachary), "\n")
    cat("Aristas:", ecount(grafo_zachary), "\n")
    cat("Nivel superior representado:", nivel_superior, "\n")
    cat(
      "Cantidad de mÃ³dulos superiores:",
      length(unique(modulos_superiores)),
      "\n"
    )
    cat(
      "Cantidad de comunidades finales:",
      length(unique(comunidades_finales)),
      "\n"
    )
    cat(
      "final_module coincide con membership():",
      coincide_membership,
      "\n\n"
    )
    cat("Columnas disponibles en multilevel_modules:\n")
    cat(paste(names(multinivel), collapse = ", "), "\n")
  },
  file = archivo_txt
)

cat("Figura generada correctamente:\n")
cat(archivo_png, "\n\n")
cat("Resumen generado correctamente:\n")
cat(archivo_txt, "\n")
