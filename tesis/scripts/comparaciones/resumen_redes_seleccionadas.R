cat("====================================\n")
cat("RESUMEN DE REDES SELECCIONADAS\n")
cat("====================================\n\n")

library(igraph)

if (!requireNamespace("igraphdata", quietly = TRUE)) {
  stop("Falta instalar igraphdata.")
}

base_dir <- normalizePath(
  ".",
  mustWork = TRUE
)

out_dir <- file.path(base_dir, "tesis", "resultados", "comparaciones")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

cargar_red <- function(nombre) {
  if (nombre == "Zachary") {
    return(make_graph("Zachary"))
  }

  env <- new.env()
  data(list = nombre, package = "igraphdata", envir = env)
  get(nombre, envir = env)
}

resumir_red <- function(nombre) {
  cat("\n------------------------------------\n")
  cat("Red:", nombre, "\n")
  cat("------------------------------------\n")

  g <- cargar_red(nombre)

  tiene_pesos <- "weight" %in% edge_attr_names(g)

  resumen <- data.frame(
    red = nombre,
    nodos = vcount(g),
    aristas = ecount(g),
    dirigida = is_directed(g),
    ponderada = tiene_pesos,
    componentes = components(as.undirected(g))$no,
    stringsAsFactors = FALSE
  )

  print(resumen)

  return(resumen)
}

redes <- c(
  "Zachary",
  "USairports",
  "enron",
  "UKfaculty",
  "yeast"
)

resumen_total <- do.call(
  rbind,
  lapply(redes, resumir_red)
)

cat("\n====================================\n")
cat("RESUMEN FINAL\n")
cat("====================================\n\n")

print(resumen_total)

write.csv(
  resumen_total,
  file.path(out_dir, "resumen_redes_seleccionadas.csv"),
  row.names = FALSE
)

capture.output(
  {
    cat("====================================\n")
    cat("RESUMEN DE REDES SELECCIONADAS\n")
    cat("====================================\n\n")
    print(resumen_total)
  },
  file = file.path(out_dir, "resumen_redes_seleccionadas.txt")
)

cat("\nArchivos generados:\n")
cat(file.path(out_dir, "resumen_redes_seleccionadas.csv"), "\n")
cat(file.path(out_dir, "resumen_redes_seleccionadas.txt"), "\n")

cat("\nFIN RESUMEN\n")