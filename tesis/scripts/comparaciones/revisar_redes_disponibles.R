cat("====================================\n")
cat("REVISION DE REDES DISPONIBLES\n")
cat("====================================\n\n")

library(igraph)

if (!requireNamespace("igraphdata", quietly = TRUE)) {
  stop("Falta instalar igraphdata. Ejecuta install.packages('igraphdata').")
}

cat("Datasets disponibles en igraphdata:\n\n")

datasets <- data(package = "igraphdata")$results
print(datasets[, c("Item", "Title")])

cat("\n====================================\n")
cat("REVISION DE REDES CANDIDATAS\n")
cat("====================================\n\n")

candidatas <- c(
  "USairports",
  "karate",
  "dolphins",
  "football",
  "lesmis",
  "enron",
  "UKfaculty",
  "yeast",
  "foodwebs",
  "immuno",
  "Koenigsberg",
  "rfid"
)

for (red in candidatas) {
  env <- new.env()

  disponible <- tryCatch(
    {
      suppressWarnings(
        data(list = red, package = "igraphdata", envir = env)
      )
      exists(red, envir = env)
    },
    error = function(e) FALSE
  )

  cat(red, ":", disponible, "\n")
}

cat("\n====================================\n")
cat("REVISION DE ZACHARY DESDE IGRAPH\n")
cat("====================================\n\n")

zachary_ok <- tryCatch(
  {
    g <- make_graph("Zachary")
    TRUE
  },
  error = function(e) FALSE
)

cat("Zachary en igraph:", zachary_ok, "\n")