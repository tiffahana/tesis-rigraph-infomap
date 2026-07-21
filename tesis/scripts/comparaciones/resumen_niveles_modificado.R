cat("====================================\n")
cat("RESUMEN DE NIVELES - IGRAPH MODIFICADO\n")
cat("====================================\n\n")

base_dir <- normalizePath(
  ".",
  mustWork = TRUE
)

out_dir <- file.path(base_dir, "tesis", "resultados", "comparaciones")

redes <- c(
  "Zachary",
  "USairports",
  "enron",
  "UKfaculty",
  "yeast"
)

resumen_niveles <- data.frame(
  red = character(),
  columna = character(),
  modulos_distintos = integer(),
  stringsAsFactors = FALSE
)

resumen_red <- data.frame(
  red = character(),
  primer_nivel_no_trivial = character(),
  modulos_en_primer_nivel_no_trivial = integer(),
  comunidades_finales = integer(),
  stringsAsFactors = FALSE
)

for (red in redes) {
  archivo <- file.path(
    out_dir,
    paste0(red, "_multilevel_modules_modificado.csv")
  )

  cat("\n------------------------------------\n")
  cat("Red:", red, "\n")
  cat("------------------------------------\n")

  if (!file.exists(archivo)) {
    warning(paste("No existe el archivo:", archivo))
    next
  }

  df <- read.csv(archivo)

  cols_nivel <- grep("^level_", names(df), value = TRUE)

  cols_analisis <- c(cols_nivel, "final_module")

  for (col in cols_analisis) {
    n_modulos <- length(unique(df[[col]]))

    resumen_niveles <- rbind(
      resumen_niveles,
      data.frame(
        red = red,
        columna = col,
        modulos_distintos = n_modulos,
        stringsAsFactors = FALSE
      )
    )

    cat(col, ":", n_modulos, "modulos distintos\n")
  }

  primer_no_trivial <- NA
  modulos_primer_no_trivial <- NA

  for (col in cols_nivel) {
    n_modulos <- length(unique(df[[col]]))

    if (n_modulos > 1) {
      primer_no_trivial <- col
      modulos_primer_no_trivial <- n_modulos
      break
    }
  }

  comunidades_finales <- length(unique(df$final_module))

  resumen_red <- rbind(
    resumen_red,
    data.frame(
      red = red,
      primer_nivel_no_trivial = primer_no_trivial,
      modulos_en_primer_nivel_no_trivial = modulos_primer_no_trivial,
      comunidades_finales = comunidades_finales,
      stringsAsFactors = FALSE
    )
  )
}

cat("\n====================================\n")
cat("RESUMEN POR NIVEL\n")
cat("====================================\n\n")
print(resumen_niveles)

cat("\n====================================\n")
cat("RESUMEN POR RED\n")
cat("====================================\n\n")
print(resumen_red)

write.csv(
  resumen_niveles,
  file.path(out_dir, "resumen_niveles_modificado.csv"),
  row.names = FALSE
)

write.csv(
  resumen_red,
  file.path(out_dir, "resumen_primer_nivel_no_trivial_modificado.csv"),
  row.names = FALSE
)

capture.output(
  {
    cat("====================================\n")
    cat("RESUMEN DE NIVELES - IGRAPH MODIFICADO\n")
    cat("====================================\n\n")

    cat("RESUMEN POR NIVEL\n\n")
    print(resumen_niveles)

    cat("\nRESUMEN POR RED\n\n")
    print(resumen_red)
  },
  file = file.path(out_dir, "resumen_niveles_modificado.txt")
)

cat("\nArchivos generados:\n")
cat(file.path(out_dir, "resumen_niveles_modificado.csv"), "\n")
cat(file.path(out_dir, "resumen_primer_nivel_no_trivial_modificado.csv"), "\n")
cat(file.path(out_dir, "resumen_niveles_modificado.txt"), "\n")

cat("\nFIN RESUMEN DE NIVELES\n")