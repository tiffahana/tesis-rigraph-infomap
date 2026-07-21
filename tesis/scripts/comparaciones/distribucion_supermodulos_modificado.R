cat("====================================\n")
cat("DISTRIBUCION DE SUPERMODULOS - IGRAPH MODIFICADO\n")
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

tabla_supermodulos <- data.frame(
  red = character(),
  nivel_superior_usado = character(),
  supermodulo = integer(),
  nodos_en_supermodulo = integer(),
  comunidades_finales_contenidas = integer(),
  stringsAsFactors = FALSE
)

resumen_redes <- data.frame(
  red = character(),
  nivel_superior_usado = character(),
  cantidad_supermodulos = integer(),
  comunidades_finales = integer(),
  presenta_jerarquia_intermedia = logical(),
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

  if (!"final_module" %in% names(df)) {
    warning(paste("La red", red, "no tiene columna final_module."))
    next
  }

  comunidades_finales <- length(unique(df$final_module))

  # Buscar primer nivel no trivial.
  # Es decir, el primer level_i que tenga mÃ¡s de un mÃ³dulo.
  nivel_superior <- NA

  for (col in cols_nivel) {
    if (length(unique(df[[col]])) > 1) {
      nivel_superior <- col
      break
    }
  }

  if (is.na(nivel_superior)) {
    cat("No se encontro nivel superior no trivial.\n")
    next
  }

  cantidad_supermodulos <- length(unique(df[[nivel_superior]]))

  presenta_jerarquia_intermedia <- cantidad_supermodulos < comunidades_finales

  cat("Nivel superior usado:", nivel_superior, "\n")
  cat("Cantidad de supermodulos:", cantidad_supermodulos, "\n")
  cat("Comunidades finales:", comunidades_finales, "\n")
  cat("Presenta jerarquia intermedia:", presenta_jerarquia_intermedia, "\n\n")

  resumen_redes <- rbind(
    resumen_redes,
    data.frame(
      red = red,
      nivel_superior_usado = nivel_superior,
      cantidad_supermodulos = cantidad_supermodulos,
      comunidades_finales = comunidades_finales,
      presenta_jerarquia_intermedia = presenta_jerarquia_intermedia,
      stringsAsFactors = FALSE
    )
  )

  supermodulos <- sort(unique(df[[nivel_superior]]))

  for (sm in supermodulos) {
    sub <- df[df[[nivel_superior]] == sm, ]

    nodos_en_supermodulo <- nrow(sub)
    comunidades_contenidas <- length(unique(sub$final_module))

    tabla_supermodulos <- rbind(
      tabla_supermodulos,
      data.frame(
        red = red,
        nivel_superior_usado = nivel_superior,
        supermodulo = sm,
        nodos_en_supermodulo = nodos_en_supermodulo,
        comunidades_finales_contenidas = comunidades_contenidas,
        stringsAsFactors = FALSE
      )
    )

    cat(
      "Supermodulo", sm,
      "| nodos:", nodos_en_supermodulo,
      "| comunidades finales contenidas:", comunidades_contenidas,
      "\n"
    )
  }
}

cat("\n====================================\n")
cat("RESUMEN POR RED\n")
cat("====================================\n\n")
print(resumen_redes)

cat("\n====================================\n")
cat("TABLA DE SUPERMODULOS\n")
cat("====================================\n\n")
print(tabla_supermodulos)

write.csv(
  resumen_redes,
  file.path(out_dir, "resumen_supermodulos_modificado.csv"),
  row.names = FALSE
)

write.csv(
  tabla_supermodulos,
  file.path(out_dir, "tabla_supermodulos_modificado.csv"),
  row.names = FALSE
)

capture.output(
  {
    cat("====================================\n")
    cat("DISTRIBUCION DE SUPERMODULOS - IGRAPH MODIFICADO\n")
    cat("====================================\n\n")

    cat("RESUMEN POR RED\n\n")
    print(resumen_redes)

    cat("\nTABLA DE SUPERMODULOS\n\n")
    print(tabla_supermodulos)

    cat("\nREDES CON JERARQUIA INTERMEDIA\n\n")
    print(resumen_redes[resumen_redes$presenta_jerarquia_intermedia, ])
  },
  file = file.path(out_dir, "distribucion_supermodulos_modificado.txt")
)

cat("\nArchivos generados:\n")
cat(file.path(out_dir, "resumen_supermodulos_modificado.csv"), "\n")
cat(file.path(out_dir, "tabla_supermodulos_modificado.csv"), "\n")
cat(file.path(out_dir, "distribucion_supermodulos_modificado.txt"), "\n")

cat("\nFIN DISTRIBUCION DE SUPERMODULOS\n")