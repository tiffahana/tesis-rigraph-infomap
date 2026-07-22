# Resume el barrido piloto LFR jerarquico.
#
# Entrada:
#   tesis/resultados/comparaciones/barrido_lfr_piloto/resultados_por_red.csv
#
# Salidas:
#   - resumen_por_configuracion.csv
#   - resumen_global.csv
#   - diferencias_metodos.csv
#   - resumen_barrido_lfr.txt
#   - figuras PNG de NMI por configuracion
#
# Ejecutar desde la raiz del repositorio:
#   Rscript tesis/scripts/comparaciones/31_resumir_barrido_lfr.R

cat("============================================================\n")
cat("RESUMEN DEL BARRIDO PILOTO LFR\n")
cat("============================================================\n\n")

repo_dir <- normalizePath(".", winslash = "/", mustWork = TRUE)

if (!file.exists(file.path(repo_dir, "DESCRIPTION")) ||
    !dir.exists(file.path(repo_dir, "tesis"))) {
  stop("Ejecute este script desde la raiz del repositorio.")
}

input_path <- file.path(
  repo_dir,
  "tesis",
  "resultados",
  "comparaciones",
  "barrido_lfr_piloto",
  "resultados_por_red.csv"
)

out_dir <- file.path(
  repo_dir,
  "tesis",
  "resultados",
  "comparaciones",
  "barrido_lfr_piloto",
  "resumen"
)

fig_dir <- file.path(out_dir, "figuras")

dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(input_path)) {
  stop("No se encontro: ", input_path)
}

datos <- read.csv(
  input_path,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

required <- c(
  "configuracion",
  "mu1",
  "mu2",
  "misma_micro",
  "semilla_red",
  "status",
  "modified_top_vs_truth_macro_nmi",
  "modified_top_vs_truth_macro_ari",
  "modified_final_vs_truth_micro_nmi",
  "modified_final_vs_truth_micro_ari",
  "official_top_vs_truth_macro_nmi",
  "official_top_vs_truth_macro_ari",
  "official_final_vs_truth_micro_nmi",
  "official_final_vs_truth_micro_ari",
  "modified_top_vs_official_top_nmi",
  "modified_top_vs_official_top_ari",
  "modified_final_vs_official_final_nmi",
  "modified_final_vs_official_final_ari"
)

missing <- setdiff(required, names(datos))

if (length(missing) > 0L) {
  stop(
    "Faltan columnas requeridas: ",
    paste(missing, collapse = ", ")
  )
}

datos_ok <- datos[datos$status == "OK", , drop = FALSE]

if (nrow(datos_ok) == 0L) {
  stop("No hay ejecuciones con status OK.")
}

metric_columns <- c(
  modified_top_vs_truth_macro_nmi = "Prototipo vs verdad macro (NMI)",
  modified_top_vs_truth_macro_ari = "Prototipo vs verdad macro (ARI)",
  modified_final_vs_truth_micro_nmi = "Prototipo vs verdad micro (NMI)",
  modified_final_vs_truth_micro_ari = "Prototipo vs verdad micro (ARI)",
  official_top_vs_truth_macro_nmi = "Oficial vs verdad macro (NMI)",
  official_top_vs_truth_macro_ari = "Oficial vs verdad macro (ARI)",
  official_final_vs_truth_micro_nmi = "Oficial vs verdad micro (NMI)",
  official_final_vs_truth_micro_ari = "Oficial vs verdad micro (ARI)",
  modified_top_vs_official_top_nmi = "Prototipo vs oficial superior (NMI)",
  modified_top_vs_official_top_ari = "Prototipo vs oficial superior (ARI)",
  modified_final_vs_official_final_nmi = "Prototipo vs oficial final (NMI)",
  modified_final_vs_official_final_ari = "Prototipo vs oficial final (ARI)"
)

summarise_vector <- function(x) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]

  if (length(x) == 0L) {
    return(c(
      n = 0,
      media = NA_real_,
      mediana = NA_real_,
      sd = NA_real_,
      minimo = NA_real_,
      maximo = NA_real_
    ))
  }

  c(
    n = length(x),
    media = mean(x),
    mediana = median(x),
    sd = if (length(x) > 1L) stats::sd(x) else 0,
    minimo = min(x),
    maximo = max(x)
  )
}

build_summary <- function(data, group_cols) {
  groups <- interaction(
    data[group_cols],
    drop = TRUE,
    lex.order = TRUE
  )

  split_data <- split(data, groups)

  rows <- lapply(split_data, function(group) {
    base <- group[1, group_cols, drop = FALSE]

    stats_list <- lapply(names(metric_columns), function(metric) {
      stats <- summarise_vector(group[[metric]])
      names(stats) <- paste0(metric, "_", names(stats))
      as.list(stats)
    })

    cbind(
      base,
      as.data.frame(
        do.call(c, stats_list),
        check.names = FALSE
      ),
      stringsAsFactors = FALSE
    )
  })

  result <- do.call(rbind, rows)
  rownames(result) <- NULL
  result
}

resumen_config <- build_summary(
  datos_ok,
  c("configuracion", "mu1", "mu2", "misma_micro")
)

resumen_global <- build_summary(
  transform(datos_ok, grupo = "global"),
  "grupo"
)

# Diferencias directas entre el prototipo y la implementacion oficial.
diferencias <- transform(
  datos_ok,
  diferencia_nmi_macro =
    modified_top_vs_truth_macro_nmi -
    official_top_vs_truth_macro_nmi,
  diferencia_ari_macro =
    modified_top_vs_truth_macro_ari -
    official_top_vs_truth_macro_ari,
  diferencia_nmi_micro =
    modified_final_vs_truth_micro_nmi -
    official_final_vs_truth_micro_nmi,
  diferencia_ari_micro =
    modified_final_vs_truth_micro_ari -
    official_final_vs_truth_micro_ari
)

resumen_diferencias <- aggregate(
  cbind(
    diferencia_nmi_macro,
    diferencia_ari_macro,
    diferencia_nmi_micro,
    diferencia_ari_micro
  ) ~ configuracion + mu1 + mu2 + misma_micro,
  data = diferencias,
  FUN = mean
)

write.csv(
  resumen_config,
  file.path(out_dir, "resumen_por_configuracion.csv"),
  row.names = FALSE
)

write.csv(
  resumen_global,
  file.path(out_dir, "resumen_global.csv"),
  row.names = FALSE
)

write.csv(
  resumen_diferencias,
  file.path(out_dir, "diferencias_metodos.csv"),
  row.names = FALSE
)

# Tabla compacta para lectura y memoria.
tabla_compacta <- data.frame(
  configuracion = resumen_config$configuracion,
  mu1 = resumen_config$mu1,
  mu2 = resumen_config$mu2,
  misma_micro = resumen_config$misma_micro,
  prototipo_macro_nmi_media =
    resumen_config$modified_top_vs_truth_macro_nmi_media,
  prototipo_micro_nmi_media =
    resumen_config$modified_final_vs_truth_micro_nmi_media,
  oficial_macro_nmi_media =
    resumen_config$official_top_vs_truth_macro_nmi_media,
  oficial_micro_nmi_media =
    resumen_config$official_final_vs_truth_micro_nmi_media,
  prototipo_vs_oficial_superior_nmi_media =
    resumen_config$modified_top_vs_official_top_nmi_media,
  prototipo_vs_oficial_final_nmi_media =
    resumen_config$modified_final_vs_official_final_nmi_media,
  stringsAsFactors = FALSE
)

write.csv(
  tabla_compacta,
  file.path(out_dir, "tabla_compacta_nmi.csv"),
  row.names = FALSE
)

# Funciones de graficacion base R.
plot_heatmap <- function(
    data,
    value_col,
    title,
    file_name,
    label_digits = 3
) {
  mu1_values <- sort(unique(data$mu1))
  mu2_values <- sort(unique(data$mu2))

  matrix_values <- matrix(
    NA_real_,
    nrow = length(mu2_values),
    ncol = length(mu1_values),
    dimnames = list(
      paste0("mu2=", format(mu2_values, nsmall = 2)),
      paste0("mu1=", format(mu1_values, nsmall = 2))
    )
  )

  for (i in seq_len(nrow(data))) {
    row_index <- match(data$mu2[i], mu2_values)
    col_index <- match(data$mu1[i], mu1_values)
    matrix_values[row_index, col_index] <- data[[value_col]][i]
  }

  png(
    file.path(fig_dir, file_name),
    width = 1400,
    height = 1050,
    res = 160
  )

  par(mar = c(6, 7, 5, 2))

  image(
    x = seq_along(mu1_values),
    y = seq_along(mu2_values),
    z = t(matrix_values),
    axes = FALSE,
    xlab = expression(mu[1]),
    ylab = expression(mu[2]),
    main = title,
    zlim = c(0, 1)
  )

  axis(
    1,
    at = seq_along(mu1_values),
    labels = format(mu1_values, nsmall = 2)
  )

  axis(
    2,
    at = seq_along(mu2_values),
    labels = format(mu2_values, nsmall = 2),
    las = 1
  )

  for (row_index in seq_along(mu2_values)) {
    for (col_index in seq_along(mu1_values)) {
      value <- matrix_values[row_index, col_index]

      if (is.finite(value)) {
        text(
          col_index,
          row_index,
          labels = format(
            round(value, label_digits),
            nsmall = label_digits
          ),
          cex = 1.2
        )
      }
    }
  }

  box()
  dev.off()
}

plot_heatmap(
  tabla_compacta,
  "prototipo_macro_nmi_media",
  "Prototipo: recuperación de macrocomunidades",
  "nmi_prototipo_macro.png"
)

plot_heatmap(
  tabla_compacta,
  "prototipo_micro_nmi_media",
  "Prototipo: recuperación de microcomunidades",
  "nmi_prototipo_micro.png"
)

plot_heatmap(
  tabla_compacta,
  "oficial_macro_nmi_media",
  "Infomap oficial: recuperación de macrocomunidades",
  "nmi_oficial_macro.png"
)

plot_heatmap(
  tabla_compacta,
  "oficial_micro_nmi_media",
  "Infomap oficial: recuperación de microcomunidades",
  "nmi_oficial_micro.png"
)

# Identificacion de mejores y peores escenarios.
best_row <- function(data, column) {
  data[which.max(data[[column]]), , drop = FALSE]
}

worst_row <- function(data, column) {
  data[which.min(data[[column]]), , drop = FALSE]
}

best_macro_mod <- best_row(
  tabla_compacta,
  "prototipo_macro_nmi_media"
)

worst_macro_mod <- worst_row(
  tabla_compacta,
  "prototipo_macro_nmi_media"
)

best_micro_mod <- best_row(
  tabla_compacta,
  "prototipo_micro_nmi_media"
)

worst_micro_mod <- worst_row(
  tabla_compacta,
  "prototipo_micro_nmi_media"
)

summary_txt <- file.path(out_dir, "resumen_barrido_lfr.txt")

capture.output(
  {
    cat("============================================================\n")
    cat("RESUMEN DEL BARRIDO PILOTO LFR\n")
    cat("============================================================\n\n")

    cat("Redes analizadas:", nrow(datos_ok), "\n")
    cat(
      "Configuraciones:",
      length(unique(datos_ok$configuracion)),
      "\n"
    )
    cat(
      "Semillas por configuracion:",
      paste(
        sort(unique(table(datos_ok$configuracion))),
        collapse = ", "
      ),
      "\n\n"
    )

    cat("TABLA COMPACTA DE NMI MEDIO\n\n")
    print(tabla_compacta)

    cat("\nPROMEDIOS GLOBALES\n\n")
    print(resumen_global)

    cat("\nDIFERENCIAS MEDIAS PROTOTIPO - OFICIAL\n\n")
    print(resumen_diferencias)

    cat("\nMEJORES Y PEORES ESCENARIOS DEL PROTOTIPO\n\n")

    cat("Macro, mejor:\n")
    print(best_macro_mod)

    cat("\nMacro, peor:\n")
    print(worst_macro_mod)

    cat("\nMicro, mejor:\n")
    print(best_micro_mod)

    cat("\nMicro, peor:\n")
    print(worst_micro_mod)

    cat("\nLECTURA METODOLOGICA\n\n")
    cat(
      "- mu1 controla la proporcion de enlaces entre ",
      "macrocomunidades y afecta principalmente el nivel superior.\n",
      sep = ""
    )
    cat(
      "- mu1 + mu2 reduce la fraccion de enlaces internos a la ",
      "microcomunidad y afecta principalmente el nivel final.\n",
      sep = ""
    )
    cat(
      "- Los resultados deben interpretarse como piloto porque ",
      "hay tres semillas por configuracion.\n",
      sep = ""
    )
  },
  file = summary_txt
)

cat("Redes analizadas:", nrow(datos_ok), "\n")
cat("Configuraciones:", nrow(tabla_compacta), "\n")
cat("Resumen:", summary_txt, "\n")
cat("Tabla compacta:", file.path(out_dir, "tabla_compacta_nmi.csv"), "\n")
cat("Figuras:", fig_dir, "\n")
cat("\nResumen generado correctamente.\n")
