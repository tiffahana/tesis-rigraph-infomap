library(igraph)

cat("VersiÃ³n de igraph:", as.character(packageVersion("igraph")), "\n")
cat("Ruta de igraph:", find.package("igraph"), "\n")





# ============================================================
# IMPORTAR Y VALIDAR LA RED LFR
# ============================================================

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

ruta_lfr <- file.path(
  repo_dir,
  "tesis",
  "datos",
  "lfr",
  "inicial"
)

archivo_red   <- file.path(ruta_lfr, "network.dat")
archivo_micro <- file.path(ruta_lfr, "community_first_level.dat")
archivo_macro <- file.path(ruta_lfr, "community_second_level.dat")

# Comprobar que existan los archivos
stopifnot(file.exists(archivo_red))
stopifnot(file.exists(archivo_micro))
stopifnot(file.exists(archivo_macro))

# Leer las aristas
aristas_raw <- read.table(
  archivo_red,
  header = FALSE,
  col.names = c("from", "to")
)

# network.dat contiene las conexiones en ambos sentidos.
# Se ordenan los extremos y se eliminan duplicados.
aristas <- data.frame(
  from = pmin(aristas_raw$from, aristas_raw$to),
  to   = pmax(aristas_raw$from, aristas_raw$to)
)

aristas <- unique(aristas)
aristas <- aristas[aristas$from != aristas$to, ]

# Leer las comunidades conocidas
micro_conocidas <- read.table(
  archivo_micro,
  header = FALSE,
  col.names = c("node_id", "micro_comunidad")
)

macro_conocidas <- read.table(
  archivo_macro,
  header = FALSE,
  col.names = c("node_id", "macro_comunidad")
)

micro_conocidas <- micro_conocidas[
  order(micro_conocidas$node_id),
]

macro_conocidas <- macro_conocidas[
  order(macro_conocidas$node_id),
]

# Crear el grafo con los 1000 nodos
vertices <- data.frame(
  name = as.character(seq_len(1000))
)

aristas$from <- as.character(aristas$from)
aristas$to   <- as.character(aristas$to)

g_lfr <- graph_from_data_frame(
  d = aristas,
  directed = FALSE,
  vertices = vertices
)

g_lfr <- simplify(
  g_lfr,
  remove.multiple = TRUE,
  remove.loops = TRUE
)

# Mostrar resumen
cat("\nRESUMEN DE LA RED LFR\n")
cat("-----------------------------------\n")
cat("Nodos:", vcount(g_lfr), "\n")
cat("Aristas Ãºnicas:", ecount(g_lfr), "\n")
cat("Red dirigida:", is_directed(g_lfr), "\n")
cat("Componentes:", components(g_lfr)$no, "\n")
cat(
  "Microcomunidades conocidas:",
  length(unique(micro_conocidas$micro_comunidad)),
  "\n"
)
cat(
  "Macrocomunidades conocidas:",
  length(unique(macro_conocidas$macro_comunidad)),
  "\n"
)

# Validaciones
stopifnot(vcount(g_lfr) == 1000)
stopifnot(ecount(g_lfr) == 9867)
stopifnot(nrow(micro_conocidas) == 1000)
stopifnot(nrow(macro_conocidas) == 1000)
stopifnot(
  length(unique(micro_conocidas$micro_comunidad)) == 26
)
stopifnot(
  length(unique(macro_conocidas$macro_comunidad)) == 5
)

cat("\nImportaciÃ³n y validaciÃ³n completadas correctamente.\n")







# ============================================================
# EJECUTAR CLUSTER_INFOMAP() MODIFICADO
# ============================================================

cat("\nEJECUTANDO IGRAPH MODIFICADO...\n")
cat("Esto puede tardar algunos minutos.\n\n")

set.seed(123)

tiempo_modificado <- system.time({

  resultado_modificado <- cluster_infomap(
    g_lfr,
    nb.trials = 100
  )

})

# Guardar el objeto completo para no repetir la ejecuciÃ³n
saveRDS(
  resultado_modificado,
  file.path(
    ruta_lfr,
    "resultado_lfr_igraph_modificado.rds"
  )
)

# ============================================================
# RESULTADO GENERAL
# ============================================================

membership_modificado <- membership(resultado_modificado)

cat("\nRESULTADO IGRAPH MODIFICADO\n")
cat("-----------------------------------\n")
cat(
  "Clase:",
  paste(class(resultado_modificado), collapse = ", "),
  "\n"
)
cat(
  "Comunidades finales:",
  length(unique(membership_modificado)),
  "\n"
)
cat(
  "Codelength:",
  resultado_modificado$codelength,
  "\n"
)
cat(
  "Modularity:",
  modularity(resultado_modificado),
  "\n"
)
cat(
  "Tiempo transcurrido:",
  as.numeric(tiempo_modificado["elapsed"]),
  "segundos\n"
)

cat("\nCampos disponibles:\n")
print(names(resultado_modificado))

# ============================================================
# INFORMcurrido:",
# ============================================================

if (is.null(resultado_modificado$multilevel_modules)) {
  stop(
    paste(
      "El resultado no contiene multilevel_modules.",
      "Revisa que se estÃ© usando el prototipo modificado."
    )
  )
}

multinivel <- as.data.frame(
  resultado_modificado$multilevel_modules
)

cat("\nPrimeras filas de multilevel_modules:\n")
print(head(multinivel))

cat("\nDimensiones de multilevel_modules:\n")
print(dim(multinivel))

cat("\nColumnas de multilevel_modules:\n")
print(colnames(multinivel))

columnas_nivel <- grep(
  "^level_[0-9]+$",
  colnames(multinivel),
  value = TRUE
)

modulos_por_nivel <- sapply(
  columnas_nivel,
  function(columna) {
    valores <- multinivel[[columna]]
    length(unique(valores[!is.na(valores)]))
  }
)

cat("\nMÃ³dulos detectados por nivel:\n")
print(modulos_por_nivel)

cat("\nCampos de resumen:\n")
cat(
  "num_levels:",
  resultado_modificado$num_levels,
  "\n"
)
cat(
  "num_top_modules:",
  resultado_modificado$num_top_modules,
  "\n"
)
cat(
  "max_tree_depth:",
  resultado_modificado$max_tree_depth,
  "\n"
)

# ============================================================
# COMPROBAR FINAL_MODULE Y MEMBERSHIP()
# ============================================================

multinivel_ordenado <- multinivel[
  order(as.integer(multinivel$node_id)),
]

if (!is.null(names(membership_modificado))) {

  membership_ordenado <- membership_modificado[
    match(
      as.character(multinivel_ordenado$node_id),
      names(membership_modificado)
    )
  ]

} else {

  membership_ordenado <- membership_modificado

}

final_module_ok <- all(
  as.integer(multinivel_ordenado$final_module) ==
    as.integer(membership_ordenado)
)

cat(
  "\nfinal_module coincide con membership():",
  final_module_ok,
  "\n"
)

# ============================================================
# GUARDAR RESULTADOS INICIALES
# ============================================================

write.csv(
  multinivel,
  file.path(
    ruta_lfr,
    "multilevel_modules_lfr_modificado.csv"
  ),
  row.names = FALSE
)

membership_csv <- data.frame(
  node_id = as.integer(names(membership_modificado)),
  final_module = as.integer(membership_modificado)
)

membership_csv <- membership_csv[
  order(membership_csv$node_id),
]

write.csv(
  membership_csv,
  file.path(
    ruta_lfr,
    "membership_lfr_modificado.csv"
  ),
  row.names = FALSE
)

cat("\nArchivos guardados correctamente:\n")
cat("- resultado_lfr_igraph_modificado.rds\n")
cat("- multilevel_modules_lfr_modificado.csv\n")
cat("- membership_lfr_modificado.csv\n")

set.seed(123)




# ============================================================
# COMPARAR PARTICIONES CONOCIDAS Y DETECTADAS
# ============================================================

nivel_superior <- as.character(
  resultado_modificado$top_module_level
)

if (length(nivel_superior) != 1L ||
    !nivel_superior %in% colnames(multinivel)) {
  stop(
    "top_module_level no corresponde a una columna vÃ¡lida."
  )
}

cat("\nVALIDACIÃ“N CON COMUNIDADES CONOCIDAS\n")
cat("-----------------------------------\n")
cat("Nivel superior utilizado:", nivel_superior, "\n")

# Particiones detectadas por nodo
detectadas <- data.frame(
  node_id = as.integer(multinivel$node_id),
  modulo_superior_detectado = as.integer(
    multinivel[[nivel_superior]]
  ),
  final_module = as.integer(
    multinivel$final_module
  )
)

# Unir los datos usando node_id
comparacion_lfr <- merge(
  macro_conocidas,
  micro_conocidas,
  by = "node_id"
)

comparacion_lfr <- merge(
  comparacion_lfr,
  detectadas,
  by = "node_id"
)

comparacion_lfr <- comparacion_lfr[
  order(comparacion_lfr$node_id),
]

stopifnot(nrow(comparacion_lfr) == 1000)

cat(
  "Macrocomunidades conocidas:",
  length(unique(comparacion_lfr$macro_comunidad)),
  "\n"
)

cat(
  "Microcomunidades conocidas:",
  length(unique(comparacion_lfr$micro_comunidad)),
  "\n"
)

cat(
  "MÃ³dulos superiores detectados:",
  length(unique(
    comparacion_lfr$modulo_superior_detectado
  )),
  "\n"
)

cat(
  "Comunidades finales detectadas:",
  length(unique(comparacion_lfr$final_module)),
  "\n"
)

nivel_igual_final <- all(
  comparacion_lfr$modulo_superior_detectado ==
    comparacion_lfr$final_module
)

cat(
  "Nivel superior coincide con final_module:",
  nivel_igual_final,
  "\n"
)

cat(
  "Nivel superior coincide con final_module:",
  nivel_igual_final,
  "\n"
)

# ============================================================
# CALCULAR NMI Y ARI
# ============================================================

nmi_macro <- igraph::compare(
  comparacion_lfr$macro_comunidad,
  comparacion_lfr$modulo_superior_detectado,
  method = "nmi"
)

ari_macro <- igraph::compare(
  comparacion_lfr$macro_comunidad,
  comparacion_lfr$modulo_superior_detectado,
  method = "adjusted.rand"
)

nmi_micro <- igraph::compare(
  comparacion_lfr$micro_comunidad,
  comparacion_lfr$final_module,
  method = "nmi"
)

ari_micro <- igraph::compare(
  comparacion_lfr$micro_comunidad,
  comparacion_lfr$final_module,
  method = "adjusted.rand"
)

metricas_lfr <- data.frame(
  comparacion = c(
    "Macro conocidas vs nivel superior detectado",
    "Micro conocidas vs final_module"
  ),
  NMI = c(nmi_macro, nmi_micro),
  ARI = c(ari_macro, ari_micro)
)

cat("\nMÃ‰TRICAS DE VALIDACIÃ“N LFR\n")
cat("-----------------------------------\n")
print(metricas_lfr)

# ============================================================
# TABLAS DE CONTINGENCIA
# ============================================================

tabla_macro <- table(
  macro_conocida =
    comparacion_lfr$macro_comunidad,
  modulo_detectado =
    comparacion_lfr$modulo_superior_detectado
)

tabla_micro <- table(
  micro_conocida =
    comparacion_lfr$micro_comunidad,
  final_detectado =
    comparacion_lfr$final_module
)

cat("\nTABLA MACRO\n")
print(tabla_macro)

cat("\nTABLA MICRO\n")
print(tabla_micro)

# ============================================================
# GUARDAR RESULTADOS
# ============================================================

write.csv(
  comparacion_lfr,
  file.path(
    ruta_lfr,
    "comparacion_particiones_lfr.csv"
  ),
  row.names = FALSE
)

write.csv(
  metricas_lfr,
  file.path(
    ruta_lfr,
    "metricas_validacion_lfr.csv"
  ),
  row.names = FALSE
)

write.csv(
  as.data.frame(tabla_macro),
  file.path(
    ruta_lfr,
    "contingencia_macro_lfr.csv"
  ),
  row.names = FALSE
)

write.csv(
  as.data.frame(tabla_micro),
  file.path(
    ruta_lfr,
    "contingencia_micro_lfr.csv"
  ),
  row.names = FALSE
)

resumen_lfr <- c(
  "VALIDACIÃ“N DEL PROTOTIPO CON RED LFR",
  "",
  paste("Nodos:", vcount(g_lfr)),
  paste("Aristas:", ecount(g_lfr)),
  paste("Macrocomunidades conocidas:", 5),
  paste("Microcomunidades conocidas:", 26),
  "",
  paste("Primer nivel con divisiÃ³n real:", nivel_superior),
  paste(
    "MÃ³dulos superiores detectados:",
    length(unique(
      comparacion_lfr$modulo_superior_detectado
    ))
  ),
  paste(
    "Comunidades finales detectadas:",
    length(unique(comparacion_lfr$final_module))
  ),
  paste(
    "Nivel superior coincide con final_module:",
    nivel_igual_final
  ),
  "",
  paste("NMI macro:", nmi_macro),
  paste("ARI macro:", ari_macro),
  paste("NMI micro:", nmi_micro),
  paste("ARI micro:", ari_micro),
  "",
  paste(
    "Codelength:",
    resultado_modificado$codelength
  ),
  paste(
    "Modularity:",
    modularity(resultado_modificado)
  ),
  paste(
    "num_levels:",
    resultado_modificado$num_levels
  ),
  paste(
    "num_top_modules:",
    resultado_modificado$num_top_modules
  ),
  paste(
    "top_module_level:",
    resultado_modificado$top_module_level
  ),
  paste(
    "max_tree_depth:",
    resultado_modificado$max_tree_depth
  )
)

writeLines(
  resumen_lfr,
  file.path(
    ruta_lfr,
    "resumen_validacion_lfr.txt"
  )
)

cat("\nArchivos de validaciÃ³n guardados correctamente.\n")