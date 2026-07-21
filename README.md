# Implementación multinivel de Infomap en R/igraph

Repositorio asociado a la memoria de título:

> **Implementación del Algoritmo Infomap en el ecosistema R: Integración de Características Avanzadas para Detección de Comunidades en Redes Complejas**

**Autora:** Daniela Salinas Castro  
**Carrera:** Ingeniería Civil en Computación  
**Institución:** Universidad de O'Higgins  
**Año:** 2026  

## Descripción

Este repositorio contiene una versión local experimental del paquete
[R/igraph](https://github.com/igraph/rigraph), modificada para incorporar y
representar información jerárquica o multinivel generada durante la ejecución
del algoritmo Infomap.

La implementación tradicional de `cluster_infomap()` expone principalmente una
partición plana, donde cada nodo queda asociado a una comunidad final. La
modificación desarrollada amplía el objeto de clase `communities` con
información complementaria sobre los distintos niveles de la estructura
comunitaria.

Este repositorio deriva del proyecto oficial `igraph/rigraph`. La modificación
presentada no forma parte de la distribución oficial de igraph.

## Objetivo

Diseñar, implementar y evaluar una extensión de `cluster_infomap()` que permita
acceder a información jerárquica o multinivel, manteniendo la compatibilidad con
la interfaz tradicional del paquete igraph para R.

## Campos incorporados

La versión experimental agrega los siguientes campos al objeto retornado por
`cluster_infomap()`:

- `multilevel_modules`: tabla con la pertenencia de cada nodo en los distintos
  niveles de la jerarquía.
- `num_levels`: cantidad total de columnas utilizadas para representar los
  niveles y la comunidad final.
- `num_top_modules`: cantidad de módulos presentes en el primer nivel con una
  división efectiva.
- `top_module_level`: nivel utilizado para calcular `num_top_modules`.
- `max_tree_depth`: profundidad máxima de la estructura jerárquica explícita.

La partición plana tradicional permanece disponible mediante:

- `membership()`
- `sizes()`
- `modularity()`

La columna `final_module` de `multilevel_modules` se mantiene consistente con
el resultado entregado por `membership()`.

## Modificación principal

La extensión de la salida de `cluster_infomap()` se encuentra implementada
principalmente en:

```text
R/community.R
```

La modificación no reemplaza el algoritmo Infomap utilizado internamente. Su
propósito es recuperar, organizar y exponer información multinivel que no se
encuentra disponible explícitamente en la salida tradicional.

## Entorno utilizado

La implementación y las pruebas principales fueron realizadas con el siguiente
entorno:

| Herramienta | Versión utilizada |
|---|---:|
| R | 4.6.0 |
| igraph original | 2.3.2 |
| igraph modificado | 2.3.2.9015 |
| Infomap para R | 2.12.0 |
| RStudio | 2026.05.0+218 |
| Rtools | Rtools45 |
| Sistema operativo | Windows 11 / macOS |

## Requisitos

Para compilar la versión modificada en Windows se requiere:

- R.
- Rtools compatible con la versión instalada de R.
- Git.
- PowerShell o una terminal equivalente.
- Las dependencias requeridas por el paquete igraph.

Las pruebas finales de compilación e instalación fueron realizadas en Windows
11 utilizando Rtools45.

## Clonar el repositorio

```powershell
git clone --recurse-submodules https://github.com/tiffahana/tesis-rigraph-infomap.git
cd tesis-rigraph-infomap
git checkout campos-resumen-jerarquia
```

## Compilar e instalar

Desde la carpeta principal del repositorio se puede instalar la versión local
mediante:

```powershell
R CMD INSTALL --preclean .
```

También es posible construir primero el paquete:

```powershell
R CMD build .
R CMD INSTALL igraph_2.3.2.9015.tar.gz
```

El nombre exacto del archivo generado puede variar según la versión definida en
`DESCRIPTION`.

## Ejemplo de uso

```r
library(igraph)

g <- make_graph("Zachary")

resultado <- cluster_infomap(
  g,
  nb.trials = 100
)

membership(resultado)
sizes(resultado)
modularity(resultado)

resultado$num_levels
resultado$num_top_modules
resultado$top_module_level
resultado$max_tree_depth

head(resultado$multilevel_modules)

stopifnot(
  identical(
    as.integer(resultado$multilevel_modules$final_module),
    as.integer(membership(resultado))
  )
)
```

## Evaluación experimental

La implementación fue evaluada utilizando las siguientes redes:

- Zachary Karate Club.
- USairports.
- Yeast.
- Una red artificial jerárquica generada mediante el benchmark LFR.

En la configuración LFR utilizada durante la investigación, el prototipo
recuperó:

- 6 macrocomunidades;
- 147 microcomunidades;
- NMI igual a 1;
- ARI igual a 1.

Estos resultados corresponden a la configuración experimental documentada en
la memoria y no implican que todas las redes o parámetros produzcan una
recuperación exacta.

## Scripts y resultados

El repositorio incluye scripts utilizados para:

- validar los campos multinivel;
- comparar igraph original con la versión modificada;
- comparar la versión modificada con Infomap oficial;
- analizar las redes Zachary, USairports y Yeast;
- validar la jerarquía conocida del benchmark LFR;
- calcular NMI y ARI;
- generar tablas y figuras utilizadas en la memoria.

Los scripts y resultados serán organizados en una carpeta específica de la
tesis antes de generar la versión estable `v1.0`.

## Alcance y limitaciones

Esta implementación corresponde a un prototipo experimental desarrollado con
fines académicos.

Actualmente:

- no incorpora el cálculo de `hierarchical_codelength`;
- no forma parte del paquete oficial igraph;
- utiliza definiciones operacionales propias para algunos campos de resumen;
- fue evaluada sobre un conjunto acotado de redes;
- puede presentar variaciones entre ejecuciones debido al carácter estocástico
  de Infomap.

## Proyecto original y atribución

Este trabajo se basa en el código fuente del paquete R/igraph, desarrollado y
mantenido por la comunidad oficial de igraph.

Proyecto original:

```text
https://github.com/igraph/rigraph
```

Se mantienen los archivos de licencia, autoría, citación y atribución
correspondientes al proyecto original.

## Licencia

Este repositorio conserva la licencia del proyecto R/igraph:

**GNU General Public License, versión 2 o posterior.**

## Autora de la extensión experimental

**Daniela Salinas Castro**  
Ingeniería Civil en Computación  
Universidad de O'Higgins  
2026
