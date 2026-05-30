
library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(readr)

# listado_xm()
# Descarga un catálogo de la API XM (endpoint /lists) y devuelve un tibble.
#
# ARGUMENTOS
#   metric_id    (chr)  Identificador del catálogo. Valores posibles:
#                       "ListadoAgentes", "ListadoRecursos", "ListadoRios",
#                       "ListadoEmbalses", "ListadoAGPE", "ListadoMetricas".
#   entity       (chr)  Entidad por la que se hace la consulta: "Agente",
#                       "Sistema", "Recurso", etc. Determina el nivel/criterio
#                       con el que la API devuelve el catálogo y puede usarse en
#                       distintos MetricId (p. ej. "ListadoAGPE" o
#                       "ListadoRecursos"), no solo en uno. Si es NULL (defecto)
#                       no se envía el campo "Entity" en la petición. Los valores
#                       válidos dependen de cada catálogo (ver listado_metricas).
#   ruta_salida  (chr)  Ruta de un CSV donde guardar el resultado. Si es NULL
#                       (defecto) no escribe ningún archivo.
#
# VALOR DE RETORNO
#   Tibble con las columnas propias del catálogo consultado. Los campos varían
#   según el MetricId: por ejemplo, "ListadoAgentes" devuelve código y nombre
#   del agente; "ListadoRecursos" incluye además tipo de tecnología y río.
#
# EJEMPLOS DE USO
#
#   source("listados_xm.R")
#
#   # Listado de agentes del mercado
#   agentes <- listado_xm("ListadoAgentes")
#
#   # Listado de recursos consultado por recurso, guardar CSV
#   recursos <- listado_xm("ListadoRecursos", entity = "Recurso",
#                          ruta_salida = "datos/listado_recursos.csv")
#
#   # Mismo catálogo, consultado por agente o por sistema
#   agpe_agente  <- listado_xm("ListadoAGPE", entity = "Agente")
#   agpe_sistema <- listado_xm("ListadoAGPE", entity = "Sistema")

listado_xm <- function(metric_id, entity = NULL, ruta_salida = NULL) {
  url <- "https://servapibi.xm.com.co/lists"
  params <- list(MetricId = metric_id)
  if (!is.null(entity)) params$Entity <- entity
  body <- toJSON(params, auto_unbox = TRUE)

  r <- POST(url = url, body = body, encode = "json", content_type("application/json"))
  if (r$status_code != 200) {
    stop(sprintf("Error al consultar '%s': código HTTP %i", metric_id, r$status_code))
  }

  items <- jsonlite::fromJSON(content(r, as = "text", encoding = "UTF-8"),
                              simplifyVector = FALSE)$Items
  entidades <- unlist(lapply(items, `[[`, "ListEntities"), recursive = FALSE)
  entidades <- bind_rows(lapply(entidades, function(e) as.list(unlist(e))))
  
  if (!is.null(ruta_salida)) {
    write_csv(x = entidades, file = ruta_salida)
  }
  entidades
}

