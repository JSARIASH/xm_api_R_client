# Catálogo de métricas disponibles en la API de XM.
# Documentación: https://github.com/EquipoAnaliticaXM/API_XM#elementos-para-su-uso

library(httr)
library(jsonlite)
library(dplyr)
library(readr)

metricas <- POST("https://servapibi.xm.com.co/lists",
          body = '{"MetricId": "ListadoMetricas"}',
          encode = "json", content_type("application/json"))

if (metricas$status_code != 200) {
  stop(sprintf("ListadoMetricas devolvió código %i", metricas$status_code))
}
metricas <- jsonlite::fromJSON(content(metricas, as = "text", encoding = "UTF-8"),
                               flatten = TRUE)

metricas <- bind_rows(metricas$Items$ListEntities)
colnames(metricas) <- sub("^Values\\.", "", colnames(metricas)) |>
  tolower()

# Bug del catálogo: las métricas Listado* reportan url = .../list (singular),
# pero el endpoint real es /lists. Lo corregimos en el origen.
metricas$url <- sub("/list$", "/lists", metricas$url)

write_csv(metricas, "datos/listado_metricas.csv")
