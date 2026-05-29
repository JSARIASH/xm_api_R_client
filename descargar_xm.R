# Cliente genérico para la API de XM (servapibi.xm.com.co).
# Hace la petición por chunks (la API limita el rango por petición),
# desempaqueta el JSON y devuelve un tibble ya casteado.

library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(readr)

# Aplana un Item (= una fecha) en una lista de filas, una por entidad.
# Maneja los tres tipos de entidad de XM (HourlyEntities, DailyEntities,
# MonthlyEntities) y los dos modos de empaquetado (array de entidades para
# Entity=Agente; objeto único para Entity=Sistema).\

flatten_dia <- function(dia) {
  ent_key <- intersect(names(dia),
                       c("HourlyEntities", "DailyEntities", "MonthlyEntities"))
  if (length(ent_key) == 0) return(list())
  ents <- dia[[ent_key]]
  # Si trae Value/Values en el nivel superior es una entidad única (Sistema),
  # envolvemos en lista para tratar siempre como array.
  if (!is.null(ents$Value) || !is.null(ents$Values)) ents <- list(ents)
  lapply(ents, function(e) c(list(Date = dia$Date), as.list(unlist(e))))
}

# descargar_xm()
# Descarga una métrica de la API XM para un rango de fechas y devuelve un tibble.
#
# ARGUMENTOS
#   metric_id      (chr)  Identificador de la métrica. Ver listado_metricas.csv,
#                         columna MetricId. Ej: "Gene", "PrecBolsNaci".
#   entity         (chr)  Entidad de agregación soportada por la métrica.
#                         Valores posibles: "Sistema", "Agente", "Recurso",
#                         "ListaRecursos", "DesagListaRecursos". Ver columna
#                         Entity en listado_metricas.csv.
#   fecha_inicial  (Date) Primer día del rango a descargar.
#   fecha_final    (Date) Último día del rango. Por defecto: today() - 5
#                         (la API publica con ~5 días de rezago).
#   endpoint       (chr)  Frecuencia del dato: "hourly" | "daily" | "monthly".
#                         El valor que se pase se valida contra esas tres opciones
#                         dentro de la función (match.arg); si no se especifica,
#                         toma "hourly" por ser el primero del vector.
#   by_chunk       (chr)  Tamaño del fragmento por petición. Por defecto "month".
#                         Pasarlo a "week" si la API devuelve 400 por rango largo.
#   pivot_largo    (lgl)  Si TRUE (defecto), pivota las columnas Hour01..Hour24
#                         a filas (columnas: hora, valor). Pasar FALSE para
#                         mantener el formato ancho original.
#   archivo_salida (chr)  Ruta de un CSV donde guardar el resultado. Si es NULL
#                         (defecto) no escribe ningún archivo.
#
# VALOR DE RETORNO
#   Tibble con columnas: Date, ShortName (nombre de la entidad cuando
#   Entity != "Sistema"), hora (si pivot_largo=TRUE) y valor; o Hour01..Hour24
#   como columnas separadas (si pivot_largo=FALSE).
#
# EJEMPLOS DE USO
#
#   source("descargar_xm.R")
#
#   # Generación horaria del sistema, últimos 30 días
#   gen <- descargar_xm("Gene", "Sistema",
#                       fecha_inicial = today() - 30)
#
#   # Precio de bolsa nacional, rango explícito, guardar CSV
#   precio <- descargar_xm("PrecBolsNaci", "Sistema",
#                          fecha_inicial  = as.Date("2024-01-01"),
#                          fecha_final    = as.Date("2024-12-31"),
#                          archivo_salida = "datos/precio_bolsa_2024.csv")
#
#   # Compras por agente, formato ancho (sin pivot)
#   compras <- descargar_xm("ComprasBolsa", "Agente",
#                           fecha_inicial = as.Date("2025-01-01"),
#                           pivot_largo   = FALSE)
#
#   # Proyección mensual (demanda UPME, escenario medio)
#   proy <- descargar_xm("EscDemUPMEMedio", "Sistema",
#                        fecha_inicial = as.Date("2023-01-01"),
#                        endpoint      = "monthly")

descargar_xm <- function(metric_id,
                         entity,
                         fecha_inicial,
                         fecha_final    = today() - 5,
                         endpoint       = c("hourly", "daily", "monthly"),
                         by_chunk       = "month",
                         pivot_largo    = TRUE,
                         archivo_salida = NULL) {

  endpoint <- match.arg(endpoint)
  url <- paste0("https://servapibi.xm.com.co/", endpoint)

  # /monthly devuelve 0 items si el rango no cubre un mes completo.
  # Expandimos a límites de mes para que cualquier rango funcione.
  if (endpoint == "monthly") {
    fecha_inicial <- floor_date(fecha_inicial, "month")
    fecha_final   <- ceiling_date(fecha_final, "month") - 1
  }

  fechas <- sort(unique(c(
    seq.Date(fecha_inicial, fecha_final, by = by_chunk),
    fecha_final
  )))

  chunks <- vector("list", length(fechas) - 1)
  ultimo <- length(fechas) - 1
  for (i in seq_len(ultimo)) {
    # En chunks intermedios restamos 1 día para no solapar con el siguiente;
    # en el último usamos fecha_final tal cual (si no, perderíamos un día y
    # además /monthly devuelve 400 cuando el rango no cubre el mes entero).
    end <- if (i == ultimo) fechas[i + 1] else fechas[i + 1] - 1
    body <- toJSON(list(
      MetricId  = metric_id,
      StartDate = as.character(fechas[i]),
      EndDate   = as.character(end),
      Entity    = entity
    ), auto_unbox = TRUE)
    r <- POST(url, body = body, encode = "json",
              content_type("application/json"))
    if (r$status_code != 200) {
      warning(sprintf("Error %s..%s código %i",
                      fechas[i], end, r$status_code))
      next
    }
    items <- jsonlite::fromJSON(content(r, as = "text", encoding = "UTF-8"),
                                simplifyVector = FALSE)$Items
    if (is.null(items) || length(items) == 0) next
    chunks[[i]] <- bind_rows(unlist(lapply(items, flatten_dia), recursive = FALSE))
  }

  datos <- bind_rows(chunks)
  if (nrow(datos) == 0) {
    warning("No se recibieron datos en el rango solicitado.")
    return(datos)
  }

  # flatten_dia ya quita el prefijo HourlyEntities./DailyEntities./MonthlyEntities.
  # Acá solo queda pelar el wrapper Values. de las métricas horarias.
  colnames(datos) <- sub("^Values\\.", "", colnames(datos))

  if ("Id" %in% colnames(datos)) datos <- select(datos, -Id)

  datos <- datos %>%
    mutate(Date = ymd(Date)) %>%
    mutate(across(matches("^Hour\\d+$|^Value$"), as.numeric))

  if (pivot_largo && any(str_detect(colnames(datos), "^Hour\\d+$"))) {
    datos <- pivot_longer(
      datos,
      cols      = matches("^Hour\\d+$"),
      names_to  = "hora",
      values_to = "valor"
    )
  }

  if (!is.null(archivo_salida)) {
    write_csv(datos, archivo_salida)
  }

  datos
}
