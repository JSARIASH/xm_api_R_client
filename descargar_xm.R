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
# Entity=Agente; objeto único para Entity=Sistema).
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
