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

  fechas <- sort(unique(c(
    seq.Date(fecha_inicial, fecha_final, by = by_chunk),
    fecha_final
  )))

  chunks <- vector("list", length(fechas) - 1)
  for (i in seq_len(length(fechas) - 1)) {
    body <- toJSON(list(
      MetricId  = metric_id,
      StartDate = as.character(fechas[i]),
      EndDate   = as.character(fechas[i + 1] - 1),
      Entity    = entity
    ))
    r <- POST(url, body = body, encode = "json",
              content_type("application/json"))
    if (r$status_code != 200) {
      warning(sprintf("Error %s..%s código %i",
                      fechas[i], fechas[i + 1] - 1, r$status_code))
      next
    }
    items <- fromJSON(content(r, as = "text"))$Items
    if (is.null(items) || length(items) == 0) next
    chunks[[i]] <- bind_rows(lapply(items, unlist))
  }

  datos <- bind_rows(chunks)
  if (nrow(datos) == 0) {
    warning("No se recibieron datos en el rango solicitado.")
    return(datos)
  }

  colnames(datos) <- str_remove_all(
    colnames(datos),
    "HourlyEntities\\.Values\\.|HourlyEntities\\.|DailyEntities\\.|MonthlyEntities\\."
  )

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
