# Verifica que cada métrica de datos/listado_metricas.csv responda a una
# consulta mínima a la API de XM. No persiste los datos descargados —
# solo registra status_code, presencia de Items y mensaje de error.

library(httr)
library(jsonlite)
library(dplyr)
library(readr)
library(lubridate)

catalogo <- read_csv("datos/listado_metricas.csv", show_col_types = FALSE)

# Rango chico y lejos del lag de publicación. Sirve para hourly/daily/monthly.
fecha_inicial <- as.Date("2024-01-15")
fecha_final   <- as.Date("2024-01-16")

verificar_una <- function(metric_id, entity, url) {
  body <- jsonlite::toJSON(list(
    MetricId  = metric_id,
    StartDate = as.character(fecha_inicial),
    EndDate   = as.character(fecha_final),
    Entity    = entity
  ), auto_unbox = TRUE)

  res <- tryCatch(
    POST(url, body = body, encode = "json",
         content_type("application/json"),
         timeout(30)),
    error = function(e) e
  )

  if (inherits(res, "error")) {
    return(tibble(status_code = NA_integer_,
                  n_items     = NA_integer_,
                  mensaje     = conditionMessage(res)))
  }

  parsed <- tryCatch(
    jsonlite::fromJSON(content(res, as = "text", encoding = "UTF-8"),
                       simplifyVector = FALSE),
    error = function(e) NULL
  )

  n_items <- if (!is.null(parsed) && !is.null(parsed$Items))
    length(parsed$Items) else 0L

  mensaje <- if (!is.null(parsed) && !is.null(parsed$Message))
    as.character(parsed$Message) else NA_character_

  tibble(status_code = res$status_code,
         n_items     = n_items,
         mensaje     = mensaje)
}

resultados <- vector("list", nrow(catalogo))
for (i in seq_len(nrow(catalogo))) {
  row <- catalogo[i, ]
  cat(sprintf("[%3d/%d] %-25s %-10s ... ",
              i, nrow(catalogo), row$metricid, row$entity))
  r <- verificar_una(row$metricid, row$entity, row$url)
  resultados[[i]] <- bind_cols(
    tibble(metricid = row$metricid, entity = row$entity, url = row$url),
    r
  )
  cat(sprintf("status=%s items=%s\n",
              r$status_code, r$n_items))
  Sys.sleep(0.15)
}

resultados <- bind_rows(resultados)

resultados <- resultados %>%
  mutate(ok = !is.na(status_code) & status_code == 200 & n_items > 0)

write_csv(resultados, "datos/verificacion_metricas.csv")

cat("\n=== Resumen ===\n")
cat(sprintf("Total:        %d\n", nrow(resultados)))
cat(sprintf("OK:           %d\n", sum(resultados$ok)))
cat(sprintf("HTTP != 200:  %d\n",
            sum(!is.na(resultados$status_code) & resultados$status_code != 200)))
cat(sprintf("Sin items:    %d\n",
            sum(!is.na(resultados$status_code) & resultados$status_code == 200 & resultados$n_items == 0)))
cat(sprintf("Error red:    %d\n", sum(is.na(resultados$status_code))))
