# Re-verifica solo las métricas que fallaron en verificar_metricas.R,
# usando parámetros más amigables:
#   - Listado*: corrige /list -> /lists.
#   - monthly: rango de 1 año.
#   - daily/hourly: rango de 30 días en mitad de 2023 (cobertura sólida).

library(httr)
library(jsonlite)
library(dplyr)
library(readr)
library(lubridate)

fallidas <- read_csv("datos/verificacion_metricas.csv", show_col_types = FALSE) |>
  filter(!ok)

verificar_una <- function(metric_id, entity, url, start, end) {
  url_fix <- sub("/list$", "/lists", url)

  body <- jsonlite::toJSON(list(
    MetricId  = metric_id,
    StartDate = as.character(start),
    EndDate   = as.character(end),
    Entity    = entity
  ), auto_unbox = TRUE)

  res <- tryCatch(
    POST(url_fix, body = body, encode = "json",
         content_type("application/json"), timeout(30)),
    error = function(e) e
  )
  if (inherits(res, "error")) {
    return(tibble(url_usado = url_fix, status_code = NA_integer_,
                  n_items = NA_integer_, mensaje = conditionMessage(res)))
  }

  parsed <- tryCatch(
    jsonlite::fromJSON(content(res, as = "text", encoding = "UTF-8"),
                       simplifyVector = FALSE),
    error = function(e) NULL)

  n_items <- if (!is.null(parsed) && !is.null(parsed$Items))
    length(parsed$Items) else 0L
  mensaje <- if (!is.null(parsed) && !is.null(parsed$Message))
    as.character(parsed$Message) else NA_character_

  tibble(url_usado = url_fix, status_code = res$status_code,
         n_items = n_items, mensaje = mensaje)
}

elegir_rango <- function(url, metricid) {
  if (grepl("/list$|/lists$", url))
    return(list(start = as.Date("2024-01-15"), end = as.Date("2024-01-16")))
  if (grepl("/monthly$", url))
    return(list(start = as.Date("2023-01-01"), end = as.Date("2024-01-01")))
  # hourly / daily
  list(start = as.Date("2023-06-01"), end = as.Date("2023-06-30"))
}

resultados <- vector("list", nrow(fallidas))
for (i in seq_len(nrow(fallidas))) {
  row <- fallidas[i, ]
  rg  <- elegir_rango(row$url, row$metricid)
  cat(sprintf("[%2d/%d] %-25s %-10s %-7s ... ",
              i, nrow(fallidas), row$metricid, row$entity,
              sub("https://servapibi.xm.com.co/", "", row$url)))
  r <- verificar_una(row$metricid, row$entity, row$url, rg$start, rg$end)
  resultados[[i]] <- bind_cols(
    tibble(metricid = row$metricid, entity = row$entity,
           url_catalogo = row$url, start = rg$start, end = rg$end),
    r
  )
  cat(sprintf("status=%s items=%s\n", r$status_code, r$n_items))
  Sys.sleep(0.15)
}

resultados <- bind_rows(resultados) |>
  mutate(ok = !is.na(status_code) & status_code == 200 & n_items > 0)

write_csv(resultados, "datos/verificacion_metricas_retry.csv")

cat("\n=== Resumen retry ===\n")
cat(sprintf("Recuperadas (ok ahora): %d\n", sum(resultados$ok)))
cat(sprintf("Aún fallando:           %d\n", sum(!resultados$ok)))
cat("\n--- Las que siguen fallando ---\n")
resultados |> filter(!ok) |>
  select(metricid, entity, url_catalogo, status_code, n_items, mensaje) |>
  print(n = 50)
