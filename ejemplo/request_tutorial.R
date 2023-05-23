# install.packages("httr")
# Request methods: 
# GET: Retrieve information associated with a specific URL resource
# HEAD: Retrieve header information linked with a URL resource
# POST: Send data to the web server – for example, form data
# PUT: Replace the data for a specific URL with new data transmitted by the client
# DELETE: Delete the data behind the respective URL

library(jsonlite)
library(lubridate)
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)
library(httr)

# Listad de las métricas
dts_xm <- POST("http://servapibi.xm.com.co/lists", 
     body = '{"MetricId": "ListadoMetricas"}', 
     encode = "json", content_type("application/json"))

a <- content(dts_xm, "parsed")

content(dts_xm)

dts_xm$headers$`x-content-type-options`

names(dts_xm)
dts_xm$url
dts_xm$status_code
dts_xm$headers
dts_xm$all_headers
dts_xm$cookies
dts_xm$content
dts_xm$request
dts_xm$handle

content(dts_xm, "text", encoding = "ISO-8859-1")

stringi::stri_enc_detect(content(dts_xm, "raw"))
content(dts_xm, "text", encoding = "UTF-8")


datos <- jsonlite::fromJSON(content(dts_xm, "text"), simplifyVector = FALSE)
length(datos)
datos$Items

# datos es un liasta con dos elementos. 
datos[[1]]
sapply(1:length(datos[[2]]), function (x) length(datos[[2]][x]))

datos[[2]][[2]]

t(unlist(datos[[2]][[1]]$ListEntities))
all(sapply(1:183, function(x) ncol(t(unlist(datos[[2]][[x]]$ListEntities)))) == 10)

cantidad_metricas <- length(datos[[2]])

listado_metricas <- lapply(1:cantidad_metricas, function (x) t(unlist(datos[[2]][[x]]$ListEntities)))
listado_metricas <- do.call(rbind, listado_metricas)
listado_metricas <- as_tibble(listado_metricas)

View(listado_metricas)

colnames(listado_metricas) <- str_remove_all(colnames(listado_metricas), "Values.")
readr::write_csv(x = listado_metricas, file = "datos/listado_metricas.csv", col_names = TRUE)

#length(datos[[2]][[180]]$ListEntities[[1]]$Values)
# vector_carac <- fromJSON(content(dts_xm, as = "text"))
# vector_carac[[2]]





# Ejemplo de petición con la generación:
# Hay una petición con filtro para algunos agentes. 
generacion <- POST("http://servapibi.xm.com.co/hourly", body = '{"MetricId": "Gene",
"StartDate":"2022-05-05",
"EndDate":"2022-06-04",
"Entity": "Recurso",
"Filter":["TBST","GVIO"]}',
encode = "json", content_type("application/json"))


generacin_tabla <- fromJSON(content(generacion, as = "text"))
datos_tabla <- generacin_tabla$Items
dim(datos_tabla)
matriz_datos <- cbind(fecha = datos_tabla$Date, do.call(rbind, lapply(datos_tabla$HourlyEntities, unlist)))
matriz_datos <- as_tibble(matriz_datos)


# Se realiza la petición sin filtros
generacion <- POST("http://servapibi.xm.com.co/hourly", 
  body = 
  '{"MetricId": "Gene",
    "StartDate":"2022-05-05", 
    "EndDate":"2022-05-06",
    "Entity": "Recurso"}',
  encode = "json", content_type("application/json")
  )

generacion$status_code
a <- fromJSON(content(generacion, as = "text"))
a <- a$Items
matriz_a <- cbind(fecha = a$Date, do.call(rbind, lapply(a$HourlyEntities, unlist)))
matriz_a <- as_tibble(matriz_a)
View(matriz_a)
str(matriz_a)
matriz_a %>% mutate_at(c(4:27), as.numeric)

# Esta mal: elimina el código 
matriz_a <- matriz_a %>% mutate(across(starts_with("Values.H"), as.numeric)) %>% 
  mutate(fecha = ymd(fecha))



# Listado de recursos ----

listado_recursos <- POST(url = "www.servapibi.xm.com.co/lists",
                         body = '{"MetricId": "ListadoRecursos"}',
                         encode = "json", content_type("application/json")
                         )


listado_recursos <- POST("http://servapibi.xm.com.co/lists", 
                   body = '{"MetricId": "ListadoRecursos",
                            "StartDate":"2022-05-05", 
                            "EndDate":"2022-05-06"}',
                   encode = "json", content_type("application/json")
)
content(listado_recursos)


listado_agetes <- POST("http://servapibi.xm.com.co/lists",
                       body = '{"MetricId": "ListadoAgentes"}',
                       encode = "json", content_type("application/json")
                       )
status_code(listado_agetes)
http_status(listado_agetes)

content(listado_agetes, as = "text")
fromJSON(content(listado_agetes, as = "text"))

listado_rios <- POST("http://servapibi.xm.com.co/lists", 
                         body = '{"MetricId": "ListadoRios",
                                  "StartDate":"2022-05-05", 
                                  "EndDate":"2022-05-06"}',
                         encode = "json", content_type("application/json"))
content(listado_rios)

listado_embalses <- POST("http://servapibi.xm.com.co/lists", 
                         body = '{"MetricId": "ListadoEmbalses",
                                  "StartDate":"2022-05-05", 
                                  "EndDate":"2022-05-06"}',
                         encode = "json", content_type("application/json"))
content(listado_embalses)
                               
listado_recursos_AGPE <- POST("http://servapibi.xm.com.co/lists", 
                         body = '{"MetricId": "ListadoAGPE",
                                  "StartDate":"2022-05-05", 
                                  "EndDate":"2022-05-06",
                                  "Entity" : "Agente"}',
                         encode = "json", content_type("application/json"))

content(listado_recursos_AGPE)
http_status(listado_recursos_AGPE)


# Precio bolsa 
precio_bolsa <- POST("http://servapibi.xm.com.co/hourly", 
                   body = '{"MetricId": "PrecBolsNaci",
                            "StartDate":"2023-04-28", 
                            "EndDate":"2023-04-29",
                            "Entity": "Sistema"}',
                   encode = "json", content_type("application/json"))
content(precio_bolsa)
length(precio_bolsa)
class(precio_bolsa)
is.list(precio_bolsa)
precio_bolsa[[6]]
names(precio_bolsa)
precio_bolsa$status_code

precio_bolsa_nacional <- fromJSON(content(precio_bolsa, as = "text"))

# precio_bolsa_nacional
# precio_bolsa_nacional[[1]]
# precio_bolsa_nacional[[2]]
# 
# precio_bolsa_nacional[[2]]$Date
# 
# # convertir todo a matriz
# dias <- length(precio_bolsa_nacional[[2]]$HourlyEntities)
# precio_bolsa_nacional[[2]]$HourlyEntities[[1]]$Values
# 
# precio_bolsa_nacional[[2]]$HourlyEntities <- lapply(1:dias, 
#                                                     function (x) {
#                                                       precio_bolsa_nacional[[2]]$HourlyEntities[[x]]$Values <- as.matrix(precio_bolsa_nacional[[2]]$HourlyEntities[[x]]$Values, nrow = 1)
#                                                     }
#                                                     )
# 
# # Los data frames deben tener nombre de fila. 
# 
# 
# precio_bolsa_dc <- do.call(rbind, precio_bolsa_nacional[[2]]$HourlyEntities)
# precio_bolsa_dc <- cbind(precio_bolsa_nacional[[2]]$Date, precio_bolsa_dc)
# colnames(precio_bolsa_dc)[1] <- "Fecha"
# 
# as_tibble(precio_bolsa_dc)



