library(jsonlite)
library(rjson)
library(lubridate)
library(dplyr)
library(tidyr)
library(tidyr)
library(stringr)
library(httr)
library(ggplot2)
library(readr)
# direccion <- paste0(getwd(), "/datos/listado_metricas.csv")
# listado_metricas <- read_csv(file = direccion, 
#                              col_names = TRUE, col_types = as.list(c(rep("c", 4), "d", rep("c", 5))))

url <- "https://www.simem.co/backend-files/api/PublicData?startDate=2029-01-01&endDate=2029-01-01&datasetId=75f675"
a <- GET(url, encode = "json", content_type("application/json"))
content(a, "parsed")

content(a)

a$headers$`x-content-type-options`

names(a)
a$url
a$status_code
a$headers
a$all_headers
a$cookies
a$content
a$request
a$handle

content(a, "text", encoding = "ISO-8859-1")

stringi::stri_enc_detect(content(a, "raw"))
content(a, "text", encoding = "UTF-8")


datos <- jsonlite::fromJSON(content(a, "text"), simplifyVector = FALSE)
datos[[3]]
datos$parameters
datos$success
datos$result


datos$result$idDataset
datos$result$name
datos$result$metadata
datos$result$filterDate
datos$result$variables
datos$result$columns
datos$result$tags


datos[[1]]
datos$result$records

# Acá es donde están los datos ----
datos$result$records
datos$result$records[[1]]
datos$result$records[[2]]


datos$result$records



# Petición de la consulta ---- 

url <- "https://www.simem.co/backend-files/api/PublicData?startDate=2029-01-01&endDate=2029-01-01&datasetId=75f675"
body = list(datasetid = "75f675", 
            startdate = "2024-01-01", 
            enddate = "2024-02-01"
)
body <- toJSON(body)
peticion <- GET(url, body = body, encode = "json", content_type("application/json"))

  
jsonlite::fromJSON(content(peticion, "text"), simplifyVector = FALSE)








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
