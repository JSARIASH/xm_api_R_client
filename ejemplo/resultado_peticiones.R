# 400 Bad request
source(file = "~/Documents/Lab_Financiero/Repos/xm_api_R_client/ejemplo/librerias.R")


listado_metricas <- read_csv(file = "~/Documents/Lab_Financiero/Repos/xm_api_R_client/datos/listado_metricas.csv", col_names = TRUE, 
                             col_types = as.list(c(rep("c", 4), "d", rep("c", 5))))


respuestas_http <- matrix(nrow = nrow(listado_metricas), ncol = 4)

for (i in 1:nrow(listado_metricas)) {
  if (str_detect(listado_metricas$MetricId[i], "Listado")) {
    body = list(MetricId = listado_metricas$MetricId[i])
    body <- toJSON(body)
    consulta <- POST("http://servapibi.xm.com.co/lists", 
                     body = body,
                     encode = "json", content_type("application/json")
    )
    
  } else {
    body = list(MetricId = listado_metricas$MetricId[i], 
                StartDate = "2022-05-05", 
                EndDate = "2022-05-06", 
                Entity = listado_metricas$Entity[i]
    )
    body <- toJSON(body)
    consulta <- POST(listado_metricas$Url[i], 
                     body = body,
                     encode = "json", content_type("application/json")
    )
  }
  
  respuestas_http[i, 1] <- listado_metricas$MetricId[i]
  respuestas_http[i, 2] <- listado_metricas$Entity[i]
  respuestas_http[i, 3] <- listado_metricas$Url[i]
  respuestas_http[i, 4] <- consulta$status_code
}
colnames(respuestas_http) <- c("MetricId", "Entity", "Url" ,"Status")
respuestas_http <- as_tibble(respuestas_http)

## Peticiones con error 400

peticiones_erroneas <- respuestas_http %>% filter(Status != 200)
# Algunas consultas no requieren el campo entity. 
fila <- 6
sapply(1:6, 
       function(x){
         body <- list(MetricId = peticiones_erroneas$MetricId[x],
                      StartDate = "2022-05-05", 
                      EndDate = "2022-05-05")
         body <- toJSON(body)
         
         consulta <- POST(url = peticiones_erroneas$Url[x], 
              body = body, 
              encode = "json", content_type("application/json"))
         return(consulta$status_code)
       })


# 2 y 6 bad request. 
peticiones_erroneas %>% slice(c(2, 6))

body <- list(MetricId = peticiones_erroneas$MetricId[2],
             StartDate = "2022-05-05", 
             EndDate = "2022-05-05", 
             Entity = "Enlace")
body <- toJSON(body)
POST(url = peticiones_erroneas$Url[2], 
     body = body, 
     encode = "json", content_type("application/json"))

POST(url = "http://servapibi.xm.com.co/hourly", 
     body = '{"MetricId": "ImpoMoneda",
    "StartDate":"2022-05-05", 
    "EndDate":"2022-05-06",
    "Entity": "Sistema"}', 
     encode = "json", content_type("application/json"))
