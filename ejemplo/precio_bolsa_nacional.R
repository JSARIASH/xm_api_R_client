direccion <- paste0(getwd(), "/ejemplo/librerias.R")
source(file = direccion)

# Fechas del archivo precio bolsa nacional 
fechas_pb <- read_csv("datos/precio_bolsa_nacional.csv", col_names = TRUE, col_select = 1)
fechas_pb <- max(fechas_pb$fecha) # Última fecha en los datos. 
#fecha_inicial <- ymd("2000-01-01")


fecha_inicial <- fechas_pb + 1

fechas_consulta <- c(seq.Date(from = fecha_inicial, to = today(), by = "month"), today())
url <- "https://servapibi.xm.com.co/hourly"
       
precio_bolsa_tot <- vector(length = length(fechas_consulta) - 1, mode = "list")
for (i in seq_len(length(precio_bolsa_tot) - 1)) {
  body = list(MetricId = "PrecBolsNaci", 
              StartDate = as.character(fechas_consulta[i]), 
              EndDate = as.character(fechas_consulta[i + 1] - 1), 
              Entity = "Sistema"
  )
  body <- toJSON(body)
  precio_bolsa <- POST(url, body = body, encode = "json", content_type("application/json"))
  if (precio_bolsa$status_code != 200) {
      warning(sprintf(" Error entre las fechas %s y %s \n código %i \\oO/", fechas_consulta[i], fechas_consulta[i + 1], precio_bolsa$status_code)) 
  } 
  precio_bolsa <- fromJSON(content(precio_bolsa, as = "text"))
  dias <- length(precio_bolsa$Items)
  precio_bolsa_dc <- lapply(1:dias, 
                            function (x) {
                              c(fecha = precio_bolsa$Items[[x]]$Date ,unlist(precio_bolsa$Items[[x]]$HourlyEntities[[1]]$Values))
                            }
                      )
  precio_bolsa_dc <- bind_rows(precio_bolsa_dc)
  precio_bolsa_tot[[i]] <- precio_bolsa_dc
}

precio_bolsa_tot <- bind_rows(precio_bolsa_tot)
precio_bolsa_tot <- precio_bolsa_tot %>% 
  mutate(fecha = ymd(fecha)) %>% 
  mutate_at(vars(matches("Hour")), as.numeric)

precio_bolsa_tot <- precio_bolsa_tot %>% select(-code)
precio_bolsa_tot <- precio_bolsa_tot %>% pivot_longer(cols = 2:25, names_to = "hora", values_to  = "precio") 

# Se anexa a los datos que ya estan en el archivo. Nombre de las columnas no son requeridas. 
write_csv(precio_bolsa_tot, file = "datos/precio_bolsa_nacional.csv", col_names = FALSE, append = TRUE)

