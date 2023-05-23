direccion <- paste0(getwd(), "/ejemplo/librerias.R")
source(file = direccion)

# Compras de energía en contratos por cada AGENTE
# se presume que es para atender la demanda nacional (usuarios regulados y no regulados)
# Coincide con la información que reporta xm en la carpeta de contratos: 
# https://sinergox.xm.com.co/trpr/Paginas/Historicos/Historicos.aspx
# Fecha incial para la descarga = "2000-01-01"
# Fecha final de descarga. Hoy menos 5 días
# Estas fechas deben corroborse. 

fecha_inicial <- ymd("2000-01-01")
fechas_consulta <- c(seq.Date(fecha_inicial, today(), by = "month"), today() - 5)
url <- "http://servapibi.xm.com.co/hourly"
compras_contratos_agentes <- vector(mode = "list", length = length(fechas_consulta) - 1)#Compras en contratos dirigida a los usuarios regulados. 
for (i in seq_len(length(fechas_consulta) - 1)) {
  body <- list(MetricId = "CompContEner",
               StartDate = as.character(fechas_consulta[i]),
               EndDate = as.character(fechas_consulta[i + 1] - 1),
               Entity = "Agente"
  )
  body <- toJSON(body)
  consulta <- POST(url = url, body = body, encode = "json", content_type("application/json"))
  consulta <- fromJSON(content(consulta, as = "text"))
  consulta <- lapply(1:length(consulta$Items), 
                     function (x) {
                       return(unlist(consulta$Items[[x]]))                                          
                     }
  )
  consulta <- bind_rows(consulta)
  colnames(consulta) <- str_remove_all(colnames(consulta), pattern = "HourlyEntities.Values.|HourlyEntities.")
  compras_contratos_agentes[[i]] <- consulta
}


compras_contratos_agentes <- bind_rows(compras_contratos_agentes)
compras_contratos_agentes <- compras_contratos_agentes %>% mutate(Date = ymd(Date)) %>% mutate_at(vars(matches("Hour")), as.numeric) %>% 
  select(-Id) %>% pivot_longer(cols = -c(Date, code), names_to = "hora", values_to = "Cantidad")

colnames(compras_contratos_agentes_results$compras_contratos_agentes) <- c("fecha", "codigo_agente", "hora", "contratos_sistema")
write_csv(compras_contratos_agentes_results$compras_contratos_agentes, file = "datos/contratos_x_agentes_dem_nacional.csv", col_names = TRUE, append = FALSE)



# compras_contratos_agentes_results$compras_contratos_agentes %>% 
#   filter(codigo_agente == "EPMC", fecha == ymd("2021-01-01"), hora == "Hour01") %>% select(contratos_sistema) %>% pull
