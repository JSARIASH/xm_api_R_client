direccion <- paste0(getwd(), "/ejemplo/librerias.R")
source(file = direccion)

# Compras de energía en contratos por el sistema (mercado - todas las compras de los agentes) 
# con el fin de atender a los usuarios regulados. Es agregada, no se tiene como compararla en XM
# Fecha incial para la descarga = "2000-01-01"
# Fecha final de descarga. Hoy menos 5 días
# Estas fechas deben corroborse. 

fecha_inicial <- ymd("2000-01-01")
fechas_consulta <- c(seq.Date(fecha_inicial, today(), by = "month"), today() - 5)
url <- "http://servapibi.xm.com.co/hourly"
compras_contratos_sistema <- vector(mode = "list", length = length(fechas_consulta) - 1)#Compras en contratos dirigida a los usuarios regulados. 
for (i in seq_len(length(fechas_consulta) - 1)) {
  body <- list(MetricId = "CompContEnerReg",
               StartDate = as.character(fechas_consulta[i]),
               EndDate = as.character(fechas_consulta[i + 1] - 1),
               Entity = "Sistema"
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
  compras_contratos_sistema[[i]] <- consulta
}

compras_contratos_sistema <- bind_rows(compras_contratos_sistema)
compras_contratos_sistema <- compras_contratos_sistema %>% mutate(Date = ymd(Date)) %>% mutate_at(vars(matches("Hour")), as.numeric) %>% 
  select(-Id) %>% pivot_longer(cols = -c(Date, code), names_to = "hora", values_to = "Cantidad")


colnames(compras_contratos_sistema) <- c("fecha", "sistema", "hora", "compras_contratos_sistema")

write_csv(compras_contratos_sistema, file = "datos/compras_contratos_x_sistema_usr_reg.csv", col_names = TRUE)

