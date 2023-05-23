direccion <- paste0(getwd(), "/ejemplo/librerias.R")
print(direccion)
source(file = direccion)

# Compras de energía en bolsa para antender la demanda nacional (puede ser regulada o no regulada)
# es la demanda total. 
# Fecha incial para la descarga = "2000-01-01"
# Fecha final de descarga. Hoy menos 5 días
# Estas fechas deben corroborse. 

fecha_inicial <- ymd("2000-01-01") # ymd("2023-01-01") 
fechas_consulta <- c(seq.Date(fecha_inicial, to = today(), by = "month"), today() - 5)
url <- "http://servapibi.xm.com.co/hourly"

# Energía comprada en bolsa para atender la demanda nacional. 
compras_bolsa <- vector(mode = "list", length = length(fechas_consulta) - 1)  
for (i in seq_len(length(fechas_consulta) - 1)) {
  body <- list(MetricId = "CompBolsNaciEner",
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
  compras_bolsa[[i]] <- consulta
}

compras_bolsa <- bind_rows(compras_bolsa)
compras_bolsa <- compras_bolsa %>% mutate(Date = ymd(Date)) %>% mutate_at(vars(matches("Hour")), as.numeric) %>% 
  select(-Id) %>% pivot_longer(cols = -c(Date, code), names_to = "hora", values_to = "Cantidad")


write_csv(x = compras_bolsa, file = "datos/compras_bolsa_agentes.csv", col_names = TRUE)



# X11()
# compras_bolsa %>% group_by(code, Date) %>% 
#   summarise(cantidad_promedio = mean(Cantidad, na.rm = TRUE)) %>% 
#   ggplot(aes(Date, cantidad_promedio, group = code)) + geom_line(alpha = 0.4) +
#   theme_light()

# energia_b <- read_csv(file = "datos/compras_bolsa_demanda_nacional.csv", col_names = TRUE)
# energia_b %>% filter(codigo_agente == "HLAC", fecha >= ymd("2000-01-01"), fecha <= ymd("2000-01-10"))
