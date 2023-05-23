direccion <- paste0(getwd(), "/ejemplo/librerias.R")
source(file = direccion)

# Compras de energía en contratos por los agentes dirigida a usuarias regulados. 
# No tiene en cuenta los usuarios no regulados. 
# Fecha incial para la descarga = "2000-01-01"
# Fecha final de descarga. Hoy menos 5 días
# Estas fechas deben corroborse. 
#ymd("2000-01-01")

fecha_inicial <- ymd("2000-01-01")
fechas_consulta <- c(seq.Date(from = fecha_inicial, to = today(), by = "month"), today() - 5)
url <- "http://servapibi.xm.com.co/hourly"
contratacion_ur <- vector(mode = "list", length = length(fechas_consulta) - 1)#Compras en contratos dirigida a los usuarios regulados. 
for (i in seq_len(length(fechas_consulta) - 1)) {
  body <- list(MetricId = "CompContEnerReg",
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
  contratacion_ur[[i]] <- consulta
  #print(i)
}

contratacion_ur <- bind_rows(contratacion_ur)
contratacion_ur <- contratacion_ur %>% mutate(Date = ymd(Date)) %>% mutate_at(vars(matches("Hour")), as.numeric) %>% 
                   select(-Id) %>% pivot_longer(cols = -c(Date, code), names_to = "hora", values_to = "Cantidad")

colnames(contratacion_ur) <- c("fecha", "codigo_agente", "hora", "compras_contratos_rglados")
write_csv(contratacion_ur, file = "datos/compras_contratos_x_agentes_us_rglados.csv", col_names = TRUE)

# X11()
# contratacion_ur %>% group_by(code, Date) %>% 
#   summarise(cantidad_promedio = mean(Cantidad, na.rm = TRUE)) %>% 
#   ggplot(aes(Date, cantidad_promedio, group = code)) + geom_line() +
#   theme_light()
# contratacion_ur <- contratacion_ur %>% mutate(Date = ymd(Date)) %>% 
#                                 mutate_at(vars(matches("Hour")), as.numeric) %>%  select(-Id) #%>% 
#                                pivot_longer(cols = -c(Date, code), names_to = "hora", values_to = "Cantidad")
# 
# X11()
# compras_contratos_usuario_r %>% group_by(code, Date) %>% 
#   summarise(cantidad_promedio = mean(Cantidad, na.rm = TRUE)) %>% 
#   ggplot(aes(Date, cantidad_promedio, group = code)) + geom_line()
