direccion <- paste0(getwd(), "/ejemplo/librerias.R")
source(file = direccion)

fecha_inicial <- ymd("2015-01-01")
fechas_consulta <- c(seq.Date(from = fecha_inicial, to = today(), by = "12 month"))
url <- "http://servapibi.xm.com.co/monthly"
proyeccion_upme <- vector(mode = "list", length = length(fechas_consulta) - 1)#Compras en contratos dirigida a los usuarios regulados. 
for (i in seq_len(length(fechas_consulta) - 1)) {
  body <- list(MetricId = "EscDemUPMEMedio",
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
  proyeccion_upme[[i]] <- consulta
  print(i)
}

proyeccion_upme <- bind_rows(proyeccion_upme)
proyeccion_upme <- proyeccion_upme %>% 
  transmute(
    fecha = ymd(Date), 
    demanda_mes = as.double(MonthlyEntities.Value)
  )
proyeccion_upme <- proyeccion_upme %>% mutate(demanda_mes_GWh = demanda_mes / 1e6)
proyeccion_upme %>% ggplot(aes(fecha, demanda_mes_GWh)) + 
  geom_line()
modelo <- lm(data = proyeccion_upme, demanda_mes_GWh ~ fecha)

proyeccion_upme %>% ggplot(aes(fecha, demanda_mes_GWh)) + 
  geom_line() +
  geom_abline(intercept = modelo$coefficients[1], slope = modelo$coefficients[2])


predict(modelo, data.frame(fecha = ymd("2026-01-01")))