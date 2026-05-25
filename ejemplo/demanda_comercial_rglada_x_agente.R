direccion <- paste0(getwd(), "/ejemplo/librerias.R")
source(file = direccion)

# Compras de energía en contratos por los agentes dirigida a usuarias regulados. 
# No tiene en cuenta los usuarios no regulados. No se reporta en el SINERGOX
# Fecha incial para la descarga = "2000-01-01"
# Fecha final de descarga. Hoy menos 5 días
# Estas fechas deben corroborse. 
#ymd("2000-01-01")

fecha_inicial <- ymd("2022-01-01")
fechas_consulta <- c(seq.Date(from = fecha_inicial, to = today(), by = "month"), today() - 5)
url <- "http://servapibi.xm.com.co/hourly"

dmanda_cmercia_agente <- vector(mode = "list", length = length(fechas_consulta) - 1)#Compras en contratos dirigida a los usuarios regulados. 
for (i in seq_len(length(fechas_consulta) - 1)) {
  body <- list(MetricId = "DemaComeReg",
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
  dmanda_cmercia_agente[[i]] <- consulta
  #print(i)
}
dmanda_cmercia_agente <- bind_rows(dmanda_cmercia_agente)
dmanda_cmercia_agente <- dmanda_cmercia_agente %>% 
  mutate_at(vars(matches("Hour")), as.numeric) %>% 
  mutate(Date = ymd(Date)) %>% rename(fecha = Date) %>% 
  select(-Id) %>% 
  pivot_longer(
    cols = -c(fecha, code), names_to = "hora", values_to = "dmda_cmercial_rglada_x_agnt"
    )
write_csv(dmanda_cmercia_agente, 
          file = "datos/demanda_comercial_rglada_x_agente_papaer.csv", 
          col_names = TRUE, append = FALSE
          )

