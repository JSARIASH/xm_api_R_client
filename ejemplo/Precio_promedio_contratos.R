direccion <- paste0(getwd(), "/ejemplo/librerias.R")
source(file = direccion)

# Precio promedio diario de los contratos del sistema, usuarios regulados y no regulados. 
# Fecha incial para la descarga = "2000-01-01"
# Fecha final de descarga. Hoy menos 5 días
# Estas fechas deben corroborse. 
#ymd("2000-01-01")

fecha_inicial <- ymd("2000-01-01")
fecha_final <- today() - 4
paso <- as.numeric((fecha_final - fecha_inicial) / 30)
paso <- ceiling(paso)
fechas_consulta <- fecha_inicial %m+% months(c(0, 1:paso))
fechas_consulta <- fechas_consulta[fechas_consulta <= (today())]

url_contratos_sistema <- "http://servapibi.xm.com.co/daily" # 
url_contratos_tipo_usuario <- "http://servapibi.xm.com.co/hourly" #


pp_contratos_sistema <- vector(mode = "list", length = length(fechas_consulta) - 1) # Precio promedio contratos sistema. 
pp_contratos_rglados <- vector(mode = "list", length = length(fechas_consulta) - 1) # Precio promedio contratos usuarios regulados
pp_contratos_no_rglados <- vector(mode = "list", length = length(fechas_consulta) - 1) # Precio promedio contratos usuarios no regulados 

for (i in seq_len(length(fechas_consulta) - 1)) {
  body_contratos_sistema <- list(MetricId = "PrecPromCont",
               StartDate = as.character(fechas_consulta[i]),
               EndDate = as.character(fechas_consulta[i + 1] - 1),
               Entity = "Sistema"
  )
  body_contratos_rglados <- list(MetricId = "PrecPromContRegu",
               StartDate = as.character(fechas_consulta[i]),
               EndDate = as.character(fechas_consulta[i + 1] - 1),
               Entity = "Sistema"
  )
  body_contratos_no_rglados <- list(MetricId = "PrecPromContNoRegu",
               StartDate = as.character(fechas_consulta[i]),
               EndDate = as.character(fechas_consulta[i + 1] - 1),
               Entity = "Sistema"
  )
  
  body_contratos_sistema <- toJSON(body_contratos_sistema)
  body_contratos_rglados <- toJSON(body_contratos_rglados)
  body_contratos_no_rglados <- toJSON(body_contratos_no_rglados)

  consulta_contratos_sistema <- POST(url = url_contratos_sistema, body = body_contratos_sistema, encode = "json", content_type("application/json"))
  consulta_contratos_rglados <- POST(url = url_contratos_tipo_usuario, body = body_contratos_rglados, encode = "json", content_type("application/json"))
  consulta_contratos_no_rglados <- POST(url = url_contratos_tipo_usuario, body = body_contratos_no_rglados, encode = "json", content_type("application/json"))
  
  consulta_contratos_sistema <- fromJSON(content(consulta_contratos_sistema, as = "text"))
  consulta_contratos_rglados <- fromJSON(content(consulta_contratos_rglados, as = "text"))
  consulta_contratos_no_rglados <- fromJSON(content(consulta_contratos_no_rglados, as = "text"))
  
  consulta_contratos_sistema <- lapply(1:length(consulta_contratos_sistema$Items), 
                     function (x) {
                       return(unlist(consulta_contratos_sistema$Items[[x]]))                                          
                     }
              )
  
  consulta_contratos_rglados <- lapply(1:length(consulta_contratos_rglados$Items), 
                     function (x) {
                       return(unlist(consulta_contratos_rglados$Items[[x]]))                                          
                     }
              )
  
  consulta_contratos_no_rglados <- lapply(1:length(consulta_contratos_no_rglados$Items), 
                     function (x) {
                       return(unlist(consulta_contratos_no_rglados$Items[[x]]))                                          
                     }
              )
  
  
  consulta_contratos_sistema <- bind_rows(consulta_contratos_sistema)
  consulta_contratos_rglados <- bind_rows(consulta_contratos_rglados)
  consulta_contratos_no_rglados <- bind_rows(consulta_contratos_no_rglados)
  
  colnames(consulta_contratos_sistema) <- str_remove_all(colnames(consulta_contratos_sistema), pattern = "DailyEntities.")
  colnames(consulta_contratos_rglados) <- str_remove_all(colnames(consulta_contratos_rglados), pattern = "HourlyEntities.Values.|HourlyEntities.")
  colnames(consulta_contratos_no_rglados) <- str_remove_all(colnames(consulta_contratos_no_rglados), pattern = "HourlyEntities.Values.|HourlyEntities.")
  
  
  pp_contratos_sistema[[i]] <- consulta_contratos_sistema
  pp_contratos_rglados[[i]] <- consulta_contratos_rglados
  pp_contratos_no_rglados[[i]] <- consulta_contratos_no_rglados
  
  
  
  #print(i)
}

pp_contratos_sistema <- bind_rows(pp_contratos_sistema)
pp_contratos_rglados <- bind_rows(pp_contratos_rglados)
pp_contratos_no_rglados <- bind_rows(pp_contratos_no_rglados)

pp_contratos_sistema <- pp_contratos_sistema %>% mutate(Date = ymd(Date), Value = as.numeric(Value))
pp_contratos_rglados <- pp_contratos_rglados %>% select(-c(Id, code)) %>% pivot_longer(cols = 2:25, names_to = "hora", values_to = "precio") %>% 
                        mutate(Date = ymd(Date), precio = as.numeric(precio))

pp_contratos_no_rglados <- pp_contratos_no_rglados %>% select(-c(Id, code)) %>% pivot_longer(cols = 2:25, names_to = "hora", values_to = "precio") %>% 
                        mutate(Date = ymd(Date), precio = as.numeric(precio))


# tmp_reg <- Precio_promedio_contratos_results$pp_contratos_rglados %>% 
#   mutate(
#     hora = str_remove_all(hora, "Hour"),  
#     fecha = paste(Date, hora, sep = " "), 
#     fecha = ymd_h(fecha)
#     ) %>% select(fecha, precio)
# 
# write_csv(tmp_reg, file = "datos/pp_contratos_rglados.csv", col_names = TRUE)
# 
# tmp_no_reg <- Precio_promedio_contratos_results$pp_contratos_no_rglados %>% 
#   mutate(
#     hora = str_remove_all(hora, "Hour"),  
#     fecha = paste(Date, hora, sep = " "), 
#     fecha = ymd_h(fecha)
#   ) %>% select(fecha, precio)
# 
# 
# write_csv(tmp_no_reg, file = "datos/pp_contratos_no_rglados.csv", col_names = TRUE)
# 
# 
# tmp_contratos_sistema <- Precio_promedio_contratos_results$pp_contratos_sistema %>% 
#   select(Date, Value) %>% rename(fecha = Date, precio = Value)
# 
# write_csv(tmp_contratos_sistema, file = "datos/pp_contratos_sistema.csv", col_names = TRUE)

# dev.new()
# ggplot(data = pp_contratos_rglados, aes(Date, precio)) + geom_line() +
#        geom_line(data = pp_contratos_no_rglados, aes(Date, precio), color = "red") +
#        geom_line(data = pp_contratos_sistema, aes(Date, Value), color = "royalblue") + 
#        theme_linedraw()
# 
# bind_cols(pp_contratos_rglados, p_no_reg = pp_contratos_no_rglados$precio) %>% 
#   group_by(Date) %>% summarise(precio = mean(precio, na.rm = TRUE), p_no_reg = mean(p_no_reg, na.rm = TRUE)) %>% 
#   mutate(diferencia = precio - p_no_reg) %>% ggplot(aes(Date, diferencia)) + geom_point(color = "royalblue", fill = "black", pch = 21)
# 
# 
# bind_cols(pp_contratos_rglados, p_no_reg = pp_contratos_no_rglados$precio) %>% mutate(a_m = paste(year(Date), month(Date), sep = "-")) %>%  
#   group_by(a_m) %>% summarise(precio = mean(precio, na.rm = TRUE), p_no_reg = mean(p_no_reg, na.rm = TRUE)) %>% 
#   ungroup() %>% mutate(a_m = ym(a_m), diferencia = precio  - p_no_reg) %>% 
#   ggplot(aes(a_m, diferencia)) + geom_point(size = 4, pch = 21, color = "darkblue", fill = "royalblue") +
#   theme_light() + geom_smooth(fill = "royalblue")


# pp_contratos_sistema <- pp_contratos_sistema %>% mutate(Date = ymd(Date), Value = as.numeric(Value))
# 
# X11()
# pp_contratos_sistema %>% ggplot(aes(Date, Value)) + geom_line()