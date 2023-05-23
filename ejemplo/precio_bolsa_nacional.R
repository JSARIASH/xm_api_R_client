direccion <- paste0(getwd(), "/ejemplo/librerias.R")
source(file = direccion)

fecha_inicial <- ymd("2000-01-01")
fechas_consulta <- c(seq.Date(from = fecha_inicial, to = today(), by = "month"), today())
url <- "http://servapibi.xm.com.co/hourly"
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
precio_bolsa_tot <- precio_bolsa_tot %>% mutate(fecha = ymd(fecha)) %>% mutate_at(vars(matches("Hour")), as.numeric)

precio_bolsa_tot <- precio_bolsa_tot %>% select(-code)
precio_bolsa_tot <- precio_bolsa_tot %>% pivot_longer(cols = 2:25, names_to = "hora", values_to  = "precio") 
write_csv(precio_bolsa_tot, file = "datos/precio_bolsa_nacional.csv", col_names = TRUE, append = FALSE)

# dev.new()
# ggplot(data = precio_bolsa_tot, aes(fecha_hora, precio)) + geom_line(color = "navy", alpha = .81) + 
#   theme_linedraw() + xlab(label = "Fecha") + ylab(label = expression("$"/KWh)) + 
#   ggtitle(label = "Precio en bolsa de la energía eléctrica", subtitle = "Precios corrientes") + 
#   geom_rect(data = tibble(), 
#             aes(xmin = ymd_h("2015-01-01 0"), xmax = ymd_h("2016-12-01 0"),
#                 ymin = -Inf, ymax = Inf
#             ), inherit.aes = FALSE,
#             colour = NA, alpha = 0.1, fill="red"
#   ) + geom_text(aes(x = ymd_h("2014-01-01 0"), y = 2000, label = "Fenómeno del niño"))

# Se compara el precio de bolsa descargado de la API con el reportado por xm. 
# pb <- read_csv(file = "datos/precio_bolsa_nacional.csv", col_names = TRUE, col_types = c("D", "c", "d"))
# pb %>% group_by(fecha) %>% summarise(preciob = mean(precio, na.rm = TRUE)) %>% 
#   ggplot(aes(fecha, preciob)) + geom_line()

#pb <- read_csv(file = "datos/precio_bolsa_nacional.csv")
