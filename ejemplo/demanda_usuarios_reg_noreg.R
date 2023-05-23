source(file = "~/Documents/Lab_Financiero/Repos/xm_api_R_client/ejemplo/librerias.R")

# Sistema: agregado por todo el sistema. 
# Agente: Por agente O.o

body <- list(MetricId = "DemaReal",
            StartDate = "2015-01-25", 
            EndDate = "2015-02-25", 
            Entity = "Agente"                 
        )

body <- toJSON(body)
demanda_real <- POST("http://servapibi.xm.com.co/hourly", body = body, 
                     encode = "json", content_type("application/json"))
demanda <- fromJSON(content(demanda_real, as = "text"))

length(demanda$Items)
demanda$Metric


dts <- lapply(1:length(demanda$Items),
       function (x) {
         unlist(demanda$Items[[x]])
       }
       )
dts <- do.call(rbind, dts)
dts <- as_tibble(dts)

colnames(dts) <- str_remove_all(colnames(dts), "HourlyEntities.Values.|HourlyEntities.")

dts <- dts %>% mutate(Date = ymd(Date)) %>% mutate_at(vars(matches("Hour")), as.numeric)
dts
X11()

dts %>% pivot_longer(cols = 4:27, names_to = "hora", values_to  = "Cantidad") %>% 
  group_by(code, Date) %>% summarise(Cantidad_media = mean(Cantidad, na.rm = TRUE) / 1e6) %>% 
  ggplot(aes(Date, Cantidad_media, group = code)) + geom_line() + theme_linedraw() +
  ylab(label = "GWh") + xlab(label = "Fecha") + ggtitle(label = "Demanda promedio")



