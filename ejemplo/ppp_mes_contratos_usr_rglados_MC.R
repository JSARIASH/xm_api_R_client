direccion <- paste0(getwd(), "/ejemplo/librerias.R")
source(file = direccion)

# MC de la componente G.
# Precio promedio mensual de los contratos bilaterales dirigidos a los usuarios 
# regulados. Se tiene precio desde el 2007
# https://sinergox.xm.com.co/trpr/Paginas/Informes/PreciosContratosMercado.aspx


fecha_inicial <- ymd("2008-01-01")
fechas_consulta <- c(seq.Date(from = fecha_inicial, to = today(), by = "years"), today() - 5)
url <- "http://servapibi.xm.com.co/monthly"

mc <- vector(mode = "list", length = length(fechas_consulta) - 1)#Compras en contratos dirigida a los usuarios regulados. 
for (i in seq_len(length(fechas_consulta) - 1)) {
 body <- list(MetricId = "MC",
               StartDate = as.character(fechas_consulta[i]),
               EndDate = as.character(fechas_consulta[i + 1] - 1),
               Entity = "Sistema"
  )
  body <- toJSON(body)
  consulta <- POST(url = url, body = body, encode = "json", content_type("application/json"))
  consulta <- content(consulta, as = "parsed")
  consulta <- lapply(1:length(consulta$Items), 
                     function (x) {
                       return(unlist(consulta$Items[[x]]))                                          
                     }
  )
  consulta <- bind_rows(consulta) 
  consulta <- consulta %>% select(1, 3)
  colnames(consulta) <- c("fecha", "mc")
  mc[[i]] <- consulta
  #print(i)
}

mc <- bind_rows(mc)
mc <- mc %>% mutate(fecha = ymd(fecha), mc = as.double(mc))



