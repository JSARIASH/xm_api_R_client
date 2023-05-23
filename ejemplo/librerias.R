# Librerias

library(jsonlite)
library(rjson)
library(lubridate)
library(dplyr)
library(tidyr)
library(tidyr)
library(stringr)
library(httr)
library(ggplot2)
library(readr)
direccion <- paste0(getwd(), "/datos/listado_metricas.csv")
listado_metricas <- read_csv(file = direccion, 
                             col_names = TRUE, col_types = as.list(c(rep("c", 4), "d", rep("c", 5))))
