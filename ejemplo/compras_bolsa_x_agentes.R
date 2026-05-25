source(paste0(getwd(), "/ejemplo/librerias.R"))
source(paste0(getwd(), "/descargar_xm.R"))

# Compras de energía en bolsa para atender la demanda nacional (regulada + no regulada).
# Equivale a Compras_Bolsa_Nacional_(kWh)_*.csv en SINERGOX.

compras_bolsa <- descargar_xm(
  metric_id      = "CompBolsNaciEner",
  entity         = "Agente",
  fecha_inicial  = ymd("2000-01-01"),
  archivo_salida = "datos/compras_bolsa_x_agentes.csv"
)
