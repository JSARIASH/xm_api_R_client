# CLAUDE.md

Este archivo le indica a Claude Code (claude.ai/code) cómo trabajar con el código de este repositorio.

## Qué es este repositorio

Scripts de R que extraen datos de las APIs del mercado mayorista de electricidad de Colombia y guardan instantáneas en CSV dentro de `datos/`. Se usan dos APIs:

- **API XM** (`http(s)://servapibi.xm.com.co`) — endpoints `/hourly`, `/daily`, `/monthly`, `/lists`. POST con cuerpo JSON que incluye un `MetricId`. Esta es la API principal.
- **API SIMEM** (`https://www.simem.co/backend-files/api/PublicData`) — GET con parámetros de consulta, basada en `datasetId`. Solo explorada en `simem.R`.

Ninguna de las dos APIs requiere autenticación.

## Cómo ejecutar los scripts

Abrir el proyecto en RStudio usando `xm_api_R_client.Rproj` (esto fija el directorio de trabajo en la raíz del proyecto, que todos los scripts asumen).

Cada archivo en `ejemplo/` es un script independiente que se ejecuta con `source`. Todos comienzan con:

```r
direccion <- paste0(getwd(), "/ejemplo/librerias.R")
source(file = direccion)
```

Por lo tanto, el directorio de trabajo **debe ser la raíz del proyecto** — `getwd()` se usa para ubicar `ejemplo/librerias.R` y para escribir en `datos/`. Ejecutar desde cualquier otro directorio romperá las rutas relativas.

No hay suite de pruebas, pasos de compilación ni paquete — estos son scripts, no una librería.

## Anatomía de un script (el patrón común)

La mayoría de los scripts en `ejemplo/` son variaciones menores de la misma estructura. Reconocerla hace que editar uno sea sencillo:

1. **`source` de `librerias.R`** — carga `httr`, `jsonlite`, `dplyr`, `tidyr`, `lubridate`, `stringr`, `readr`, `ggplot2`, y lee `datos/listado_metricas.csv` en `listado_metricas` (el catálogo de `MetricId`s válidos y sus valores de `Entity`).
2. **Construir un vector de fechas mensual** — `fechas_consulta <- c(seq.Date(fecha_inicial, today(), by = "month"), today() - 5)`. La API XM tiene un límite de rango de fechas por solicitud, por lo que las peticiones se dividen mensualmente en un bucle.
3. **POST por fragmento** — el cuerpo es `list(MetricId=..., StartDate=..., EndDate=..., Entity=...)` pasado por `toJSON()`.
4. **Desempacar el JSON** — la estructura de la respuesta es `$Items[[i]]$HourlyEntities[[1]]$Values$Hour01..Hour24` para `/hourly`, o `$Items[[i]]$DailyEntities$Value` para `/daily`, o `$Items[[i]]$MonthlyEntities$Value` para `/monthly`. Los scripts aplanan con `unlist()` + `bind_rows()` y eliminan el prefijo con `str_remove_all(colnames(.), "HourlyEntities.Values.|HourlyEntities.")` (o el equivalente con `DailyEntities.`).
5. **Pivot + conversión de tipos** — `pivot_longer` en las columnas `Hour01..Hour24`, coerción con `mutate(Date = ymd(Date))` y `mutate_at(vars(matches("Hour")), as.numeric)`.
6. **Escribir CSV** — en `datos/`. La mayoría de los scripts sobreescriben; `precio_bolsa_nacional.R` es la excepción — lee el CSV existente, encuentra la fecha máxima y solo agrega las filas nuevas.

Para agregar una nueva métrica, el camino más rápido es copiar el script más parecido (p. ej. `compras_bolsa_x_agentes.R` para `/hourly` + `Entity=Agente`, o `proyeccion_upme.R` para `/monthly`) y cambiar `MetricId`, `Entity` y el nombre del archivo de salida.

## Datos

`datos/` es el directorio de salida. Todo en `datos/*.csv` está en el gitignore — excepto `listado_metricas.csv`, que SÍ está commiteado (es el catálogo de métricas, no una salida generada, y es necesario para `librerias.R` en un clon limpio). `datos/README` documenta el contenido de cada CSV de salida y las diferencias sutiles entre métricas con nombres similares (p. ej. compras para *demanda nacional* vs *usuarios regulados*).

Varios archivos de salida pesan cientos de MB — tener esto en cuenta antes de leerlos con `Read`.

## Particularidades de la API

- Los errores HTTP no lanzan excepciones; verificar `response$status_code != 200` explícitamente. `precio_bolsa_nacional.R` hace esto con un `warning()`; la mayoría de los demás scripts no lo hacen, por lo que es posible obtener un resultado parcial silencioso.
- `Entity` debe coincidir con lo que soporta la métrica (`Sistema`, `Agente`, `Recurso`, etc.) — `listado_metricas` lista las combinaciones válidas.
- El patrón `today() - 5` al final del vector de fechas refleja que XM publica los datos con unos días de retraso — consultar hasta hoy devuelve días vacíos al final.
- `simem.R` es exploratorio / borrador — los patrones allí aún no están generalizados en un script reutilizable como los archivos de `ejemplo/`.

## Archivos fuera del patrón principal

- `plot_ggplot2_parametrizado.R` — no tiene relación con la API XM. Fragmentos de estilo ggplot para figuras de artículos (usa `ggflags`, `extrafont`).
- `ejemplo/request_tutorial.R` — recorrido anotado de la API XM, conservado como referencia. No es usado por otros scripts.
- `ejemplo/resultado_peticiones.R` — ayudantes de inspección / borrador.
