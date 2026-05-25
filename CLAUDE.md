# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this repo is

R client scripts that pull data from Colombia's wholesale electricity market APIs and write CSV snapshots to `datos/`. Two APIs are used:

- **XM API** (`http(s)://servapibi.xm.com.co`) — endpoints `/hourly`, `/daily`, `/monthly`, `/lists`. POST + JSON body with a `MetricId`. This is the primary API.
- **SIMEM API** (`https://www.simem.co/backend-files/api/PublicData`) — GET with query params, `datasetId`-based. Only explored in `simem.R`.

Neither API requires authentication.

## Running scripts

Open the project in RStudio via `xm_api_R_client.Rproj` (sets the working directory to the project root, which every script assumes).

Each file in `ejemplo/` is a standalone, source-and-run script. They all start with:

```r
direccion <- paste0(getwd(), "/ejemplo/librerias.R")
source(file = direccion)
```

So the working directory **must be the project root** — `getwd()` is used to locate `ejemplo/librerias.R` and to write into `datos/`. Running from any other cwd will break the relative paths.

There is no test suite, no build step, no package — these are scripts, not a library.

## Script anatomy (the common pattern)

Most scripts in `ejemplo/` are minor variations on the same shape. Recognising the shape makes editing one trivial:

1. **Source `librerias.R`** — loads `httr`, `jsonlite`, `dplyr`, `tidyr`, `lubridate`, `stringr`, `readr`, `ggplot2`, and reads `datos/listado_metricas.csv` into `listado_metricas` (the catalog of valid `MetricId`s and their `Entity` values).
2. **Build a monthly date vector** — `fechas_consulta <- c(seq.Date(fecha_inicial, today(), by = "month"), today() - 5)`. The XM API has a per-request date-range limit, so requests are chunked monthly and looped.
3. **POST per chunk** — body is `list(MetricId=..., StartDate=..., EndDate=..., Entity=...)` passed through `toJSON()`.
4. **Unwrap the JSON** — the response shape is `$Items[[i]]$HourlyEntities[[1]]$Values$Hour01..Hour24` for `/hourly`, or `$Items[[i]]$DailyEntities$Value` for `/daily`, or `$Items[[i]]$MonthlyEntities$Value` for `/monthly`. The scripts flatten with `unlist()` + `bind_rows()` and strip the prefix with `str_remove_all(colnames(.), "HourlyEntities.Values.|HourlyEntities.")` (or the `DailyEntities.` equivalent).
5. **Pivot + cast** — `pivot_longer` the `Hour01..Hour24` columns, coerce with `mutate(Date = ymd(Date))` and `mutate_at(vars(matches("Hour")), as.numeric)`.
6. **Write CSV** — into `datos/`. Most scripts overwrite; `precio_bolsa_nacional.R` is the exception — it reads the existing CSV, finds the max date, and appends only new rows.

When adding a new metric, the fastest path is to copy the closest existing script (e.g. `compras_bolsa_x_agentes.R` for `/hourly` + `Entity=Agente`, `proyeccion_upme.R` for `/monthly`) and change `MetricId`, `Entity`, and the output filename.

## Data

`datos/` is the output directory. Everything in `datos/*.csv` is gitignored — except `listado_metricas.csv`, which IS committed (it is the metric catalog, not generated output, and is needed by `librerias.R` on a fresh clone). `datos/README` documents what each output CSV contains and the subtle differences between similarly-named metrics (e.g. compras for *demanda nacional* vs *usuarios regulados*).

Several output files are hundreds of MB — be aware before reading them with `Read`.

## API quirks worth knowing

- HTTP errors don't throw; check `response$status_code != 200` explicitly. `precio_bolsa_nacional.R` does this with a `warning()`; most other scripts do not, so a silent partial result is possible.
- `Entity` must match what the metric supports (`Sistema`, `Agente`, `Recurso`, etc.) — `listado_metricas` lists valid combinations.
- The `last today() - 5` pattern in date vectors reflects that XM publishes data with a few days' lag — querying right up to today returns empty days at the tail.
- `simem.R` is exploratory / scratch — the patterns there are not yet generalised into a reusable script the way the `ejemplo/` files are.

## Files outside the main pattern

- `plot_ggplot2_parametrizado.R` — unrelated to the XM API. ggplot styling snippets for paper figures (uses `ggflags`, `extrafont`).
- `ejemplo/request_tutorial.R` — annotated walkthrough of the XM API, kept as reference. Not used by other scripts.
- `ejemplo/resultado_peticiones.R` — scratch / inspection helpers.
