# Take-home Exercise 3

The exercise has three pages:

| Page | What it contains |
|---|---|
| `take-home-exercise-03-workflow.qmd` | The full workflow: data preparation, housing matching, Huff model, validation, network comparisons and plotting code. The chunks run in order and save every result and figure. |
| `take-home-exercise-03.qmd` | The technical report. It reads the workflow's saved results and shows the key steps and findings. |
| `take-home-exercise-03-summary.qmd` | The executive summary slides, which read the same saved results. |

From the repository root, render the workflow first, then the report and the summary:

```sh
quarto render take-home-exercise-03-workflow.qmd
quarto render take-home-exercise-03.qmd
quarto render take-home-exercise-03-summary.qmd
```

The workflow uses the downloaded inputs in `data/take-home-03/raw/` and
`data/geospatial/MasterPlan2019SubzoneBoundaryNoSea.geojson`. Both LTA passenger
archives are needed: `origin_destination_train_202609.zip` and
`origin_destination_train_202608.zip`. Keep the ZIP files: the hourly station
comparison reads directly from them.
Rendering does not download new data or require an API key.

The input layout under `data/take-home-03/raw/` is:

| Input | File or folder used by the QMD |
|---|---|
| September and August train journeys | `origin_destination_train_202609.zip`, `origin_destination_train_202608.zip` |
| LTA station footprints | Shapefile and its companion files inside `stations/` |
| LTA station codes | `station-codes/Train Station Codes and Chinese Names.xls` |
| LTA bus-stop locations | Shapefile and its companion files inside `bus-stops/` |
| June 2026 population | `population-2026/respopagesextod2026.csv` |
| Station coordinate registry | `all-stations.csv`, `future-stations.csv` |
| HDB buildings and properties | `hdb-buildings.geojson`, `hdb-properties.csv` |
| OpenStreetMap extract | `osm.osm.pbf`; the QMD creates `osm-singapore.gpkg` if needed |
| GHS 2025 workplace table | `workplace-industry-2025.json` |
| Bus service snapshots, 4 November 2026 | `BusRoutes-20261104.rds`, `BusServices-20261104.rds`, `BusStops-20261104.rds` |

The [source manifest](source-manifest.csv) and
[workplace and bus manifest](context-source-manifest.csv) record source pages,
retrieval dates and file hashes. Extract the LTA shapefile, station-code and
population archives into the folders above. The bus RDS files contain the
collected DataMall records as R tables, rather than downloadable ZIP archives.
Exact reruns need the retained snapshots; newly downloaded records may differ.
Raw inputs remain local. LTA passenger and bus downloads require DataMall access.

The first run recalculates the model and can take a while. Expensive fitting
chunks are cached locally. Use `--no-cache` when a full recalculation is
needed. Raw data, saved results, caches and temporary inspection tools stay
inside the ignored `data/` directory.

The workflow saves `analysis.rds`, `review.rds` and `station-diagnostics.rds` in
`data/take-home-03/derived/`, along with the four model tables required by the
handout. The summary reads those results and checks that its review matches
the workflow's analysis. Summary charts are exported by the workflow too.

Required R packages: `sf`, `tidyverse`, `readxl`, `jsonlite`, `digest`, `MCI`,
`leaflet`, `htmlwidgets`, `knitr`, `scales`, `spdep`, `ggrepel`, `units` and `pacman`.
`session-info.txt` records the versions used. GDAL needs OSM support to create
the GeoPackage if that local extract is missing.

Source links and modelling assumptions are listed in the report. Keep
`.env`, account keys, raw downloads and temporary inspection files out of Git.
