# Take-home Exercise 3

The report and executive summary share `data/take-home-03/derived/analysis.rds`
and the additional checks in `review.rds`. Both pages check the baseline file hash.
Raw and derived datasets stay under the repository's ignored `data/` directory.

## Passenger data and API access

Request access at <https://datamall.lta.gov.sg/content/datamall/en/request-for-api.html>.
The dataset is **Passenger Volume by Origin Destination Train Stations**.
Both original files have been downloaded to `data/take-home-03/raw/`:

- `origin_destination_train_202609.zip` (or the CSV inside it)
- `origin_destination_train_202608.zip` (or the CSV inside it)

For another download, paste your DataMall **API AccountKey** into the repository's local
`.env` file, between the quotes after `LTA_ACCOUNT_KEY=`, and run the downloader.
The file is ignored by Git. An existing `LTA_ACCOUNT_KEY` environment variable
takes precedence. The key is never written to the source manifest or report.
The separate ERP 2.0 / EXTOL SDK key is not used for passenger-volume downloads.
The public data downloads do not require that key. LTA retains a limited rolling
window, so keep both original passenger archives once downloaded.

## Run from the repository root

Required R packages: `sf`, `tidyverse`, `readxl`, `curl`, `jsonlite`, `digest`,
`MCI`, `leaflet`, `htmlwidgets`, `knitr`, `scales`, `spdep`, `ggrepel`, and `units`. Quarto is required to render
the two pages. The installed GDAL must support OSM and GeoPackage.

```sh
Rscript scripts/download-take-home-03.R
Rscript scripts/prepare-take-home-03.R
Rscript scripts/download-context-take-home-03.R
Rscript scripts/check-take-home-03.R
Rscript scripts/analyse-take-home-03.R
Rscript scripts/diagnose-take-home-03.R
Rscript scripts/review-take-home-03.R
Rscript scripts/visualise-take-home-03.R
Rscript scripts/station-plots-take-home-03.R
Rscript scripts/context-plots-take-home-03.R
Rscript scripts/review-plots-take-home-03.R
quarto render take-home-exercise-03.qmd
quarto render take-home-exercise-03-summary.qmd
quarto render take-home-exercise.qmd
```

The report uses `freeze: auto` and reads saved results. Rendering never triggers
downloads, preparation or model fitting. Run the scripts explicitly after changing
data or analysis code, then render the two pages. The full calibration can take tens
of minutes; the additional review script fits only one new holdout model.
`session-info.txt` records the R and package versions used for the checks.
Plot code shown in the report is extracted from the scripts that produce the figures;
small displayed tables are evaluated directly from the saved results.

`review-take-home-03.R` compares training-only reference models, records parameter
and fold variation, tests log-error spatial association with 9,999 permutations,
and excludes all eleven TEL4/Hume/CCL6 stations together in a separate calibration.
It also holds existing line counts fixed during expansion, checks predictions across
the five fitted parameter sets, and reports housing matches and older-group coverage.
The recent-station check uses the September snapshot, not a historical back-test.
None of these checks changes the selected baseline fit.

The boundary input is the existing
`data/geospatial/MasterPlan2019SubzoneBoundaryNoSea.geojson`, available from
<https://data.gov.sg/datasets/d_8594ae9ff96d0c708bc2af633048edfb/view>.

Both passenger months are required for the finished report. If either is missing,
the analysis saves the spatial/access results with ridership unset. Restore the
original archive and rerun the analysis before rendering the report and summary.

The analysis exports `existing-network-interactions.rds`,
`expanded-network-interactions.rds`, `origins.rds` and `destinations.rds` under
`data/take-home-03/derived/`, alongside the shared report results.

The analysis also runs `scripts/explore-take-home-03.R` to save the local station
and neighbourhood comparisons. That script can be rerun on an existing model
without fitting the model again.

`scripts/demand-take-home-03.R` locates HDB population at matched blocks,
weighted by dwellings, and activities at POIs. Unallocated residents remain at
the subzone point. Locations are grouped by 500-metre cell, subzone and demand
type, preserving each subzone's totals. The selected model's four exported
tables use these demand-location IDs; `subzone_id` links them to the 332
subzones used in the catchment maps. Catchment probabilities are demand-weighted
averages of the finer locations.

Ten models compare the original demand/connection specifications with the
MRT/LRT distinction, distributed demand, office footprint area, workplace counts
and a direct-bus competition term. All use the same geographical folds. The
office and selected employment models are also checked with a second,
Ward-clustered geographical partition. Both partitions guide model development.
No station-specific observed ridership is used as attraction for new stations.

The office component uses the union of OSM `building=office` footprints from
the existing extract. Ground area is measured in square metres; missing floor
counts are not filled in. Office points retain their subzone identifiers and
add a separately normalised component to activity demand. Calibration estimates
its weight. All four networks use the same demand locations and weights.

The selected employment model replaces that office-area component with GHS 2025
employed-resident counts by planning area of workplace (SingStat C020150).
`context-take-home-03.R` distributes each named area's total over its activity
cells in proportion to POIs. The 44 matched areas contain 2,071,100 workers;
work-from-home, no-fixed-workplace and other-area categories are not allocated.
Within-area workplace locations remain estimated. Zero allocation elsewhere
does not establish zero employment. Existing resident and POI demand remains.

LTA BusServices, BusRoutes and BusStops snapshots dated 4 November 2026 describe
direct daytime services from stops within 400/500/800 metres of each station to
a later Downtown Core stop. Each regular numbered service is counted once;
letter-suffixed short-working and express variants are excluded. Published
08:31–16:59 headway midpoints supply approximate departures per hour. The shared
competition coefficient can reduce rail attraction where these alternatives
are more frequent. It is zero in all five employment-model validation folds,
so the selected model omits it. The snapshot postdates the September baseline.
`context-source-manifest.csv` records these inputs and the employment table.

The context trial and transfer-check scripts retain the individual comparisons;
the full analysis script can rerun all ten models directly from prepared inputs.

`diagnose-take-home-03.R` records the Raffles Place and Marine Parade hourly
profiles, the preceding model's demand contributions, and the comparisons with
separate office counts and office footprint area. Both pages read its shared
`station-diagnostics.rds` output. The hourly profiles describe the errors; they
are not used to predict unopened stations.

## Checks and assumptions

- `source-manifest.csv` records downloaded file hashes and source pages. OSM and
  the coordinate registry are snapshots; retain these raw files for exact reruns.
- The 2026 population notes specify Master Plan 2025. The available 2019 polygons
  have a complete one-to-one name match and are used as a geographic approximation.
- CCL6 was added to the baseline from LTA's confirmed 12 July 2026 opening. The
  downloaded station-code workbook omits those three stations.
- Passenger codes CC33 (Marina Bay) and CC34 (Bayfront) match the workbook's CE2
  and CE1 respectively, as checked against LTA's current station listings.
- HDB addresses use equal postal code and block number, at most 150 metres apart,
  with the street name matched to the property table. Ambiguous addresses are
  excluded. Street-code matches supported by at least 90% of address votes fill
  remaining gaps. Unmatched properties and agreement counts are exported. Each
  block is represented by a point within its largest recorded footprint.
- Observed rail entries are not home-to-station choices. Calibration uses station
  totals. The fitted shares are model-based estimates of origin station choice.
- Numerical checks use synthetic inputs only, in temporary storage. They test
  probabilities, MCI agreement, fixed totals, held-out isolation and aggregation.
- The scenario is a fixed-demand comparison, not a projection of future housing,
  passenger growth, train loads or travel-time savings.

Data sources retain their original licences. LTA, HDB, URA and SingStat data are
credited to their agencies; OSM requires attribution to OpenStreetMap contributors.
No account keys or temporary signed download URLs belong in Git.
