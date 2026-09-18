# Take-home Exercise 1

The report uses the 2022 records in the assignment's Thailand Road Accident
2019–2022 dataset and a six-province Greater Bangkok study window.

## Reproduce from the repository root

Required R packages: sf, tidyverse, spatstat.geom, spatstat.explore, digest,
jsonlite, knitr, rmarkdown, and spNetwork. The report runs its R code without displaying
setup details. Quarto source and the analysis script retain the complete code.

```powershell
Rscript scripts/download-take-home-01.R
quarto render take-home-exercise-01.qmd
quarto render take-home-exercise-01-summary.qmd
quarto render take-home-exercise.qmd
```

To prepare or refresh the road extension after the accident preparation:

```powershell
Rscript scripts/download-take-home-01-roads.R
Rscript scripts/analyse-take-home-01-roads.R
quarto render take-home-exercise-01.qmd
quarto render take-home-exercise-01-summary.qmd
```

The road extension focuses on a 10 km square around the annual KDE peak at
100.714 E, 13.733 N. The road downloader requests OpenStreetMap ways dated
1 January 2022 through Overpass, within 13.65–13.80 N and 100.65–100.80 E.
This leaves at least 1.5 km of surrounding roads for a 1 km network bandwidth. It retains
motorways, trunk, primary, secondary, tertiary and unclassified roads, including
their links. Residential streets, service roads and paths are outside this
network. `road-source.csv` records the query, file checksum and attribution.
The report reads `road_analysis.rds`, produced by the analysis script. When this
file is missing, the report runs the downloader and road analysis after preparing
the core accident data.

For road matching, points must be within 50 m of a selected road. Near ties
between different levels and expressway reports closest to an incompatible
road class are excluded. The full match audit is saved as CSV. Shared OSM nodes
define junctions; geometric crossings alone do not. Distances ignore one-way
restrictions and measure undirected proximity on this selected road network.
The density estimate uses the discontinuous network kernel, a quartic kernel
with a 1 km radius and sampling segments of up to 200 m. The calculation uses
kilometres and a unit bandwidth; a single-event integration check verifies that
the output is reports per km. Network edges are kept
beyond the study boundary, but unobserved accidents beyond it are not supplied.

The road-distance comparison uses the same snapped records for both straight
and network distances, with the original 1 km / 7 day thresholds and 999
agency-month time permutations. A 25 m matching limit provides a separate
sensitivity check. These exploratory comparisons are conditional on successful
road matching; omitted local streets and uncertain carriageway assignments can
change the measured distances. They are not estimates of driving time or risk
per vehicle. Numerical outputs are written to the derived-data directory.

On this computer, Rscript is available at
`C:\Program Files\R\R-4.6.1\bin\Rscript.exe` if it is not on PATH.

The download script pins Kaggle version 1 and geoBoundaries commit 9469f09.
It retrieves public files without credentials, extracts the accident CSV,
and validates download checksums against source-manifest.csv on subsequent runs.
source-metadata.json records the metadata retrieved at first acquisition.

Raw files go in data/take-home-01/raw/ and derived objects in
data/take-home-01/derived/. Both are covered by the repository's existing
data-directory ignore rule. Data are reproducible through the download script;
they are not silently supplied by a saved R workspace.

For an intentional raw-data update, review the pinned versions, metadata and
manifest together, then render the report file explicitly to execute its R
code. The site's freeze setting can otherwise reuse previous results during a
whole-site render.

The accident compilation is listed as CC0 on Kaggle. The boundary product is
ODbL 1.0, derived from OpenStreetMap/Wambacher, with its source and attribution
preserved in the report and metadata. No data were retrieved from private
accounts.

## Analysis and checks

The report prepares the source data, then runs `scripts/analyse-take-home-01.R`.
There is no saved-workspace dependency. That script reads the objects just
created by the report, computes the density surfaces and conditional Knox-style
pair tests, and writes numerical results to data/take-home-01/derived/.
It can also be run directly after the preparation has been rendered.

- Projection: WGS 84 / UTM 47N (EPSG:32647), metres.
- Gaussian KDE: 1,000 / 2,000 / 4,000 m standard deviations, 250 m pixels,
  Jones-Diggle edge correction, unit event weights. Quarterly maps use 2,000 m,
  a common scale, and divide by 90 / 91 / 92 / 92 days.
- Main pair test: unordered pairs at distance <= 1,000 m and elapsed time
  <= 7 days. Timestamps are permuted within agency-month groups; both coincident
  locations and cross-month pairs are retained. Conditional inference concerns
  the observed events only, without an external-population edge correction.
- 999 permutations, seed 62601. Upper-tail p = (1 + number of simulated counts
  >= observed) / 1,000. `sample.int()` also handles groups of size one.
- Nine-scale check: 500 / 1,000 / 2,000 m crossed with 3 / 7 / 14 days. The
  observed vector and 999 simulated vectors are standardised symmetrically,
  using all 1,000 vectors. Their maximum standardised excess gives the global
  upper-tail test and family-wise adjusted p-values for individual settings.
- Robustness checks at 1,000 m / 7 days: remove one record from each of the two
  matching coordinate/timestamp pairs; exclude pairs at identical coordinates;
  and shuffle within month only. The possible duplicates remain in the main
  analysis because distinct report IDs do not establish duplicate incidents.
- Validation: observed pair count independently recomputed from complete
  distance/time matrices; unordered pairs checked for uniqueness; the first
  permutation checked to preserve every group's timestamps; quarter totals
  checked against the prepared sample. No raw coordinates are jittered.

Generated summary CSVs and session information are saved with the derived data.
Presentation figures are in take-home-01/figures/. The revealjs presentation
contains ten content slides plus a cover. Render the report before the slides
to refresh the shared figures, and review slide wording if the input data change.

## Severity, timing and monthly map

The report also runs `scripts/analyse-take-home-01-facets.R` after saving the
prepared records. It checks non-negative integer casualty counts and compares
fatal reports (at least one recorded death), injury-only reports, and reports
with no recorded casualty. All comparisons use the same 3,599 mapped records;
the 189 unlocated reports include two fatal reports and are audited separately.
Vehicle labels describe the single category supplied per report, not every
vehicle involved. Time periods are equal six-hour blocks in local Thai time;
the night comparison uses 18:00–05:59, not measured lighting conditions.

The two severity maps divide their Gaussian KDEs by their own event counts,
then express density as percentage of the group per square kilometre. They use
the existing 2 km bandwidth, 250 m cells and Jones-Diggle edge correction. The
script checks that each density integrates to its event count within 2%.
These maps compare distributions; they are not maps of fatality probability.
Province, agency, vehicle and night/day summaries provide descriptive checks
of how the record mix affects comparisons. No new significance tests are run.
Counts and deaths are kept distinct, and the outcome groups sum to the full
sample. CSV tables and the RDS results are saved in the derived-data directory.

`take-home-01/explorer-template.html` supplies the monthly map's HTML, CSS and
JavaScript. The R script inserts display coordinates, province boundaries and
counts to create the self-contained `accident-explorer.html`. It requires no
external map tiles or JavaScript packages. Display boundaries are simplified
by 100 m; record selection continues to use the original boundaries. Each
animation frame shows one calendar month, never an interpolated location or
cumulative count. Playback starts only on request, can be paused, and stops
after December. Province totals remain visible as a numerical alternative to
overlapping points. The report explicitly includes the generated HTML as a
Quarto resource so it is copied into the published site.
