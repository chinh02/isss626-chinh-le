# Take-home Exercise 1

The report uses the 2022 records in the assignment's Thailand Road Accident
2019–2022 dataset and a six-province Greater Bangkok study window.

## Reproduce from the repository root

Required R packages: sf, tidyverse, spatstat.geom, spatstat.explore, digest,
jsonlite, knitr, and rmarkdown. The report runs its R code without displaying
setup details. Quarto source and the analysis script retain the complete code.

```powershell
Rscript scripts/download-take-home-01.R
quarto render take-home-exercise-01.qmd
quarto render take-home-exercise-01-summary.qmd
quarto render take-home-exercise.qmd
```

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
contains eight content slides plus a cover. Render the report before the slides
to refresh the shared figures, and review slide wording if the input data change.
