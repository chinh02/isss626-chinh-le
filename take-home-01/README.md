# Take-home Exercise 1

The report uses the 2022 records in the assignment's Thailand Road Accident
2019–2022 dataset and a six-province Greater Bangkok study window.

## Reproduce from the repository root

Required R packages: sf, tidyverse, spatstat.geom, digest, jsonlite, knitr,
and rmarkdown. The report runs its R code without displaying setup details.

```powershell
Rscript scripts/download-take-home-01.R
quarto render take-home-exercise-01.qmd
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
