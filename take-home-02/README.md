# Take-home Exercise 2

The report examines the course-provided ACLED Myanmar records from 1 January
2021 to 30 September 2025 using MIMU township boundaries v9.4.

From the repository root:

```powershell
Rscript scripts/analyse-take-home-02.R
quarto render take-home-exercise-02.qmd
quarto render take-home-exercise-02-summary.qmd
quarto render take-home-exercise.qmd
```

On this machine Rscript is at `C:\Program Files\R\R-4.6.1\bin\Rscript.exe`.
The report calls `scripts/visualise-take-home-02.R` to make the figures and the
monthly explorer. It reruns the analysis if its RDS output is missing or older
than the analytical script or primary input files. A direct script run always
rebuilds the outputs, reusing space-time permutations only when the data,
neighbourhood, function, parameters and package version match their cache key.
Named chunks in the scripts supply the visible code beside each part of the
report, so the displayed code stays in sync with the calculations and figures.
Read the generated session-info.txt for package versions.

## Inputs

- `data/take-home-02/raw/aspatial/ACLED_Data_Myanmar_Jan2021-Sep2025.csv`
- `data/take-home-02/raw/geospatial/mmr_polbnda_adm3_250k_mimu_1.shp`, with its
  `.dbf`, `.shx`, `.prj` and `.cst` companions.

Get the ACLED file from the assignment's eLearn resources. The supplied ZIP can
also be used when the CSV has not been extracted; its local filename is
`ACLED_Data_Myanmar_Jan2021-Sep2025 (1).zip`.

Boundary source: https://geonode.themimu.info/layers/geonode:mmr_polbnda_adm3_250k_mimu_1
Use the **Zipped Shapefile** download. The raw data and event-level audit files
remain under the repository's existing `/data/` ignore rule. MIMU's source page
states an online-use restriction; confirm applicable course permission before
publishing the maps. This implementation only prepares local outputs.

## Decisions

- Retain Myanmar battles, explosions/remote violence and violence against civilians.
- Match the named township and state/region. Drop unnamed townships and precision-3
  locations. Apply only the verified Pangsang spelling alias. Retain reported
  township names in a small number of coordinate disagreements; save their IDs.
- Complete all 330 townships and 57 months. Zero means no retained record.
- Queen neighbours use a one-metre snap tolerance in a Myanmar-centred Albers
  projection. Three island townships have no shared-boundary neighbours; keep
  them in the data but leave them unclassified in Queen-based inference.
- Local Moran and Gi* use 999 conditional permutations, two-sided p-values and
  row-standardised weights. Gi* includes self. Main maps use p < 0.05;
  Benjamini–Hochberg adjustments are reported as a sensitivity check.
- Space-time Gi* uses the same township and Queen neighbours in the current and
  preceding month, relative to the entire 18,810-bin cube. The first month uses
  only current-month neighbours. Permutations condition on the focal count and
  sample from the remaining cube; this exchangeability assumption is exploratory.
- Mann–Kendall is applied to Gi* scores, not raw event counts. A six-month circular
  block bootstrap of Sen-detrended scores (999 replications) checks short-term
  serial dependence. It does not remove reporting changes or establish that
  seasonal effects are absent. BH adjustments are across township trend tests;
  bin-level adjustments are within each month.
- EHSA rules follow the ArcGIS reference, coded explicitly to preserve the sign,
  final-month and 90% conditions. Synthetic histories check those conditions.
  The handout's trend-significant map is accompanied by the complete category
  counts, so persistent patterns without a significant trend are not lost.
- Civilian targeting is a subset of the same retained event categories, not a
  separate fatality measure. Cases are selected as the highest-count township
  per significant hotspot class (up to two), plus the highest-count township overall.

Analytical outputs are under `data/take-home-02/derived/`; the report and slides
use the same RDS results. The explorer contains township aggregates only.
