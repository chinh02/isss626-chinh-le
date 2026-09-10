# Run from the project root: Rscript scripts/download-take-home-01.R
# Downloads the assignment's public accident dataset and provincial boundaries.
library(jsonlite)
library(digest)

raw_dir <- file.path("data", "take-home-01", "raw")
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
dir.create("take-home-01", showWarnings = FALSE)

sources <- data.frame(
  dataset = c("Accidents: Kaggle version 1", "Thailand ADM1: geoBoundaries 9469f09"),
  filename = c("thailand-road-accident-v1.zip", "geoBoundaries-THA-ADM1.geojson"),
  url = c(
    "https://www.kaggle.com/api/v1/datasets/download/thaweewatboy/thailand-road-accident-2019-2022?datasetVersionNumber=1",
    "https://github.com/wmgeolab/geoBoundaries/raw/9469f09/releaseData/gbOpen/THA/ADM1/geoBoundaries-THA-ADM1.geojson"
  ),
  licence = c("CC0: Public Domain (Kaggle listing)", "Open Database License 1.0"),
  stringsAsFactors = FALSE
)

for (i in seq_len(nrow(sources))) {
  destination <- file.path(raw_dir, sources$filename[i])
  if (!file.exists(destination)) {
    download.file(sources$url[i], destination, mode = "wb", method = "libcurl")
  }
}

archive <- file.path(raw_dir, sources$filename[1])
csv_members <- unzip(archive, list = TRUE)$Name
csv_members <- csv_members[grepl("\\.csv$", csv_members, ignore.case = TRUE)]
stopifnot(length(csv_members) > 0L)
missing_csv <- csv_members[!file.exists(file.path(raw_dir, csv_members))]
if (length(missing_csv) > 0L) {
  unzip(archive, files = missing_csv, exdir = raw_dir, overwrite = FALSE)
}

manifest_path <- "take-home-01/source-manifest.csv"
sources$sha256 <- vapply(file.path(raw_dir, sources$filename), function(path) {
  digest(path, algo = "sha256", file = TRUE)
}, character(1))
sources$bytes <- file.info(file.path(raw_dir, sources$filename))$size

if (file.exists(manifest_path)) {
  recorded <- read.csv(manifest_path)
  stopifnot(identical(sources$sha256, recorded$sha256))
} else {
  sources$accessed_utc <- format(Sys.time(), tz = "UTC", usetz = TRUE)
  write.csv(sources, manifest_path, row.names = FALSE)
}

metadata_path <- "take-home-01/source-metadata.json"
if (!file.exists(metadata_path)) {
  metadata <- list(
    kaggle = fromJSON("https://www.kaggle.com/api/v1/datasets/view/thaweewatboy/thailand-road-accident-2019-2022"),
    geoboundaries = fromJSON("https://www.geoboundaries.org/api/current/gbOpen/THA/ADM1/")
  )
  stopifnot(metadata$kaggle$currentVersionNumber == 1,
            metadata$geoboundaries$boundaryID == "THA-ADM1-36821470")
  write_json(metadata, metadata_path, pretty = TRUE, auto_unbox = TRUE)
}

cat("Downloaded and verified core datasets. CSV files:\n")
cat(paste(file.path(raw_dir, csv_members), collapse = "\n"), "\n")
