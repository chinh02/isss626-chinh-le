# Run from the repository root: Rscript scripts/download-take-home-03.R
suppressPackageStartupMessages({
  library(curl)
  library(jsonlite)
  library(digest)
})

raw_dir <- "data/take-home-03/raw"
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
dir.create("take-home-03", showWarnings = FALSE)
options(timeout = 900)

sources <- data.frame(
  file = c("stations.zip", "station-codes.zip", "bus-stops.zip",
           "population-2026.zip", "future-stations.csv", "all-stations.csv",
           "hdb-buildings.geojson", "hdb-properties.csv", "osm.osm.pbf"),
  url = c(
    "https://datamall.lta.gov.sg/content/dam/datamall/datasets/Geospatial/TrainStation_Sep2026.zip",
    "https://datamall.lta.gov.sg/content/dam/datamall/datasets/Geospatial/Train%20Station%20Codes%20and%20Chinese%20Names.zip",
    "https://datamall.lta.gov.sg/content/dam/datamall/datasets/Geospatial/BusStopLocation_Sep2026.zip",
    "https://www.singstat.gov.sg/files/c6755f50-4bfb-4bfa-b7d3-00259b9509ed.zip",
    "https://raw.githubusercontent.com/elliotwutingfeng/singapore_train_station_coordinates/main/future_stations.csv",
    "https://raw.githubusercontent.com/elliotwutingfeng/singapore_train_station_coordinates/main/operational_and_future_stations.csv",
    "d_16b157c52ed637edd6ba1232e026258d",
    "d_17f5382f26140b1fdae0ba2ef6239d2f",
    "https://download.geofabrik.de/asia/malaysia-singapore-brunei-latest.osm.pbf"
  ),
  page = c(
    rep("https://datamall.lta.gov.sg/content/datamall/en/static-data.html", 3),
    "https://www.singstat.gov.sg/find-data/explore-data-themes/population/geographic-distribution/latest-news-data",
    rep("https://github.com/elliotwutingfeng/singapore_train_station_coordinates", 2),
    "https://data.gov.sg/datasets/d_16b157c52ed637edd6ba1232e026258d/view",
    "https://data.gov.sg/datasets/d_17f5382f26140b1fdae0ba2ef6239d2f/view",
    "https://download.geofabrik.de/asia/malaysia-singapore-brunei.html"
  )
)

fetch <- function(url, path) {
  if (file.exists(path)) return(invisible(path))
  message("Downloading ", basename(path))
  # Resolve temporary government download URLs without saving their credentials.
  if (startsWith(url, "d_")) {
    response <- fromJSON(rawToChar(curl_fetch_memory(paste0(
      "https://api-open.data.gov.sg/v1/public/api/datasets/", url, "/poll-download"))$content))
    stopifnot(response$code == 0, length(response$data$url) == 1)
    url <- response$data$url
  }
  partial <- paste0(path, ".part")
  curl_download(url, partial, quiet = TRUE)
  stopifnot(file.info(partial)$size > 0, file.rename(partial, path))
  invisible(path)
}

failures <- character()
for (i in seq_len(nrow(sources))) {
  tryCatch(fetch(sources$url[i], file.path(raw_dir, sources$file[i])),
           error = function(e) {
             failures <<- c(failures, sources$file[i])
             message("Unavailable: ", sources$file[i], " (", conditionMessage(e), ")")
           })
}

# LTA provides these two months through its account-based download service.
# Alternatively, place the original ZIP files in raw_dir; never put a key in code.
if (!nzchar(Sys.getenv("LTA_ACCOUNT_KEY")) && file.exists(".env")) {
  readRenviron(".env")
}
account_key <- Sys.getenv("LTA_ACCOUNT_KEY")
for (month in c("202609", "202608")) {
  path <- file.path(raw_dir, paste0("origin_destination_train_", month, ".zip"))
  csv <- sub("\\.zip$", ".csv", path)
  if (!file.exists(path) && !file.exists(csv) && nzchar(account_key)) {
    http_status <- NA_integer_
    tryCatch({
      handle <- new_handle(failonerror = FALSE, timeout = 60)
      handle_setheaders(handle, AccountKey = account_key, accept = "application/json")
      response <- curl_fetch_memory(paste0(
        "https://datamall2.mytransport.sg/ltaodataservice/PV/ODTrain?Date=", month), handle)
      http_status <- response$status_code
      stopifnot(response$status_code == 200)
      link <- fromJSON(rawToChar(response$content))$value$Link
      stopifnot(length(link) == 1)
      fetch(link, path)
    }, error = function(e) {
      if (!is.na(http_status) && http_status != 200) {
        message("LTA passenger request for ", month, " returned HTTP ", http_status, ".")
      } else {
        message("Passenger download failed for ", month, "; no completed file was saved.")
      }
    })
  }
  if (!file.exists(path) && !file.exists(csv)) {
    message("NEEDED: origin_destination_train_", month, ".zip or .csv in ", raw_dir)
  }
}

for (file in sources$file[grepl("\\.zip$", sources$file)]) {
  archive <- file.path(raw_dir, file)
  destination <- file.path(raw_dir, sub("\\.zip$", "", file))
  if (file.exists(archive) && !dir.exists(destination)) {
    unzip(archive, exdir = destination, overwrite = FALSE)
  }
}

passenger_sources <- data.frame(
  file = paste0("origin_destination_train_", c("202609", "202608"), ".zip"),
  url = paste0("https://datamall2.mytransport.sg/ltaodataservice/PV/ODTrain?Date=", c("202609", "202608")),
  page = "https://datamall.lta.gov.sg/content/datamall/en/dynamic-data.html"
)
sources <- rbind(sources, passenger_sources)
manifest <- sources[file.exists(file.path(raw_dir, sources$file)), ]
manifest$bytes <- file.info(file.path(raw_dir, manifest$file))$size
manifest$sha256 <- vapply(file.path(raw_dir, manifest$file), digest,
                          character(1), algo = "sha256", file = TRUE)
manifest$retrieved_utc <- format(file.info(file.path(raw_dir, manifest$file))$mtime,
                                tz = "UTC", usetz = TRUE)
write.csv(manifest, "take-home-03/source-manifest.csv", row.names = FALSE)
if (length(failures)) message("Downloads still needed: ", paste(failures, collapse = ", "))
