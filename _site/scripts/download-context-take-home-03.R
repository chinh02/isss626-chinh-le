# Extra evidence for workplace demand and competing bus services.
suppressPackageStartupMessages({library(curl); library(jsonlite); library(dplyr); library(digest)})
raw_dir <- "data/take-home-03/raw"
options(timeout = 180)
employment_file <- file.path(raw_dir, "workplace-industry-2025.json")
if (!file.exists(employment_file)) {
  handle <- new_handle(useragent = "GIS course analysis", timeout = 90)
  handle_setheaders(handle, accept = "application/json")
  response <- curl_fetch_memory("https://tablebuilder.singstat.gov.sg/api/table/tabledata/C020150", handle)
  partial <- paste0(employment_file, ".part")
  writeBin(response$content, partial)
  payload <- tryCatch(fromJSON(partial), error = function(e) NULL)
  if (response$status_code == 200 && !is.null(payload) && identical(payload$StatusCode, 200L)) {
    stopifnot(file.rename(partial, employment_file))
    message("Downloaded the 2025 workplace table.")
  } else message("Workplace API unavailable: HTTP ", response$status_code,
                 "; response bytes ", length(response$content))
}

if (!nzchar(Sys.getenv("LTA_ACCOUNT_KEY")) && file.exists(".env")) readRenviron(".env")
stopifnot(nzchar(Sys.getenv("LTA_ACCOUNT_KEY")))
snapshot <- "20261104"
for (endpoint in c("BusServices", "BusRoutes", "BusStops")) {
  path <- file.path(raw_dir, paste0(endpoint, "-", snapshot, ".rds"))
  if (file.exists(path)) next
  handle <- new_handle(timeout = 90)
  handle_setheaders(handle, AccountKey = Sys.getenv("LTA_ACCOUNT_KEY"), accept = "application/json")
  pages <- list()
  skip <- 0
  repeat {
    response <- curl_fetch_memory(paste0("https://datamall2.mytransport.sg/ltaodataservice/",
                                         endpoint, "?$skip=", skip), handle)
    stopifnot(response$status_code == 200)
    records <- fromJSON(rawToChar(response$content))$value
    if (!length(records) || !nrow(records)) break
    pages[[length(pages) + 1]] <- records
    if (nrow(records) < 500) break
    skip <- skip + 500
  }
  records <- bind_rows(pages)
  stopifnot(nrow(records) > 0)
  saveRDS(records, path)
  message(endpoint, ": ", nrow(records), " records saved.")
}

files <- c("workplace-industry-2025.json", paste0(c("BusServices", "BusRoutes", "BusStops"), "-", snapshot, ".rds"))
pages <- c("https://tablebuilder.singstat.gov.sg/table/CT/C020150",
           rep("https://datamall.lta.gov.sg/content/datamall/en/dynamic-data.html", 3))
manifest <- tibble(file = files, page = pages) %>% filter(file.exists(file.path(raw_dir, file)))
manifest$bytes <- file.info(file.path(raw_dir, manifest$file))$size
manifest$sha256 <- vapply(file.path(raw_dir, manifest$file), digest, character(1), algo = "sha256", file = TRUE)
manifest$retrieved_utc <- format(file.info(file.path(raw_dir, manifest$file))$mtime, tz = "UTC", usetz = TRUE)
write.csv(manifest, "take-home-03/context-source-manifest.csv", row.names = FALSE)
