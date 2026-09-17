# OpenStreetMap roads as mapped at the start of 2022, via Overpass.
library(sf)
library(digest)
library(jsonlite)
library(purrr)
library(tibble)

raw_path <- "data/take-home-01/raw/roads-eastern-bangkok-20220101.json"
endpoint <- "https://overpass-api.de/api/interpreter"
query <- paste0('[out:json][timeout:120][date:"2022-01-01T00:00:00Z"];',
                'way["highway"~"^(motorway|trunk|primary|secondary|tertiary|unclassified)(_link)?$"]',
                '(13.65,100.65,13.80,100.80);out geom;')
if (!file.exists(raw_path)) {
  options(timeout = 180)
  download.file(paste0(endpoint, "?data=", URLencode(query, reserved = TRUE)),
                raw_path, mode = "wb", headers = c("User-Agent" = "GIS-coursework/1.0"))
}
response <- fromJSON(raw_path, simplifyVector = FALSE)
stopifnot(is.null(response$remark), length(response$elements) > 0)
checksum <- digest(raw_path, algo = "sha256", file = TRUE)
# Overpass includes a changing service timestamp even for a historical query.
# Verify the actual ways separately from the downloaded response bytes.
elements_checksum <- digest(toJSON(response$elements, auto_unbox = TRUE,
                                   digits = NA, null = "null"),
                            algo = "sha256", serialize = FALSE)
manifest_path <- "take-home-01/road-source.csv"
if (file.exists(manifest_path)) {
  recorded <- read.csv(manifest_path)
  stopifnot(length(recorded$sha256_elements) == 1,
            recorded$sha256_elements == elements_checksum)
} else {
  write.csv(data.frame(filename = basename(raw_path), url = endpoint, query,
                       snapshot = "2022-01-01T00:00:00Z", sha256 = checksum,
                       sha256_elements = elements_checksum,
                       licence = "OpenStreetMap contributors, ODbL 1.0",
                       accessed_utc = format(Sys.time(), tz = "UTC", usetz = TRUE)),
            manifest_path, row.names = FALSE)
}
ways <- response$elements
tag <- function(x, key, default = NA_character_) {
  value <- x$tags[[key]]
  if (is.null(value)) default else value
}
roads <- st_sf(tibble(
  osm_id = map_chr(ways, ~as.character(.x$id)),
  fclass = map_chr(ways, tag, key = "highway"),
  name = map_chr(ways, tag, key = "name"),
  name_en = map_chr(ways, tag, key = "name:en"),
  ref = map_chr(ways, tag, key = "ref"),
  layer = map_chr(ways, tag, key = "layer", default = "0"),
  bridge = map_chr(ways, tag, key = "bridge", default = "no"),
  tunnel = map_chr(ways, tag, key = "tunnel", default = "no"),
  node_ids = map(ways, ~as.character(unlist(.x$nodes)))),
  geometry = st_sfc(map(ways, function(w) {
    st_linestring(do.call(rbind, lapply(w$geometry, function(p) c(p$lon, p$lat))))
  }), crs = 4326))
roads <- st_transform(roads, 32647)
saveRDS(roads, "data/take-home-01/derived/eastern_bangkok_roads_raw.rds")
cat("Read", nrow(roads), "road ways from the January 2022 snapshot.\n")
