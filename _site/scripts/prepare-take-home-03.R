# Run from the repository root after download-take-home-03.R.
# ---- th03-packages ----
suppressPackageStartupMessages({
  library(sf)
  library(tidyverse)
  library(readxl)
})
base <- "data/take-home-03"
raw_dir <- file.path(base, "raw")
out <- file.path(base, "derived")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
normalise <- function(x) gsub("[^A-Z0-9]", "", toupper(x))

# ---- th03-population ----
subzones <- st_read("data/geospatial/MasterPlan2019SubzoneBoundaryNoSea.geojson", quiet = TRUE) %>%
  st_make_valid() %>% st_transform(3414) %>%
  transmute(origin_id = SUBZONE_C, subzone = SUBZONE_N, planning_area = PLN_AREA_N,
            region = REGION_N, key = normalise(paste(PLN_AREA_N, SUBZONE_N)))
population <- read_csv(file.path(raw_dir, "population-2026/respopagesextod2026.csv"),
                       show_col_types = FALSE)
stopifnot(!anyNA(population$Pop), all(population$Time == 2026),
          !anyDuplicated(population[c("PA", "SZ", "AG", "Sex", "TOD", "Time")]))
population <- population %>%
  mutate(key = normalise(paste(PA, SZ)), age = parse_number(AG), hdb = startsWith(TOD, "HDB")) %>%
  group_by(key) %>%
  summarise(residents = sum(Pop), hdb_residents = sum(Pop[hdb]),
            older_hdb = sum(Pop[hdb & age >= 65]), .groups = "drop")
stopifnot(setequal(population$key, subzones$key), !anyDuplicated(subzones$key))
subzones <- left_join(subzones, population, by = "key") %>%
  mutate(older_share = if_else(hdb_residents > 0, older_hdb / hdb_residents, NA_real_))
origins <- st_point_on_surface(subzones) %>% arrange(origin_id)

# ---- th03-station-codes ----
codebook <- read_excel(file.path(raw_dir, "station-codes/Train Station Codes and Chinese Names.xls")) %>%
  transmute(code = stn_code, station = mrt_station_english, line = mrt_line_english)
coordinates <- read_csv(file.path(raw_dir, "all-stations.csv"), show_col_types = FALSE)
future <- read_csv(file.path(raw_dir, "future-stations.csv"), show_col_types = FALSE)
# CCL6 opened on 12 July 2026; the downloaded codebook omits these three stations.
ccl6 <- coordinates %>% filter(station_code %in% c("CC30", "CC31", "CC32")) %>%
  transmute(code = station_code, station = station_name, line = "Circle Line")
codebook <- bind_rows(codebook, anti_join(ccl6, codebook, by = "code")) %>%
  mutate(station_id = normalise(station), network = "Existing")
additions <- future %>%
  filter((grepl("^J[SEW][0-9]+$", station_code)) |
           station_code %in% paste0("CR", 2:13)) %>%
  transmute(code = station_code, station = station_name,
            line = if_else(startsWith(code, "J"), "Jurong Region Line", "Cross Island Line"),
            station_id = normalise(station),
            network = if_else(startsWith(code, "J"), "JRL", "CRL Phase 1"))
stopifnot(sum(additions$network == "JRL") == 24, sum(additions$network == "CRL Phase 1") == 12)
code_crosswalk <- bind_rows(codebook, additions)
stopifnot(!anyDuplicated(code_crosswalk$code))

# ---- th03-station-locations ----
station_file <- list.files(file.path(raw_dir, "stations"), "\\.shp$", recursive = TRUE, full.names = TRUE)
footprints <- st_read(station_file, quiet = TRUE) %>% st_make_valid() %>% st_transform(3414) %>%
  group_by(STN_NAM) %>% summarise() %>% st_point_on_surface()
xy <- st_coordinates(footprints)
official_xy <- tibble(code = footprints$STN_NAM, x = xy[, 1], y = xy[, 2])
points <- st_as_sf(coordinates, coords = c("lon", "lat"), crs = 4326) %>% st_transform(3414)
xy <- st_coordinates(points)
registry_xy <- tibble(code = coordinates$station_code, registry_x = xy[, 1], registry_y = xy[, 2])
station_rows <- code_crosswalk %>% left_join(official_xy, by = "code") %>%
  left_join(registry_xy, by = "code") %>%
  mutate(x = if_else(network == "Existing", coalesce(x, registry_x), registry_x),
         y = if_else(network == "Existing", coalesce(y, registry_y), registry_y))
stopifnot(!anyNA(station_rows[c("x", "y")]))
# Keep the same representative location for an existing interchange in every scenario.
station_points <- station_rows %>% arrange(network != "Existing", code) %>%
  group_by(station_id) %>% summarise(station = first(station), x = first(x), y = first(y),
                                    existing = any(network == "Existing"), .groups = "drop") %>%
  st_as_sf(coords = c("x", "y"), crs = 3414)

# ---- th03-bus-connections ----
bus_file <- list.files(file.path(raw_dir, "bus-stops"), "\\.shp$", recursive = TRUE, full.names = TRUE)
bus_stops <- st_read(bus_file, quiet = TRUE) %>%
  filter(STATUS == "OP") %>% distinct(BUS_STOP_N, .keep_all = TRUE) %>% st_transform(3414)
for (radius in c(400, 500, 800)) {
  station_points[[paste0("bus_", radius)]] <- lengths(st_is_within_distance(station_points, bus_stops, dist = radius))
}
networks <- list(Existing = "Existing", JRL = c("Existing", "JRL"),
                 CRL = c("Existing", "CRL Phase 1"), Both = c("Existing", "JRL", "CRL Phase 1"))
stations <- lapply(networks, function(included) {
  attributes <- code_crosswalk %>% filter(network %in% included) %>% group_by(station_id) %>%
    summarise(lines = n_distinct(line), codes = paste(code, collapse = "/"), .groups = "drop")
  inner_join(station_points, attributes, by = "station_id") %>% arrange(station_id)
})
stopifnot(nrow(stations$Both) - nrow(stations$Existing) == 29)
write_csv(code_crosswalk, file.path(out, "station-code-crosswalk.csv"))

# ---- th03-osm-extract ----
# GDAL's streaming conversion consumes every OSM layer, avoiding truncated reads.
osm_gpkg <- file.path(raw_dir, "osm-singapore.gpkg")
if (!file.exists(osm_gpkg)) {
  message("Converting Singapore OSM points and polygons")
  Sys.setenv(OSM_MAX_TMPFILE_SIZE = "512")
  gdal_utils("vectortranslate", file.path(raw_dir, "osm.osm.pbf"), osm_gpkg,
    options = c("-f", "GPKG", "-spat", "103.5", "1.1", "104.15", "1.5", "points", "multipolygons"))
}
tag <- function(x, name) {
  direct <- if (name %in% names(x)) x[[name]] else rep(NA_character_, nrow(x))
  coalesce(direct, str_match(x$other_tags, paste0('"', name, '"=>"([^"]+)"'))[, 2])
}
extract_activity <- function(layer) {
  x <- st_read(osm_gpkg, layer = layer, quiet = TRUE)
  x$amenity_tag <- tag(x, "amenity")
  x$shop_tag <- tag(x, "shop")
  x$office_tag <- tag(x, "office")
  x$leisure_tag <- tag(x, "leisure")
  x <- x %>% mutate(category = case_when(
    !is.na(shop_tag) & !shop_tag %in% c("no", "vacant") ~ "Shops",
    !is.na(office_tag) & office_tag != "no" ~ "Offices",
    amenity_tag %in% c("school", "college", "university", "kindergarten") ~ "Education",
    amenity_tag %in% c("hospital", "clinic", "doctors", "dentist") ~ "Healthcare",
    amenity_tag %in% c("restaurant", "cafe", "fast_food", "food_court", "marketplace", "library", "cinema", "theatre") |
      leisure_tag %in% c("sports_centre", "stadium", "fitness_centre", "swimming_pool") ~ "Food and leisure",
    TRUE ~ NA_character_)) %>% filter(!is.na(category)) %>%
    st_make_valid() %>% st_transform(3414) %>% st_point_on_surface() %>%
    select(name, category)
  x$source_layer <- layer
  x
}
pois <- bind_rows(extract_activity("multipolygons"), extract_activity("points"))
hits <- st_intersects(pois, subzones)
pois <- pois[lengths(hits) > 0, ]
hits <- hits[lengths(hits) > 0]
pois$origin_id <- subzones$origin_id[vapply(hits, `[`, integer(1), 1)]
# Drop repeated named facilities in the same category within 50 metres.
keys <- paste(normalise(pois$name), pois$category)
duplicate <- rep(FALSE, nrow(pois))
named <- which(!is.na(pois$name) & nzchar(pois$name))
for (group in split(named, keys[named])) {
  if (length(group) < 2) next
  nearby <- st_is_within_distance(pois[group, ], dist = 50)
  for (i in seq_along(group)) {
    earlier <- group[nearby[[i]]]
    duplicate[group[i]] <- any(earlier < group[i] & !duplicate[earlier])
  }
}
pois <- pois[!duplicate, ]
origin_activity <- st_drop_geometry(pois) %>% count(origin_id, name = "activity")
origins <- origins %>% left_join(origin_activity, by = "origin_id") %>% mutate(activity = replace_na(activity, 0L))
stopifnot(sum(origins$activity) == nrow(pois), sum(origins$residents) == sum(population$residents))

# ---- th03-interactions ----
interactions <- lapply(stations, function(destinations) {
  distances <- st_distance(origins, destinations)
  expand_grid(origin_id = origins$origin_id, station_id = destinations$station_id) %>%
    mutate(distance_km = as.vector(t(as.matrix(distances))) / 1000) %>%
    left_join(st_drop_geometry(destinations) %>% select(station_id, lines, starts_with("bus_")), by = "station_id")
})
for (name in names(stations)) {
  stopifnot(nrow(interactions[[name]]) == nrow(origins) * nrow(stations[[name]]),
            !anyDuplicated(interactions[[name]][c("origin_id", "station_id")]),
            !anyNA(interactions[[name]]))
}

# ---- th03-residential-buildings ----
expand_road <- function(x) {
  x <- toupper(x)
  x <- sub("^ST[.]? ", "SAINT ", x)
  abbreviations <- c("RD" = "ROAD", "AVE" = "AVENUE", "ST" = "STREET", "DR" = "DRIVE",
                    "CRES" = "CRESCENT", "TER" = "TERRACE", "NTH" = "NORTH", "STH" = "SOUTH",
                    "BT" = "BUKIT", "UPP" = "UPPER", "CTRL" = "CENTRAL", "JLN" = "JALAN",
                    "LOR" = "LORONG", "TG" = "TANJONG", "KG" = "KAMPONG", "C'WEALTH" = "COMMONWEALTH",
                    "CL" = "CLOSE", "PK" = "PARK", "GDNS" = "GARDENS", "HTS" = "HEIGHTS", "PL" = "PLACE")
  for (short in names(abbreviations)) x <- gsub(paste0("\\b", short, "\\b"), abbreviations[[short]], x)
  normalise(x)
}
properties <- read_csv(file.path(raw_dir, "hdb-properties.csv"), show_col_types = FALSE) %>%
  filter(residential == "Y", total_dwelling_units > 0) %>%
  mutate(address_key = paste(normalise(blk_no), expand_road(street)))
stopifnot(!anyDuplicated(properties$address_key))
building_footprints <- st_read(file.path(raw_dir, "hdb-buildings.geojson"), quiet = TRUE) %>%
  st_make_valid() %>% st_transform(3414)
# Verify a street-code match using equal postal code and block number, within 150 m.
address_rows <- lapply(c("points", "multipolygons"), function(layer) {
  x <- st_read(osm_gpkg, layer = layer, quiet = TRUE)
  x$road <- tag(x, "addr:street")
  x$postal <- tag(x, "addr:postcode")
  x$block <- tag(x, "addr:housenumber")
  x %>% filter(!is.na(road), postal %in% building_footprints$POSTAL_COD, !is.na(block)) %>%
    select(road, postal, block) %>% st_make_valid() %>% st_transform(3414) %>% st_point_on_surface()
}) %>% bind_rows()
xy <- st_coordinates(address_rows)
address_rows <- st_drop_geometry(address_rows) %>% mutate(address_x = xy[, 1], address_y = xy[, 2])
xy <- st_coordinates(st_point_on_surface(building_footprints))
building_rows <- st_drop_geometry(building_footprints) %>% mutate(x = xy[, 1], y = xy[, 2])
address_matches <- inner_join(building_rows, address_rows,
                             by = c("POSTAL_COD" = "postal", "BLK_NO" = "block"), relationship = "many-to-many") %>%
  mutate(separation_m = sqrt((x - address_x)^2 + (y - address_y)^2), road = expand_road(road)) %>%
  filter(separation_m <= 150)
street_votes <- address_matches %>% distinct(ST_COD, POSTAL_COD, road) %>% count(ST_COD, road) %>%
  group_by(ST_COD) %>% mutate(agreement = n / sum(n)) %>% ungroup()
write_csv(street_votes, file.path(out, "hdb-address-agreement.csv"))
street_key <- street_votes %>% filter(agreement >= 0.9) %>%
  select(street_code = ST_COD, road)
write_csv(street_key, file.path(out, "hdb-street-crosswalk.csv"))
direct_matches <- address_matches %>%
  mutate(address_key = paste(normalise(BLK_NO), road)) %>%
  filter(address_key %in% properties$address_key) %>%
  distinct(POSTAL_COD, BLK_NO, address_key) %>% group_by(POSTAL_COD, BLK_NO) %>%
  mutate(ambiguous = n() > 1) %>% ungroup()
ambiguous <- direct_matches %>% filter(ambiguous) %>% distinct(POSTAL_COD, BLK_NO)
direct_matches <- direct_matches %>% filter(!ambiguous) %>% select(-ambiguous)
buildings <- building_footprints %>%
  anti_join(ambiguous, by = c("POSTAL_COD", "BLK_NO")) %>%
  left_join(direct_matches, by = c("POSTAL_COD", "BLK_NO")) %>%
  left_join(street_key, by = c("ST_COD" = "street_code")) %>%
  mutate(address_key = coalesce(address_key, paste(normalise(BLK_NO), expand_road(road))),
         footprint_area = as.numeric(st_area(geometry))) %>%
  filter(address_key %in% properties$address_key) %>%
  arrange(desc(footprint_area)) %>% distinct(address_key, .keep_all = TRUE) %>%
  select(address_key) %>% st_point_on_surface() %>%
  inner_join(properties %>% select(address_key, blk_no, street, total_dwelling_units), by = "address_key")
unmatched <- anti_join(properties, st_drop_geometry(buildings), by = "address_key")
write_csv(unmatched, file.path(out, "unmatched-hdb-properties.csv"))
hits <- st_intersects(buildings, subzones)
buildings$origin_id <- vapply(hits, function(x) if (length(x)) subzones$origin_id[x[1]] else NA_character_, character(1))
stopifnot(!anyNA(buildings$origin_id), !anyDuplicated(buildings$address_key))

# ---- th03-access-distances ----
access <- lapply(names(stations), function(scenario) {
  destinations <- stations[[scenario]]
  nearest <- st_nearest_feature(buildings, destinations)
  tibble(address_key = buildings$address_key, origin_id = buildings$origin_id,
         dwellings = buildings$total_dwelling_units, scenario = scenario,
         nearest_station = destinations$station[nearest],
         distance_m = as.numeric(st_distance(buildings, destinations[nearest, ], by_element = TRUE)))
}) %>% bind_rows()
baseline <- access %>% filter(scenario == "Existing") %>% select(address_key, baseline_m = distance_m)
access <- access %>% left_join(baseline, by = "address_key") %>% mutate(gain_m = baseline_m - distance_m)
stopifnot(all(access$gain_m >= -1e-7), all(access$gain_m[access$scenario == "Existing"] == 0))
age_groups <- subzones %>% st_drop_geometry() %>% filter(hdb_residents > 0) %>%
  mutate(age_group = factor(ntile(older_share, 4), levels = 1:4,
                            labels = c("Lowest older share", "Second quarter", "Third quarter", "Highest older share"))) %>%
  select(origin_id, older_share, age_group)
access <- left_join(access, age_groups, by = "origin_id")
coverage <- access %>% crossing(threshold = c(400, 800, 1200)) %>% group_by(scenario, threshold) %>%
  summarise(share = weighted.mean(distance_m <= threshold, dwellings), .groups = "drop")
equity <- access %>% filter(!is.na(age_group)) %>% group_by(scenario, age_group) %>%
  summarise(mean_distance = weighted.mean(distance_m, dwellings),
            mean_gain = weighted.mean(gain_m, dwellings),
            share_800 = weighted.mean(distance_m <= 800, dwellings), dwellings = sum(dwellings), .groups = "drop")
local_access <- access %>% filter(scenario == "Both") %>% group_by(origin_id) %>%
  summarise(gain_m = weighted.mean(gain_m, dwellings),
            after_m = weighted.mean(distance_m, dwellings), dwellings = sum(dwellings), .groups = "drop") %>%
  left_join(st_drop_geometry(subzones) %>% select(origin_id, subzone, older_share), by = "origin_id")

# ---- th03-save-preparation ----
prepared <- list(subzones = subzones, origins = origins, stations = stations,
                 interactions = interactions, code_crosswalk = code_crosswalk,
                 pois = pois, buildings = buildings, access = access, coverage = coverage,
                 equity = equity, local_access = local_access,
                 hdb_match = tibble(total_blocks = nrow(properties), matched_blocks = nrow(buildings),
                                   total_dwellings = sum(properties$total_dwelling_units),
                                   matched_dwellings = sum(buildings$total_dwelling_units)))
saveRDS(prepared, file.path(out, "prepared.rds"))
write_csv(st_drop_geometry(origins), file.path(out, "origins.csv"))
write_csv(local_access, file.path(out, "neighbourhood-access.csv"))
write_csv(coverage, file.path(out, "access-coverage.csv"))
message("Prepared ", nrow(origins), " origins; ", nrow(stations$Existing), " existing station complexes; ",
        nrow(buildings), " matched residential blocks.")
