# Workplace totals and direct daytime bus alternatives, from public sources.
suppressPackageStartupMessages({library(sf); library(tidyverse); library(jsonlite)})
source("scripts/demand-take-home-03.R")
prepared <- readRDS("data/take-home-03/derived/prepared.rds")
distributed <- make_demand_locations(prepared)
office <- make_office_locations(prepared)
raw_dir <- "data/take-home-03/raw"

# ---- th03-workplace-totals ----
workplace <- fromJSON(file.path(raw_dir, "workplace-industry-2025.json"), simplifyVector = FALSE)$Data
employment <- map_dfr(workplace$row, function(row) {
  total <- keep(row$columns, ~ .x$key == "Total")[[1]]$value
  tibble(planning_area = toupper(row$rowText), workers = as.numeric(if_else(total == "-", "0", total)))
})
areas <- prepared$subzones %>% group_by(planning_area) %>% summarise(.groups = "drop")
mapped <- employment %>% filter(planning_area %in% areas$planning_area)
unmapped <- employment %>% filter(planning_area != "TOTAL", !planning_area %in% areas$planning_area)
stopifnot(!anyDuplicated(employment$planning_area), !anyNA(employment$workers))

# Workplace counts are observed for planning areas; the allocation within them is estimated.
locations <- bind_rows(mutate(distributed, office_area = 0), office$locations) %>%
  left_join(st_drop_geometry(prepared$origins) %>% select(subzone_id = origin_id, planning_area), by = "subzone_id") %>%
  left_join(mapped, by = "planning_area") %>% mutate(workers = replace_na(workers, 0)) %>%
  group_by(planning_area) %>%
  mutate(employment = if (sum(activity) > 0) first(workers) * activity / sum(activity) else 0) %>% ungroup()
allocation <- st_drop_geometry(locations) %>% group_by(planning_area) %>%
  summarise(workers = first(workers), allocated = sum(employment), .groups = "drop")
stopifnot(max(abs(allocation$workers - allocation$allocated)) < 1e-6,
          !anyDuplicated(locations$origin_id), !anyNA(locations$planning_area))

# ---- th03-direct-bus-services ----
routes <- readRDS(file.path(raw_dir, "BusRoutes-20261104.rds"))
services <- readRDS(file.path(raw_dir, "BusServices-20261104.rds"))
stops <- readRDS(file.path(raw_dir, "BusStops-20261104.rds")) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>% st_transform(3414)
stopifnot(!anyDuplicated(stops$BusStopCode),
          !anyDuplicated(services[c("ServiceNo", "Direction")]),
          !anyDuplicated(routes[c("ServiceNo", "Direction", "StopSequence")]))
hits <- st_intersects(stops, areas)
stops$planning_area <- vapply(hits, function(i) if (length(i)) areas$planning_area[i[1]] else NA_character_, character(1))
unmatched_stops <- routes %>% distinct(BusStopCode) %>% anti_join(st_drop_geometry(stops), by = "BusStopCode")
stopifnot(nrow(unmatched_stops) == 0)

# Convert the published daytime headway range to its midpoint, in minutes.
headway <- function(x) vapply(str_extract_all(x, "[0-9]+"), function(v) {
  if (!length(v)) return(NA_real_)
  value <- mean(as.numeric(v))
  if (value > 0) value else NA_real_
}, numeric(1))
services <- services %>% mutate(daytime_headway = headway(AM_Offpeak_Freq),
                                buses_per_hour = 60 / daytime_headway)
routes <- routes %>% left_join(st_drop_geometry(stops) %>% select(BusStopCode, planning_area), by = "BusStopCode")

# A route offers a direct CBD trip if a Downtown Core stop follows this boarding stop.
# Keep routes operating in the daytime, and count a service once at each station.
direct_routes <- routes %>% group_by(ServiceNo, Direction) %>%
  mutate(last_cbd_stop = if (any(planning_area == "DOWNTOWN CORE", na.rm = TRUE))
    max(StopSequence[which(planning_area == "DOWNTOWN CORE")]) else 0L,
    cbd_distance_km = vapply(seq_along(StopSequence), function(i) {
      next_stops <- which(planning_area == "DOWNTOWN CORE" & StopSequence > StopSequence[i])
      if (length(next_stops)) min(Distance[next_stops] - Distance[i]) else Inf
    }, numeric(1))) %>% ungroup() %>%
  filter(StopSequence < last_cbd_stop, planning_area != "DOWNTOWN CORE",
         WD_FirstBus != "-", WD_LastBus != "-", grepl("^[0-9]+$", ServiceNo)) %>%
  inner_join(services %>% filter(is.finite(buses_per_hour)), by = c("ServiceNo", "Direction"))
station_bus <- function(destinations, radius = 500) {
  near <- st_is_within_distance(destinations, stops, dist = radius)
  map_dfr(seq_len(nrow(destinations)), function(j) {
    direct_routes %>% filter(BusStopCode %in% stops$BusStopCode[near[[j]]]) %>%
      group_by(ServiceNo) %>% slice_min(cbd_distance_km, n = 1, with_ties = FALSE) %>% ungroup() %>%
      mutate(station_id = destinations$station_id[j], station = destinations$station[j], radius = radius)
  })
}
station_routes <- map_dfr(c(400, 500, 800), ~station_bus(prepared$stations$Both, .x))
bus_totals <- station_routes %>% group_by(station_id, radius) %>%
  summarise(direct_services = n(), direct_buses_hour = sum(buses_per_hour), .groups = "drop")
bus_features <- bus_totals %>% select(station_id, radius, direct_buses_hour) %>%
  pivot_wider(names_from = radius, values_from = direct_buses_hour, names_prefix = "direct_bus_")
destinations <- prepared$stations$Both %>% left_join(bus_features, by = "station_id") %>%
  mutate(across(starts_with("direct_bus_"), ~replace_na(.x, 0)))
stopifnot(nrow(destinations) == nrow(prepared$stations$Both), !anyNA(destinations$direct_bus_500))
context <- list(employment = employment, mapped = mapped, unmapped = unmapped,
                allocation = allocation, origins = locations, destinations = destinations,
                station_routes = station_routes, bus_totals = bus_totals, stops = stops, services = services,
                unmatched_stops = unmatched_stops, workplace_metadata = workplace[setdiff(names(workplace), "row")])
saveRDS(context, "data/take-home-03/derived/context.rds")
write_csv(mapped, "data/take-home-03/derived/workplace-totals-2025.csv")
print(mapped %>% arrange(desc(workers)) %>% head(8)); print(unmapped)
print(bus_totals %>% filter(station_id %in% c("RAFFLESPLACE", "MARINEPARADE"), radius == 500))
print(station_routes %>% filter(station_id == "MARINEPARADE", radius == 500) %>%
        select(ServiceNo, Direction, BusStopCode, AM_Offpeak_Freq, buses_per_hour, cbd_distance_km))
message("Context data saved; workplace totals are preserved within every mapped planning area.")
