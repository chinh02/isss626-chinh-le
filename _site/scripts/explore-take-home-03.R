# Local comparisons drawn from the same saved model and housing results.
suppressPackageStartupMessages({library(sf); library(tidyverse)})
result_path <- "data/take-home-03/derived/analysis.rds"
analysis <- readRDS(result_path)
prepared <- analysis$prepared
ridership <- analysis$ridership
stopifnot(!is.null(ridership))
origins <- prepared$origins
total <- sum(ridership$observed$entries)
source("scripts/model-take-home-03.R")
demand_share <- origin_weights(origins, ridership$fit$parameters["residential"], ridership$fit$parameters["office"], ridership$fit$parameters["employment"])

# ---- th03-station-examples ----
examples <- ridership$observed %>%
  filter(station_id %in% c("YISHUN", "RAFFLESPLACE", "DHOBYGHAUT"))
examples$dwellings_500 <- vapply(st_is_within_distance(examples, prepared$buildings, dist = 500),
  function(i) sum(prepared$buildings$total_dwelling_units[i]), numeric(1))
examples$office_pois_500 <- lengths(st_is_within_distance(
  examples, filter(prepared$pois, category == "Offices"), dist = 500))
examples <- st_drop_geometry(examples) %>%
  select(station, entries, lines, bus_500, dwellings_500, office_pois_500)

# ---- th03-scenario-comparison ----
catchments <- ridership$catchments %>%
  left_join(tibble(origin_id = origins$origin_id, demand_share = demand_share), by = "origin_id")
baseline_choices <- catchments %>% filter(scenario == "Existing") %>%
  select(origin_id, baseline_station = station_id)
catchments <- catchments %>% left_join(baseline_choices, by = "origin_id") %>%
  mutate(changed = station_id != baseline_station, active = demand_share > 0)
scenario_summary <- ridership$predictions %>% group_by(scenario) %>%
  summarise(new_locations = sum(!existing), new_entries = sum(predicted[!existing]),
            new_share = new_entries / sum(predicted), total_entries = sum(predicted), .groups = "drop") %>%
  left_join(catchments %>% group_by(scenario) %>%
              summarise(changed_origins = sum(changed & active), active_origins = sum(active), .groups = "drop"),
            by = "scenario")

# ---- th03-local-competition ----
change_station <- ridership$predictions %>% filter(scenario == "Both", existing) %>%
  slice_max(abs(change), n = 1, with_ties = FALSE)
before_index <- match(change_station$station_id, prepared$stations$Existing$station_id)
after_index <- match(change_station$station_id, prepared$stations$Both$station_id)
contributions <- total * demand_share *
  (ridership$probabilities$Both[, after_index] - ridership$probabilities$Existing[, before_index])
origin_index <- which.max(abs(contributions))
local_origin <- st_drop_geometry(origins[origin_index, ])
local_choices <- lapply(c("Existing", "Both"), function(scenario) {
  stations <- prepared$stations[[scenario]]
  probabilities <- ridership$probabilities[[scenario]][origin_index, ]
  tibble(station_id = stations$station_id, station = stations$station, scenario = scenario,
         probability = probabilities,
         lines = stations$lines, bus_500 = stations$bus_500,
         distance_m = as.numeric(st_distance(origins[origin_index, ], stations)))
}) %>% bind_rows()
shown <- local_choices %>% group_by(scenario) %>% slice_max(probability, n = 3, with_ties = FALSE) %>%
  pull(station_id) %>% union(change_station$station_id)
local_choices <- local_choices %>% filter(station_id %in% shown)

# ---- th03-local-housing-access ----
top_access <- prepared$local_access %>% slice_max(gain_m, n = 1, with_ties = FALSE) %>%
  left_join(st_drop_geometry(prepared$subzones) %>% select(origin_id, planning_area), by = "origin_id")
local_housing <- prepared$access %>% filter(origin_id == top_access$origin_id) %>%
  group_by(scenario, nearest_station) %>% summarise(dwellings = sum(dwellings), .groups = "drop") %>%
  group_by(scenario) %>% mutate(share = dwellings / sum(dwellings)) %>% ungroup()
older_gaps <- prepared$access %>% filter(scenario == "Both", age_group == "Highest older share") %>%
  group_by(origin_id) %>% summarise(beyond_800 = sum(dwellings[distance_m > 800]),
    mean_distance = weighted.mean(distance_m, dwellings), dwellings = sum(dwellings),
    .groups = "drop") %>%
  left_join(st_drop_geometry(prepared$subzones) %>% select(origin_id, subzone, planning_area, older_share), by = "origin_id") %>%
  arrange(desc(beyond_800))

# ---- th03-sensitivity-summary ----
sensitivity_summary <- ridership$sensitivity %>%
  left_join(st_drop_geometry(prepared$stations$Both) %>% select(station_id, station, existing), by = "station_id") %>%
  filter(scenario == "Both", !existing) %>% group_by(check) %>%
  summarise(new_entries = sum(predicted), new_share = new_entries / total,
            leading_station = station[which.max(predicted)], .groups = "drop")
# ---- th03-month-summary ----
month_summary <- bind_rows(ridership$entries) %>% group_by(month) %>%
  summarise(stations = n(), weekdays = first(weekdays), monthly_entries = sum(monthly_entries),
            daily_entries = sum(entries), .groups = "drop")

# ---- th03-model-diagnosis ----
diagnosis <- ridership$validation %>% filter(model %in% c(3, ridership$fit$model)) %>%
  left_join(st_drop_geometry(ridership$observed) %>% select(station_id, station, codes), by = "station_id") %>%
  mutate(type = if_else(grepl("(^|/)(NS|EW|CG|NE|CC|CE|DT|TE)[0-9]", codes), "MRT", "LRT-only"),
         error = predicted - observed)
type_comparison <- diagnosis %>% group_by(model, type) %>%
  summarise(stations = n(), observed = sum(observed), predicted = sum(predicted),
            WAPE = sum(abs(error)) / sum(observed), .groups = "drop")
station_errors <- diagnosis %>% select(station_id, station, model, observed, predicted) %>%
  mutate(model = if_else(model == 3, "original", "revised")) %>%
  pivot_wider(names_from = model, values_from = predicted)
months <- inner_join(ridership$entries$September, ridership$entries$August,
                     by = "station_id", suffix = c("_sep", "_aug"))
august_share_benchmark <- error_measures(months$entries_sep,
  months$entries_aug / sum(months$entries_aug) * sum(months$entries_sep))
details <- list(examples = examples, scenarios = scenario_summary, catchments = catchments,
                change_station = change_station, local_origin = local_origin, local_choices = local_choices,
                local_contribution = contributions[origin_index], top_access = top_access,
                local_housing = local_housing, older_gaps = older_gaps,
                sensitivity = sensitivity_summary, months = month_summary,
                type_comparison = type_comparison, station_errors = station_errors,
                august_share_benchmark = august_share_benchmark)
stopifnot(all(abs(scenario_summary$total_entries - total) < 1e-6),
          !anyDuplicated(catchments[c("origin_id", "scenario")]),
          all(local_choices$probability >= 0 & local_choices$probability <= 1))

# ---- th03-analysis-tables ----
parameters <- ridership$fit$parameters
interaction_tables <- lapply(c("Existing", "Both"), function(scenario) {
  pairs <- ridership$choice_interactions[[scenario]]
  destinations <- prepared$stations[[scenario]]
  attraction <- station_attraction(parameters, destinations)
  pairs %>% mutate(attraction = .env$attraction[match(station_id, destinations$station_id)],
                   distance = pmax(distance_km, ridership$fit$distance_floor))
})
names(interaction_tables) <- c("Existing", "Both")
origin_table <- ridership$choice_origins %>%
  mutate(market_share = origin_weights(ridership$choice_origins, parameters["residential"], parameters["office"], parameters["employment"]),
         market_potential = total * market_share)
destination_table <- prepared$stations$Both %>%
  left_join(st_drop_geometry(ridership$observed) %>%
              select(station_id, observed_entries = entries, existing_lines = lines), by = "station_id") %>%
  mutate(existing_lines = replace_na(existing_lines, 0L),
         mrt = station_features(prepared$stations$Both)[, "mrt"],
         attraction = station_attraction(parameters, prepared$stations$Both))
stopifnot(!anyDuplicated(origin_table$origin_id), !anyDuplicated(destination_table$station_id),
          sum(destination_table$observed_entries, na.rm = TRUE) == total,
          all(is.na(destination_table$observed_entries) == !destination_table$existing),
          abs(sum(origin_table$market_potential) - total) < 1e-6)
saveRDS(interaction_tables$Existing, "data/take-home-03/derived/existing-network-interactions.rds")
saveRDS(interaction_tables$Both, "data/take-home-03/derived/expanded-network-interactions.rds")
saveRDS(origin_table, "data/take-home-03/derived/origins.rds")
saveRDS(destination_table, "data/take-home-03/derived/destinations.rds")

analysis$details <- details
saveRDS(analysis, result_path)
write_csv(scenario_summary, "data/take-home-03/derived/scenario-comparison.csv")
write_csv(older_gaps, "data/take-home-03/derived/older-neighbourhood-gaps.csv")
write_csv(local_choices, "data/take-home-03/derived/local-station-choices.csv")
write_csv(month_summary, "data/take-home-03/derived/passenger-months.csv")
message("Local comparisons saved for the report and summary.")
