# Follow the two largest contrasting errors back to the data.
suppressPackageStartupMessages({library(sf); library(tidyverse)})
source("scripts/model-take-home-03.R")
source("scripts/demand-take-home-03.R")
analysis <- readRDS("data/take-home-03/derived/analysis.rds")
prepared <- analysis$prepared
ridership <- analysis$ridership
origins <- make_demand_locations(prepared)
stations <- ridership$observed
observed <- stations$entries
distances <- units::drop_units(st_distance(origins, stations)) / 1000
validation <- ridership$validation %>% filter(model == 6)
stopifnot(identical(validation$station_id, stations$station_id))
focus <- c("RAFFLESPLACE", "MARINEPARADE")

# ---- th03-hourly-entries ----
hourly <- lapply(c("202609", "202608"), function(month) {
  file <- paste0("origin_destination_train_", month, ".csv")
  connection <- unz(file.path("data/take-home-03/raw", sub("csv$", "zip", file)), file, open = "rb")
  on.exit(close(connection))
  trips <- read_csv(connection, show_col_types = FALSE,
                    col_types = cols(ORIGIN_PT_CODE = "c", DESTINATION_PT_CODE = "c"))
  trips %>% filter(grepl("(^|[-/])(NS26|EW14|TE26)([-/]|$)", ORIGIN_PT_CODE)) %>%
    mutate(station_id = if_else(grepl("TE26", ORIGIN_PT_CODE), "MARINEPARADE", "RAFFLESPLACE")) %>%
    group_by(station_id, DAY_TYPE, TIME_PER_HOUR) %>%
    summarise(trips = sum(TOTAL_TRIPS), .groups = "drop") %>%
    mutate(month = month, days = case_when(
      month == "202609" & DAY_TYPE == "WEEKDAY" ~ 22,
      month == "202609" ~ 8, DAY_TYPE == "WEEKDAY" ~ 20, TRUE ~ 11),
      entries = trips / days)
}) %>% bind_rows() %>%
  left_join(st_drop_geometry(stations) %>% select(station_id, station), by = "station_id")
profiles <- hourly %>% group_by(month, station_id, station, DAY_TYPE) %>%
  summarise(daily_entries = sum(entries),
            evening_share = sum(entries[TIME_PER_HOUR %in% 17:19]) / sum(entries), .groups = "drop")
reconciliation <- profiles %>% filter(DAY_TYPE == "WEEKDAY") %>%
  inner_join(bind_rows(ridership$entries), by = c("month", "station_id"))
stopifnot(nrow(reconciliation) == 4,
          max(abs(reconciliation$daily_entries - reconciliation$entries)) < 1e-8)

# ---- th03-station-demand-breakdown ----
# Refit the relevant training fold, keeping each focus station's entries out.
contributions <- lapply(focus, function(id) {
  j <- match(id, stations$station_id)
  training <- which(validation$fold != validation$fold[j])
  fit <- fit_huff(observed, distances, origins, stations, model = 6, training = training)
  probabilities <- choice_probabilities(fit$parameters, distances, stations)
  demand <- origin_weights(origins, fit$parameters["residential"])
  shares <- as.numeric(crossprod(demand, probabilities))
  scale <- sum(observed[training]) / sum(shares[training])
  stopifnot(abs(scale * shares[j] - validation$predicted[j]) < 0.01)
  st_drop_geometry(origins) %>%
    mutate(station_id = id, distance_m = distances[, j] * 1000,
           probability = probabilities[, j], entries = scale * demand * probabilities[, j])
}) %>% bind_rows() %>%
  left_join(st_drop_geometry(prepared$origins) %>% select(subzone_id = origin_id, subzone), by = "subzone_id")
nearby_pois <- lapply(focus, function(id) {
  nearby <- lengths(st_is_within_distance(prepared$pois, filter(stations, station_id == id), dist = 800)) > 0
  st_drop_geometry(prepared$pois[nearby, ]) %>% count(category) %>% mutate(station_id = id)
}) %>% bind_rows()

# ---- th03-office-demand-trials ----
xy <- st_coordinates(prepared$pois)
office_counts <- st_drop_geometry(prepared$pois) %>%
  mutate(subzone_id = origin_id, gx = floor(xy[, 1] / 500), gy = floor(xy[, 2] / 500)) %>%
  group_by(subzone_id, gx, gy) %>% summarise(offices = sum(category == "Offices"), .groups = "drop")
locations <- origins %>% left_join(office_counts, by = c("subzone_id", "gx", "gy")) %>%
  mutate(offices = if_else(kind == "activity", replace_na(offices, 0L), 0L))
stopifnot(sum(locations$offices) == sum(prepared$pois$category == "Offices"))
count_inputs <- list(origins = tibble(residents = locations$residents,
                     activity = locations$activity - locations$offices,
                     office_area = locations$offices), distances = distances)

# Read the same office footprints used by the model, with overlaps removed.
office_data <- make_office_locations(prepared)
office_points <- office_data$locations
area <- office_points$office_area
coverage <- office_data$coverage
area_inputs <- list(origins = tibble(residents = c(origins$residents, rep(0, length(area))),
                    activity = c(origins$activity, rep(0, length(area))),
                    office_area = c(rep(0, nrow(origins)), area)),
                    distances = rbind(distances, units::drop_units(st_distance(office_points, stations)) / 1000))
inputs <- list("Separate office counts" = count_inputs, "Add office footprint area" = area_inputs)
trials <- lapply(names(inputs), function(label) {
  input <- inputs[[label]]
  predicted <- rep(NA_real_, nrow(stations))
  parameters <- list()
  for (fold in 1:5) {
    message(label, ": geographical fold ", fold, "/5")
    fit <- fit_huff(observed, input$distances, input$origins, stations, model = 7,
                    training = which(validation$fold != fold))
    predicted[validation$fold == fold] <- fit$predicted[validation$fold == fold]
    parameters[[fold]] <- fit$parameters
  }
  stopifnot(all(is.finite(predicted)), all(predicted >= 0))
  list(predicted = predicted, parameters = parameters, metrics = error_measures(observed, predicted))
})
names(trials) <- names(inputs)
comparison <- bind_rows(
  error_measures(observed, validation$predicted) %>% mutate(Adjustment = "Before office adjustment"),
  bind_rows(lapply(trials, `[[`, "metrics"), .id = "Adjustment"))
predictions <- bind_rows(tibble(Adjustment = "Before office adjustment", station_id = stations$station_id,
                               observed = observed, predicted = validation$predicted),
  lapply(names(trials), function(label) tibble(Adjustment = label, station_id = stations$station_id,
                                            observed = observed, predicted = trials[[label]]$predicted)) %>% bind_rows())
focus_error <- validation %>% filter(station_id %in% focus) %>%
  summarise(absolute_error = sum(abs(predicted - observed)),
            WAPE_contribution = absolute_error / sum(validation$observed))
diagnostics <- list(hourly = hourly, profiles = profiles, contributions = contributions,
                    nearby_pois = nearby_pois, office_coverage = coverage, trials = trials,
                    comparison = comparison, predictions = predictions, focus_error = focus_error)
saveRDS(diagnostics, "data/take-home-03/derived/station-diagnostics.rds")
write_csv(comparison, "data/take-home-03/derived/station-activity-trials.csv")
print(comparison)
message("Station diagnostics saved; the selected model and expansion outputs are unchanged.")
