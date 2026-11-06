# Compare each addition using the original geographical folds and fixed observations.
suppressPackageStartupMessages({library(sf); library(tidyverse)})
source("scripts/model-take-home-03.R")
analysis <- readRDS("data/take-home-03/derived/analysis.rds")
context <- readRDS("data/take-home-03/derived/context.rds")
ridership <- analysis$ridership
origins <- context$origins
stations <- ridership$observed %>%
  select(-starts_with("direct_bus_")) %>%
  left_join(st_drop_geometry(context$destinations) %>% select(station_id, starts_with("direct_bus_")), by = "station_id")
distances <- units::drop_units(st_distance(origins, stations)) / 1000
baseline <- ridership$validation %>% filter(model == 7)
stopifnot(identical(baseline$station_id, stations$station_id),
          identical(origins$origin_id, ridership$choice_origins$origin_id),
          max(abs(distances - matrix(ridership$choice_interactions$Existing$distance_km,
                                    nrow = nrow(origins), byrow = TRUE))) < 1e-10)
results <- list()
for (model in 8:10) {
  fits <- list()
  predicted <- rep(NA_real_, nrow(stations))
  for (fold in 1:5) {
    message("Context model ", model, ", fold ", fold, "/5")
    fit <- fit_huff(stations$entries, distances, origins, stations, model,
                    training = which(baseline$fold != fold))
    predicted[baseline$fold == fold] <- fit$predicted[baseline$fold == fold]
    fits[[fold]] <- fit$parameters
  }
  results[[as.character(model)]] <- list(parameters = fits,
    validation = tibble(station_id = stations$station_id, model = model, fold = baseline$fold,
                        observed = stations$entries, predicted = predicted),
    performance = error_measures(stations$entries, predicted))
  print(results[[as.character(model)]]$performance)
  print(tibble(station = stations$station, observed = stations$entries, predicted = predicted) %>%
          filter(station %in% c("Raffles Place", "Marine Parade")))
  saveRDS(results, "data/take-home-03/derived/context-trials.rds")
}
