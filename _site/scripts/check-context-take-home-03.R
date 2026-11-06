# Check the employment model against another geographical partition and August.
suppressPackageStartupMessages({library(sf); library(tidyverse)})
source("scripts/model-take-home-03.R")
analysis <- readRDS("data/take-home-03/derived/analysis.rds")
context <- readRDS("data/take-home-03/derived/context.rds")
stations <- analysis$ridership$observed %>%
  select(-starts_with("direct_bus_")) %>%
  left_join(st_drop_geometry(context$destinations) %>% select(station_id, starts_with("direct_bus_")), by = "station_id")
origins <- context$origins
distances <- units::drop_units(st_distance(origins, stations)) / 1000
fit <- fit_huff(stations$entries, distances, origins, stations, 8)
august <- analysis$ridership$entries$August$entries[match(stations$station_id,
  analysis$ridership$entries$August$station_id)]
print(fit$parameters)
temporal <- error_measures(august, fit$predicted)
print(temporal)
folds <- cutree(hclust(dist(st_coordinates(stations)), method = "ward.D2"), k = 5)
predicted <- rep(NA_real_, nrow(stations))
for (fold in 1:5) {
  message("Employment model, alternative partition ", fold, "/5")
  check <- fit_huff(stations$entries, distances, origins, stations, 8,
                    training = which(folds != fold))
  predicted[folds == fold] <- check$predicted[folds == fold]
}
validation <- tibble(model = 8L, station_id = stations$station_id, fold = folds,
                      observed = stations$entries, predicted = predicted)
saveRDS(list(fit = fit, temporal = temporal, validation = validation),
        "data/take-home-03/derived/context-transfer-check.rds")
print(error_measures(stations$entries, predicted))
