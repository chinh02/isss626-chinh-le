# Run after prepare-take-home-03.R. Missing passenger files leave ridership unset.
suppressPackageStartupMessages({library(sf); library(tidyverse); library(MCI)})
source("scripts/model-take-home-03.R")
source("scripts/demand-take-home-03.R")
base <- "data/take-home-03"
out <- file.path(base, "derived")
prepared <- readRDS(file.path(out, "prepared.rds"))
analysis <- list(prepared = prepared, ridership = NULL, missing = character())
entries <- lapply(c("202609", "202608"), read_entries, raw_dir = file.path(base, "raw"),
                  code_crosswalk = prepared$code_crosswalk)
names(entries) <- c("September", "August")
analysis$missing <- names(entries)[vapply(entries, is.null, logical(1))]
if (length(analysis$missing)) {
  saveRDS(analysis, file.path(out, "analysis.rds"))
  message("Access analysis is ready. Ridership still needs: ", paste(analysis$missing, collapse = ", "))
} else {
  source("scripts/context-take-home-03.R", local = new.env())
  context <- readRDS(file.path(out, "context.rds"))
  bus_features <- st_drop_geometry(context$destinations) %>% select(station_id, starts_with("direct_bus_"))
  prepared$stations <- lapply(prepared$stations, function(x) left_join(x, bus_features, by = "station_id"))
  # ---- th03-observed-entries ----
  origins <- prepared$origins
  stations <- prepared$stations$Existing %>% left_join(entries$September, by = "station_id")
  stopifnot(!anyNA(stations$entries), setequal(stations$station_id, entries$September$station_id))
  observed <- stations$entries
  distances <- matrix(prepared$interactions$Existing$distance_km, nrow = nrow(origins), byrow = TRUE)
  distributed_origins <- make_demand_locations(prepared)
  distributed_pairs <- make_interactions(distributed_origins, stations)
  distributed_distances <- matrix(distributed_pairs$distance_km, nrow = nrow(distributed_origins), byrow = TRUE)
  office_origins <- context$origins
  office_distances <- units::drop_units(st_distance(office_origins, stations)) / 1000
  office_totals <- st_drop_geometry(office_origins) %>% group_by(subzone_id) %>%
    summarise(office_area = sum(office_area), employment = sum(employment), .groups = "drop")
  origins <- origins %>% left_join(office_totals, by = c("origin_id" = "subzone_id")) %>%
    mutate(across(c(office_area, employment), ~replace_na(.x, 0)))
  prepared$origins <- origins
  analysis$prepared <- prepared

  # ---- th03-spatial-validation ----
  set.seed(62603)
  folds <- kmeans(st_coordinates(stations), centers = 5, nstart = 50)$cluster
  validation <- list()
  validation_parameters <- list()
  for (model in 1:10) {
    current_origins <- if (model >= 7) office_origins else if (model >= 5) distributed_origins else origins
    current_distances <- if (model >= 7) office_distances else if (model >= 5) distributed_distances else distances
    predicted <- rep(NA_real_, nrow(stations))
    for (fold in 1:5) {
      message("Fitting model ", model, ", geographical fold ", fold, "/5")
      fit <- fit_huff(observed, current_distances, current_origins, stations, model, training = which(folds != fold))
      validation_parameters[[paste(model, fold)]] <- as_tibble_row(as.list(fit$parameters)) %>% mutate(model = model, fold = fold)
      predicted[folds == fold] <- fit$predicted[folds == fold]
    }
    validation[[model]] <- tibble(station_id = stations$station_id, model = model, fold = folds,
                                 observed = observed, predicted = predicted)
  }
  validation <- bind_rows(validation)
  performance <- validation %>% group_by(model) %>%
    group_modify(~ error_measures(.x$observed, .x$predicted)) %>% ungroup()
  performance$description <- c("Residents and distance", "Residents, activities and distance",
    "Add rail and bus connections", "Add MRT/LRT distinction",
    "Distribute demand within subzones", "Distributed demand + MRT/LRT distinction", "Add office footprint area",
    "Use 2025 workplace totals", "Office area + direct CBD buses", "Workplace totals + direct CBD buses")
  selected <- min(performance$model[performance$WAPE <= min(performance$WAPE) * 1.01])
  # Check whether the selected improvement depends on one geographical split.
  alternative_folds <- cutree(hclust(dist(st_coordinates(stations)), method = "ward.D2"), k = 5)
  fold_sensitivity <- lapply(union(6:7, selected), function(model) {
    current_origins <- if (model >= 7) office_origins else distributed_origins
    current_distances <- if (model >= 7) office_distances else distributed_distances
    predicted <- rep(NA_real_, nrow(stations))
    for (fold in 1:5) {
      message("Alternative partition: model ", model, ", fold ", fold)
      check <- fit_huff(observed, current_distances, current_origins, stations, model,
                        training = which(alternative_folds != fold))
      predicted[alternative_folds == fold] <- check$predicted[alternative_folds == fold]
    }
    tibble(model = model, station_id = stations$station_id, fold = alternative_folds,
           observed = observed, predicted = predicted)
  }) %>% bind_rows()
  choice_origins <- if (selected >= 7) office_origins else if (selected >= 5) distributed_origins else origins
  choice_interactions <- if (selected >= 5) lapply(prepared$stations, function(x) make_interactions(choice_origins, x)) else prepared$interactions
  distances <- matrix(choice_interactions$Existing$distance_km, nrow = nrow(choice_origins), byrow = TRUE)
  fit <- fit_huff(observed, distances, choice_origins, stations, selected)
  saveRDS(list(fit = fit, performance = performance, validation = validation),
          file.path(out, "calibration.rds"))

  # ---- th03-expanded-network ----
  scenario_predictions <- list()
  probabilities <- list()
  for (scenario in names(prepared$stations)) {
    message("Predicting network: ", scenario)
    destinations <- prepared$stations[[scenario]]
    pairs <- choice_interactions[[scenario]]
    p <- mci_probabilities(fit, pairs, choice_origins, destinations)
    fast_p <- choice_probabilities(fit$parameters, matrix(pairs$distance_km, nrow = nrow(choice_origins), byrow = TRUE),
                                   destinations, fit$bus_radius, fit$distance_floor)
    stopifnot(max(abs(p - fast_p)) < 1e-10, all(is.finite(p)), all(p >= 0), max(abs(rowSums(p) - 1)) < 1e-10)
    predicted <- sum(observed) * as.numeric(crossprod(origin_weights(choice_origins, fit$parameters["residential"], fit$parameters["office"], fit$parameters["employment"]), p))
    stopifnot(abs(sum(predicted) - sum(observed)) < 1e-6)
    scenario_predictions[[scenario]] <- st_drop_geometry(destinations) %>%
      mutate(scenario = scenario, predicted = predicted)
    if (selected >= 5) {
      fallback <- choice_probabilities(fit$parameters,
        matrix(prepared$interactions[[scenario]]$distance_km, nrow = nrow(origins), byrow = TRUE), destinations)
      probabilities[[scenario]] <- subzone_probabilities(p, choice_origins, origins,
        fit$parameters["residential"], fallback, fit$parameters["office"], fit$parameters["employment"])
      reconstructed <- sum(observed) * as.numeric(crossprod(origin_weights(origins, fit$parameters["residential"], fit$parameters["office"], fit$parameters["employment"]), probabilities[[scenario]]))
      stopifnot(max(abs(reconstructed - predicted)) < 1e-6)
    } else probabilities[[scenario]] <- p
  }
  predictions <- bind_rows(scenario_predictions)
  baseline <- predictions %>% filter(scenario == "Existing") %>% select(station_id, baseline = predicted)
  predictions <- predictions %>% left_join(baseline, by = "station_id") %>%
    mutate(change = predicted - baseline, percent_change = change / baseline)
  stopifnot(max(abs(predictions$change[predictions$scenario == "Existing"])) < 1e-10)
  catchments <- lapply(names(probabilities), function(scenario) {
    p <- probabilities[[scenario]]
    destinations <- prepared$stations[[scenario]]
    largest <- max.col(p, ties.method = "first")
    tibble(origin_id = origins$origin_id, scenario = scenario,
           station_id = destinations$station_id[largest],
           station = destinations$station[largest],
           probability = p[cbind(seq_len(nrow(p)), largest)])
  }) %>% bind_rows()

  # ---- th03-august-check ----
  august <- stations %>% select(station_id) %>% left_join(entries$August, by = "station_id")
  stopifnot(!anyNA(august$entries))
  temporal <- error_measures(august$entries, fit$predicted)

  # ---- th03-sensitivity ----
  settings <- tribble(~check, ~radius, ~floor, ~model,
                      "Main model", 500, 0.1, selected,
                      "400 m bus buffer", 400, 0.1, selected,
                      "800 m bus buffer", 800, 0.1, selected,
                      "50 m distance floor", 500, 0.05, selected,
                      "250 m distance floor", 500, 0.25, selected,
                      "Residential demand only", 500, 0.1, selected) %>%
    mutate(residential_only = check == "Residential demand only")
  sensitivity <- lapply(seq_len(nrow(settings)), function(i) {
    row <- settings[i, ]
    message("Sensitivity check: ", row$check)
    alt <- if (row$check == "Main model") fit else
      fit_huff(observed, distances, choice_origins, stations, row$model,
               bus_radius = row$radius, distance_floor = row$floor, residential_only = row$residential_only)
    do.call(bind_rows, lapply(c("Existing", "Both"), function(scenario) {
      destinations <- prepared$stations[[scenario]]
      pairs <- choice_interactions[[scenario]]
      p <- choice_probabilities(alt$parameters, matrix(pairs$distance_km, nrow = nrow(choice_origins), byrow = TRUE),
                                destinations, row$radius, row$floor)
      tibble(check = row$check, scenario = scenario, station_id = destinations$station_id,
             predicted = sum(observed) * as.numeric(crossprod(origin_weights(choice_origins, alt$parameters["residential"], alt$parameters["office"], alt$parameters["employment"]), p)))
    }))
  }) %>% bind_rows()
  analysis$ridership <- list(observed = stations, fit = fit, performance = performance, validation = validation,
                             predictions = predictions, probabilities = probabilities, catchments = catchments,
                             temporal = temporal, sensitivity = sensitivity, entries = entries,
                             fold_sensitivity = fold_sensitivity,
                             validation_parameters = bind_rows(validation_parameters),
                             choice_origins = choice_origins, choice_interactions = choice_interactions)
  write_csv(predictions, file.path(out, "station-predictions.csv"))
  write_csv(performance, file.path(out, "model-validation.csv"))
  write_csv(catchments, file.path(out, "station-catchments.csv"))
  saveRDS(analysis, file.path(out, "analysis.rds"))
  message("Selected model ", selected, "; analysis and validation saved.")
  source("scripts/explore-take-home-03.R", local = new.env())
}
