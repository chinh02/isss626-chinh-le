# Additional checks of the saved model; the selected fit and four scenarios stay fixed.
suppressPackageStartupMessages({library(sf); library(tidyverse); library(spdep)})
source("scripts/model-take-home-03.R")
analysis <- readRDS("data/take-home-03/derived/analysis.rds")
prepared <- analysis$prepared
ridership <- analysis$ridership
parameters <- ridership$fit$parameters
observed <- ridership$observed
validation <- ridership$validation %>% filter(model == ridership$fit$model)
stopifnot(identical(validation$station_id, observed$station_id))
stations <- observed %>% left_join(select(validation, station_id, fold, predicted), by = "station_id") %>%
  mutate(mrt = station_features(observed)[, "mrt"] == 1,
         error = predicted - entries)
total <- sum(observed$entries)
locations <- ridership$choice_origins
distances <- matrix(ridership$choice_interactions$Existing$distance_km,
                    nrow = nrow(locations), byrow = TRUE)
demand <- origin_weights(locations, parameters["residential"], parameters["office"], parameters["employment"])

# ---- th03-parameter-table ----
parameter_ranges <- ridership$validation_parameters %>% filter(model == ridership$fit$model) %>%
  pivot_longer(c(decay, rail, bus, mrt, residential, employment), names_to = "parameter") %>%
  group_by(parameter) %>% summarise(low = min(value), high = max(value), .groups = "drop") %>%
  mutate(fitted = unname(parameters[parameter]))

# ---- th03-reference-models ----
null_mean <- vapply(seq_len(nrow(stations)), function(i)
  mean(stations$entries[stations$fold != stations$fold[i]]), numeric(1))
null_type <- vapply(seq_len(nrow(stations)), function(i)
  mean(stations$entries[stations$fold != stations$fold[i] & stations$mrt == stations$mrt[i]]), numeric(1))
# A fixed Huff reference uses resident demand, equal attraction and distance power -2.
reference_p <- choice_probabilities(c(decay = 2), distances, observed)
reference_share <- as.numeric(crossprod(origin_weights(locations, 1), reference_p))
reference_prediction <- rep(NA_real_, nrow(stations))
for (fold in 1:5) {
  training <- which(stations$fold != fold)
  scale <- sum(stations$entries[training]) / sum(reference_share[training])
  reference_prediction[-training] <- scale * reference_share[-training]
}
benchmarks <- bind_rows(
  error_measures(stations$entries, null_mean) %>% mutate(model = "Training-station mean"),
  error_measures(stations$entries, null_type) %>% mutate(model = "Training mean within MRT / LRT"),
  error_measures(stations$entries, reference_prediction) %>% mutate(model = "Fixed Huff: residents, equal attraction, decay 2"))

# ---- th03-fold-errors ----
fold_errors <- st_drop_geometry(stations) %>% group_by(fold) %>%
  summarise(stations = n(), observed = sum(entries), predicted = sum(predicted),
    WAPE = sum(abs(error)) / sum(entries), bias = sum(error) / sum(entries), .groups = "drop")
rank_correlation <- cor(stations$entries, stations$predicted, method = "spearman")
in_sample <- error_measures(observed$entries, ridership$fit$predicted)
neighbours <- knn2nb(knearneigh(st_coordinates(stations), k = 6))
weights <- nb2listw(neighbours, style = "W")
log_error <- log(stations$predicted / stations$entries)
set.seed(62603)
spatial_error <- moran.mc(log_error, weights, nsim = 9999)

# ---- th03-recent-stations ----
# LTA opening dates: TEL4 23 Jun 2024, Hume 28 Feb 2025, CCL6 12 Jul 2026.
stations <- stations %>% mutate(recent_group = case_when(
  grepl("(^|/)TE2[3-9]($|/)", codes) ~ "TEL4 (June 2024)",
  grepl("(^|/)DT4($|/)", codes) ~ "Hume (February 2025)",
  grepl("(^|/)CC3[0-2]($|/)", codes) ~ "CCL6 (July 2026)"))
recent <- which(!is.na(stations$recent_group))
stopifnot(length(recent) == 11)
message("Fitting a separate check with all 11 recent stations excluded from calibration.")
recent_fit <- fit_huff(observed$entries, distances, locations, observed, ridership$fit$model,
                       training = setdiff(seq_len(nrow(observed)), recent))
recent_stations <- st_drop_geometry(stations[recent, ]) %>%
  mutate(group_prediction = recent_fit$predicted[recent])
recent_groups <- recent_stations %>% group_by(recent_group) %>%
  summarise(stations = n(), observed = sum(entries), spatial_cv = sum(predicted),
    group_holdout = sum(group_prediction), spatial_bias = spatial_cv / observed - 1,
    holdout_bias = group_holdout / observed - 1, .groups = "drop")

# ---- th03-interchange-check ----
both <- prepared$stations$Both
baseline_lines <- prepared$stations$Existing$lines[match(both$station_id, prepared$stations$Existing$station_id)]
fixed_lines <- both %>% mutate(lines = coalesce(baseline_lines, lines))
expanded_distances <- matrix(ridership$choice_interactions$Both$distance_km,
                             nrow = nrow(locations), byrow = TRUE)
fixed_p <- choice_probabilities(parameters, expanded_distances, fixed_lines)
fixed_prediction <- total * as.numeric(crossprod(demand, fixed_p))
interchange_check <- ridership$predictions %>% filter(scenario == "Both") %>%
  mutate(fixed_lines = fixed_prediction[match(station_id, both$station_id)],
    fixed_change = fixed_lines - baseline, upgrade_difference = predicted - fixed_lines) %>%
  filter(existing, lines > baseline_lines[match(station_id, both$station_id)])
stopifnot(abs(sum(fixed_prediction) - total) < 1e-6,
          all(fixed_prediction[both$existing] <= ridership$predictions$predicted[ridership$predictions$scenario == "Existing"] + 1e-6))

# ---- th03-fold-scenarios ----
# Reuse each training-fold fit at one fixed national demand total.
fold_parameters <- ridership$validation_parameters %>% filter(model == ridership$fit$model)
fold_predictions <- map_dfr(seq_len(nrow(fold_parameters)), function(i) {
  values <- unlist(fold_parameters[i, names(parameters)])
  p <- choice_probabilities(values, expanded_distances, both)
  w <- origin_weights(locations, values["residential"], values["office"], values["employment"])
  tibble(fold = fold_parameters$fold[i], station_id = both$station_id, existing = both$existing,
         predicted = total * as.numeric(crossprod(w, p)))
})
fold_scenarios <- fold_predictions %>% group_by(fold) %>%
  summarise(total = sum(predicted), new_share = sum(predicted[!existing]) / total, .groups = "drop")
stopifnot(all(abs(fold_scenarios$total - total) < 1e-6))

# ---- th03-housing-quality ----
properties <- read_csv("data/take-home-03/raw/hdb-properties.csv", show_col_types = FALSE) %>%
  filter(residential == "Y", total_dwelling_units > 0)
unmatched <- read_csv("data/take-home-03/derived/unmatched-hdb-properties.csv", show_col_types = FALSE)
housing_quality <- properties %>% group_by(bldg_contract_town) %>%
  summarise(properties = n(), dwellings = sum(total_dwelling_units), .groups = "drop") %>%
  left_join(unmatched %>% group_by(bldg_contract_town) %>%
    summarise(unmatched = sum(total_dwelling_units), .groups = "drop"), by = "bldg_contract_town") %>%
  mutate(unmatched = replace_na(unmatched, 0), matched = dwellings - unmatched,
         share = matched / dwellings) %>% arrange(share)
housing_decades <- properties %>% mutate(decade = 10 * (year_completed %/% 10)) %>% group_by(decade) %>%
  summarise(dwellings = sum(total_dwelling_units), .groups = "drop") %>%
  left_join(unmatched %>% mutate(decade = 10 * (year_completed %/% 10)) %>% group_by(decade) %>%
    summarise(unmatched = sum(total_dwelling_units), .groups = "drop"), by = "decade") %>%
  mutate(unmatched = replace_na(unmatched, 0), matched_share = 1 - unmatched / dwellings)
stopifnot(sum(housing_quality$matched) == prepared$hdb_match$matched_dwellings)

# ---- th03-age-coverage ----
age_coverage <- prepared$access %>% filter(scenario %in% c("Existing", "Both")) %>%
  group_by(age_group, scenario) %>% summarise(subzones = n_distinct(origin_id),
    share = weighted.mean(distance_m <= 800, dwellings), dwellings = sum(dwellings), .groups = "drop")

review <- list(parameters = parameter_ranges, benchmarks = benchmarks, folds = fold_errors,
  rank_correlation = rank_correlation, in_sample = in_sample,
  residual_moran = tibble(I = unname(spatial_error$statistic), p = spatial_error$p.value),
  stations = stations, recent_stations = recent_stations, recent_groups = recent_groups,
  recent_fit = recent_fit, interchanges = interchange_check,
  fixed_lines = tibble(station_id = both$station_id, predicted = fixed_prediction),
  fold_predictions = fold_predictions, fold_scenarios = fold_scenarios,
  housing_quality = housing_quality, housing_decades = housing_decades, age_coverage = age_coverage,
  baseline_sha256 = digest::digest(file = "data/take-home-03/derived/analysis.rds", algo = "sha256"))
saveRDS(review, "data/take-home-03/derived/review.rds")
writeLines(capture.output(sessionInfo()), "take-home-03/session-info.txt")
print(benchmarks); print(recent_groups); print(interchange_check %>% select(station, change, fixed_change, upgrade_difference))
print(housing_quality %>% filter(bldg_contract_town %in% c("TG", "BM")))
print(age_coverage); print(fold_scenarios); print(review$residual_moran)
