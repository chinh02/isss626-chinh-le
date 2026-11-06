# Numerical checks use synthetic observations only; nothing is saved as report data.
suppressPackageStartupMessages({library(tidyverse); library(MCI)})
source("scripts/model-take-home-03.R")
origins <- tibble(origin_id = paste0("O", 1:4), residents = c(200, 400, 600, 100),
                  activity = c(20, 50, 2, 25))
stations <- tibble(station_id = paste0("S", 1:6), lines = c(1, 2, 1, 3, 1, 2),
                   bus_500 = c(2, 8, 1, 6, 3, 4))
distances <- matrix(c(0, 0.5, 2, 4, 1, 3, 1, 0.7, 2, 1.4, 3, 4,
                      3, 2, 0.4, 2, 1.5, 3, 4, 3, 2, 0.6, 0.8, 1), nrow = 4, byrow = TRUE)
parameters <- c(decay = 1.8, residential = 0.6, rail = 0.4, bus = 0.2)
p <- choice_probabilities(parameters, distances, stations)
stopifnot(all(is.finite(p)), all(p >= 0), max(abs(rowSums(p) - 1)) < 1e-12)
observed <- 1000 * as.numeric(crossprod(origin_weights(origins, 0.6), p))
fit <- fit_huff(observed, distances, origins, stations, model = 3)
stopifnot(abs(sum(fit$predicted) - 1000) < 1e-8, max(abs(fit$predicted - observed)) < 0.05)
pairs <- expand_grid(origin_id = origins$origin_id, station_id = stations$station_id) %>%
  mutate(distance_km = as.vector(t(distances))) %>% left_join(stations, by = "station_id")
mci <- mci_probabilities(fit, pairs, origins, stations)
fast <- choice_probabilities(fit$parameters, distances, stations)
stopifnot(max(abs(mci - fast)) < 1e-10)

# A shared MRT effect must also apply to new MRT codes and match MCI exactly.
stations$codes <- c("NS1", "BP2", "NE3", "JS4", "CR5", "SW6")
type_parameters <- c(parameters, mrt = log(3))
type_p <- choice_probabilities(type_parameters, distances, stations)
type_observed <- 1000 * as.numeric(crossprod(origin_weights(origins, 0.6), type_p))
type_fit <- fit_huff(type_observed, distances, origins, stations, model = 6)
stopifnot(max(abs(type_fit$predicted - type_observed)) < 0.05)
type_mci <- mci_probabilities(type_fit, pairs, origins, stations)
stopifnot(max(abs(type_mci - choice_probabilities(type_fit$parameters, distances, stations))) < 1e-10,
          identical(as.numeric(station_features(stations)[, "mrt"]), c(1, 0, 1, 1, 1, 0)))
stations$codes <- NULL

# Changing held-out counts must have no effect on fitting or demand scaling.
first <- fit_huff(observed, distances, origins, stations, model = 2, training = 1:4)
changed <- observed
changed[5:6] <- observed[5:6] * 100
second <- fit_huff(changed, distances, origins, stations, model = 2, training = 1:4)
stopifnot(identical(first$parameters, second$parameters), identical(first$predicted, second$predicted))

# Office demand changes origin weights while conserving the same total demand.
origins$office_area <- c(1000, 0, 40, 350)
stopifnot(identical(origin_weights(origins, 0.6, 0), origin_weights(origins, 0.6)),
          abs(sum(origin_weights(origins, 0.6, 0.2)) - 1) < 1e-12)
office_observed <- 1000 * as.numeric(crossprod(origin_weights(origins, 0.6, 0.2), p))
office_fit <- fit_huff(office_observed, distances, origins, stations, model = 7)
stopifnot(max(abs(office_fit$predicted - office_observed)) < 0.05,
          abs(sum(office_fit$predicted) - 1000) < 1e-8)
office_first <- fit_huff(office_observed, distances, origins, stations, model = 7, training = 1:4)
office_changed <- office_observed
office_changed[5:6] <- office_changed[5:6] * 100
office_second <- fit_huff(office_changed, distances, origins, stations, model = 7, training = 1:4)
stopifnot(identical(office_first$parameters, office_second$parameters),
          identical(office_first$predicted, office_second$predicted))

# Employment preserves origin totals; direct buses can reduce fitted attraction.
origins$employment <- c(250, 30, 1000, 90)
stations$direct_bus_500 <- c(0, 12, 4, 25, 0, 8)
employment_demand <- origin_weights(origins, 0.5, employment_share = 0.4)
stopifnot(abs(sum(employment_demand) - 1) < 1e-12, all(employment_demand >= 0))
competition_parameters <- c(parameters, mrt = 0, competition = 0.3)
competition_p <- choice_probabilities(competition_parameters, distances, stations)
employment_observed <- 1000 * as.numeric(crossprod(employment_demand, competition_p))
employment_fit <- fit_huff(employment_observed, distances, origins, stations, model = 10)
stopifnot(max(abs(employment_fit$predicted - employment_observed)) < 0.05,
          abs(sum(employment_fit$predicted) - 1000) < 1e-8,
          all(station_features(stations)[, "competition"] <= 0))
employment_first <- fit_huff(employment_observed, distances, origins, stations, model = 10, training = 1:4)
employment_changed <- employment_observed
employment_changed[5:6] <- employment_changed[5:6] * 100
employment_second <- fit_huff(employment_changed, distances, origins, stations, model = 10, training = 1:4)
stopifnot(identical(employment_first$parameters, employment_second$parameters),
          identical(employment_first$predicted, employment_second$predicted))

# Returning finer locations to subzones preserves employment-weighted station totals.
source("scripts/demand-take-home-03.R")
locations <- origins %>% mutate(subzone_id = c("A", "A", "B", "B"))
subzones <- locations %>% group_by(subzone_id) %>%
  summarise(across(c(residents, activity, office_area, employment), sum), .groups = "drop") %>%
  rename(origin_id = subzone_id)
aggregated <- subzone_probabilities(competition_p, locations, subzones, 0.5,
  matrix(1 / nrow(stations), nrow(subzones), nrow(stations)), employment_share = 0.4)
stopifnot(max(abs(crossprod(employment_demand, competition_p) -
  crossprod(origin_weights(subzones, 0.5, employment_share = 0.4), aggregated))) < 1e-12)

# An unchanged network is identical; a new choice conserves the total.
same <- choice_probabilities(parameters, distances, stations)
stopifnot(identical(p, same))
expanded <- bind_rows(stations, tibble(station_id = "NEW", lines = 1, bus_500 = 3, direct_bus_500 = 0))
new_p <- choice_probabilities(parameters, cbind(distances, c(0.2, 0.6, 2, 4)), expanded)
new_totals <- 1000 * as.numeric(crossprod(origin_weights(origins, 0.6), new_p))
stopifnot(abs(sum(new_totals) - 1000) < 1e-8)
residential <- fit_huff(observed, distances, origins, stations, model = 3, residential_only = TRUE)
stopifnot(residential$parameters["residential"] == 1)

# Import once per boarding journey, merge interchange codes, honour public holidays.
temporary <- tempfile("th03-check-")
dir.create(temporary)
trips <- tibble(YEAR_MONTH = "2026-08", DAY_TYPE = c("WEEKDAY", "WEEKDAY", "WEEKENDS/HOLIDAY"),
                TIME_PER_HOUR = c(8, 17, 8), ORIGIN_PT_CODE = c("CC34/DT16", "NS27/CC33/TE20", "CC34/DT16"),
                DESTINATION_PT_CODE = c("NS27/CC33/TE20", "CC34/DT16", "NS27/CC33/TE20"), TOTAL_TRIPS = c(200, 400, 100))
write_csv(trips, file.path(temporary, "origin_destination_train_202608.csv"))
crosswalk <- tibble(code = c("CE1", "DT16", "NS27", "CE2", "TE20"),
                    station_id = c("ONE", "ONE", "TWO", "TWO", "TWO"), network = "Existing")
entries <- read_entries("202608", temporary, crosswalk)
stopifnot(sum(entries$monthly_entries) == 600, all(entries$weekdays == 20), sum(entries$entries) == 30)
trips$ORIGIN_PT_CODE[1] <- "UNKNOWN"
write_csv(trips, file.path(temporary, "origin_destination_train_202608.csv"))
error <- try(read_entries("202608", temporary, crosswalk), silent = TRUE)
stopifnot(inherits(error, "try-error"))
message("Passed: probability checks, MCI agreement, conservation, held-out isolation, input reconciliation and calendar checks.")
