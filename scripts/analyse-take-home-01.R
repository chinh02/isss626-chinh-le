# Run after the preparation chunks in take-home-exercise-01.qmd.
# The report also sources this script on every render.
library(sf)
library(tidyverse)
library(spatstat.geom)
library(spatstat.explore)

derived_dir <- "data/take-home-01/derived"
accidents <- readRDS(file.path(derived_dir, "accidents_2022_bmr.rds"))
provinces <- readRDS(file.path(derived_dir, "bmr_provinces.rds"))
study_window <- readRDS(file.path(derived_dir, "bmr_window.rds"))
accident_ppp <- unmark(readRDS(file.path(derived_dir, "accidents_2022_bmr_ppp.rds")))

# Distances are in metres (WGS 84 / UTM 47N). A 250 m display grid does
# not imply that the reported coordinates are accurate to 250 m.
bandwidths <- c(1000, 2000, 4000)
kde_surfaces <- lapply(bandwidths, function(h) {
  density(accident_ppp, sigma = h, eps = 250, edge = TRUE,
          diggle = TRUE, kernel = "gaussian", positive = TRUE)
})
kde_annual <- map2_dfr(kde_surfaces, bandwidths, function(surface, h) {
  as.data.frame(surface) %>%
    transmute(x, y, density = value * 1e6, bandwidth = paste(h / 1000, "km"))
})

accidents$quarter <- quarter(accidents$incident_time)
quarter_days <- c(90, 91, 92, 92)
quarter_labels <- c("Jan-Mar", "Apr-Jun", "Jul-Sep", "Oct-Dec")
kde_quarterly <- map_dfr(1:4, function(q) {
  surface <- density(accident_ppp[accidents$quarter == q], sigma = 2000,
                     eps = 250, edge = TRUE, diggle = TRUE,
                     kernel = "gaussian", positive = TRUE)
  as.data.frame(surface) %>%
    transmute(x, y, density = value * 1e6 / quarter_days[q],
              quarter = factor(quarter_labels[q], levels = quarter_labels))
})

# Locate the smoothed maximum for interpretation; this is not a road ranking.
kde_peaks <- kde_annual %>%
  group_by(bandwidth) %>% slice_max(density, n = 1, with_ties = FALSE) %>% ungroup() %>%
  st_as_sf(coords = c("x", "y"), crs = 32647) %>%
  st_join(provinces %>% select(province_geom)) %>% st_transform(4326)
peak_xy <- st_coordinates(kde_peaks)
kde_peaks <- kde_peaks %>% st_drop_geometry() %>%
  mutate(longitude = peak_xy[, 1], latitude = peak_xy[, 2])

quarter_counts <- accidents %>% st_drop_geometry() %>%
  count(province_geom, quarter) %>%
  mutate(per_day = n / quarter_days[quarter])

# Knox-style pair count: each unordered pair is counted once, with inclusive
# straight-line distance and elapsed-time thresholds. Dates stay within strata.
close_pair_test <- function(events, radii = c(500, 1000, 2000), lags = c(3, 7, 14),
                            strata = interaction(month(events$incident_time),
                                                 events$agency, drop = TRUE),
                            exclude_zero = FALSE, nsim = 999, seed = 62601) {
  xy <- st_coordinates(events)
  pattern <- ppp(xy[, 1], xy[, 2], window = Window(accident_ppp), checkdup = FALSE)
  pairs <- closepairs(pattern, rmax = max(radii), twice = FALSE, what = "ijd")
  if (exclude_zero) {
    keep <- pairs$d > 0
    pairs <- lapply(pairs, function(x) x[keep])
  }
  stopifnot(all(pairs$i != pairs$j),
            anyDuplicated(paste(pmin(pairs$i, pairs$j), pmax(pairs$i, pairs$j))) == 0L)
  times <- as.numeric(events$incident_time) / 86400
  groups <- split(seq_along(times), strata)
  settings <- expand_grid(radius_m = radii, lag_days = lags)
  nearby <- lapply(settings$radius_m, function(r) which(pairs$d <= r))
  count_pairs <- function(t) {
    gaps <- abs(t[pairs$i] - t[pairs$j])
    vapply(seq_len(nrow(settings)), function(k) {
      sum(gaps[nearby[[k]]] <= settings$lag_days[k])
    }, integer(1))
  }
  observed <- count_pairs(times)
  simulations <- matrix(NA_integer_, nrow = nsim, ncol = nrow(settings))
  set.seed(seed)
  for (b in seq_len(nsim)) {
    permuted <- times
    for (g in groups) permuted[g] <- times[g[sample.int(length(g))]]
    if (b == 1L) {
      stopifnot(all(vapply(groups, function(g) {
        identical(sort(times[g]), sort(permuted[g]))
      }, logical(1))))
    }
    simulations[b, ] <- count_pairs(permuted)
  }
  # Symmetric standardisation includes the observed pattern. The maximum over
  # all nine scales provides one family-wise, one-sided permutation test.
  all_counts <- rbind(observed, simulations)
  z <- sweep(sweep(all_counts, 2, colMeans(all_counts), "-"),
             2, apply(all_counts, 2, sd), "/")
  max_z <- apply(z, 1, max)
  results <- settings %>%
    mutate(observed = observed, expected = colMeans(simulations),
           lower = apply(simulations, 2, quantile, probs = 0.025),
           upper = apply(simulations, 2, quantile, probs = 0.975),
           excess_pct = 100 * (observed / expected - 1),
           p_upper = (1 + colSums(sweep(simulations, 2, observed, ">="))) / (nsim + 1),
           p_adjusted = vapply(z[1, ], function(v) mean(max_z >= v), numeric(1)))
  list(results = results, simulations = simulations,
       global_p = mean(max_z >= max_z[1]), nsim = nsim, seed = seed,
       spatial_pairs = nrow(as.data.frame(pairs)))
}

pair_test <- close_pair_test(accidents)
primary <- pair_test$results %>% filter(radius_m == 1000, lag_days == 7)

deduplicated <- accidents %>% distinct(longitude, latitude, incident_time, .keep_all = TRUE)
robust_tests <- list(
  "Main: shuffle within agency and month" = pair_test,
  "Remove two possible duplicate reports" = close_pair_test(deduplicated, radii = 1000, lags = 7),
  "Exclude pairs at identical coordinates" = close_pair_test(accidents, radii = 1000, lags = 7,
                                                           exclude_zero = TRUE),
  "Shuffle within month only" = close_pair_test(accidents, radii = 1000, lags = 7,
                                               strata = month(accidents$incident_time))
)
robustness <- imap_dfr(robust_tests, function(test, name) {
  test$results %>% filter(radius_m == 1000, lag_days == 7) %>% mutate(check = name)
})

# Independent direct distance-matrix check of the main observed count.
# This includes zero-distance pairs and avoids any dependence on closepairs().
distance_matrix <- as.matrix(dist(st_coordinates(accidents)))
time_gaps <- abs(outer(as.numeric(accidents$incident_time) / 86400,
                       as.numeric(accidents$incident_time) / 86400, "-"))
direct_count <- sum(upper.tri(distance_matrix) & distance_matrix <= 1000 & time_gaps <= 7)
stopifnot(direct_count == primary$observed,
          nrow(accidents) - nrow(deduplicated) == 2L,
          sum(quarter_counts$n) == nrow(accidents),
          all(is.finite(pair_test$results$p_adjusted)))
rm(distance_matrix, time_gaps)

write_csv(kde_peaks, file.path(derived_dir, "kde_peaks.csv"))
write_csv(quarter_counts, file.path(derived_dir, "quarter_province_counts.csv"))
write_csv(pair_test$results, file.path(derived_dir, "space_time_pair_tests.csv"))
write_csv(robustness, file.path(derived_dir, "space_time_robustness.csv"))
saveRDS(list(kde_annual = kde_annual, kde_quarterly = kde_quarterly,
             kde_peaks = kde_peaks, quarter_counts = quarter_counts,
             pair_test = pair_test, primary = primary, robustness = robustness),
        file.path(derived_dir, "completed_analysis.rds"))
writeLines(capture.output(sessionInfo()), file.path(derived_dir, "session-info.txt"))
print(kde_peaks)
print(primary)
print(robustness %>% select(check, observed, expected, excess_pct, p_upper))
cat("Nine-scale global permutation p:", pair_test$global_p, "\n")
