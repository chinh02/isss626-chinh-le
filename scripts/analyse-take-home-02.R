# Run from the repository root: Rscript scripts/analyse-take-home-02.R
# ---- th02-packages ----
suppressPackageStartupMessages({
  library(sf)
  library(tidyverse)
  library(spdep)
  library(sfdep)
  library(Kendall)
  library(jsonlite)
  library(digest)
})

# ---- th02-settings ----
base <- "data/take-home-02"
out <- file.path(base, "derived")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
seed <- 62602L
nsim <- 999L
alpha <- 0.05
months <- seq(as.Date("2021-01-01"), as.Date("2025-09-01"), by = "month")
projection <- "+proj=aea +lat_1=10 +lat_2=28 +lat_0=19 +lon_0=96 +datum=WGS84 +units=m +no_defs"

# ---- th02-import ----
message("Reading and checking the supplied data")
csv <- file.path(base, "raw/aspatial/ACLED_Data_Myanmar_Jan2021-Sep2025.csv")
archive <- file.path(base, "raw/aspatial/ACLED_Data_Myanmar_Jan2021-Sep2025 (1).zip")
if (!file.exists(csv) && file.exists(archive)) unzip(archive, exdir = dirname(csv))
stopifnot(file.exists(csv))
raw <- read_csv(csv, col_types = cols(.default = col_character()), show_col_types = FALSE)
stopifnot(nrow(problems(raw)) == 0, !anyDuplicated(raw$event_id_cnty))
raw <- raw %>% mutate(event_date = as.Date(event_date),
                      across(c(latitude, longitude, fatalities, geo_precision), as.numeric))
stopifnot(!anyNA(raw$event_date), !anyNA(raw$fatalities), all(raw$fatalities >= 0))
towns <- st_read(file.path(base, "raw/geospatial/mmr_polbnda_adm3_250k_mimu_1.shp"),
                 quiet = TRUE, options = "ENCODING=UTF-8") %>% arrange(TS_PCODE)
stopifnot(nrow(towns) == 330, !anyDuplicated(towns$TS_PCODE),
          all(towns$PCode_V == 9.4), all(st_is_valid(towns)))
# ---- th02-filter ----
normalise <- function(x) gsub("[^a-z0-9]", "", tolower(x))
town_key <- paste(normalise(towns$ST), normalise(towns$TS))
stopifnot(!anyDuplicated(town_key))
core_types <- c("Battles", "Explosions/Remote violence", "Violence against civilians")
core <- raw %>% filter(country == "Myanmar", event_date >= as.Date("2021-01-01"),
                       event_date <= as.Date("2025-09-30"), event_type %in% core_types)
# One spelling alias, checked against the four corresponding coordinate matches.
core <- core %>% mutate(town_name = if_else(admin3 == "Pangsang", "Pangsang (Panghkam)", admin3),
                        town_index = match(paste(normalise(admin1), normalise(town_name)), town_key),
                        exclusion = case_when(
                          is.na(admin3) | trimws(admin3) == "" ~ "No named township",
                          geo_precision == 3 ~ "Location only known to a wider area",
                          is.na(town_index) ~ "Township name could not be matched",
                          TRUE ~ "Retained"))
audit <- core %>% select(event_id_cnty, event_date, admin1, admin3, geo_precision,
                         latitude, longitude, town_index, exclusion)
write_csv(audit, file.path(out, "location-audit.csv"))
events <- core %>% filter(exclusion == "Retained") %>%
  mutate(TS_PCODE = towns$TS_PCODE[town_index], month = as.Date(format(event_date, "%Y-%m-01")),
         civilian = !is.na(civilian_targeting) & civilian_targeting == "Civilian targeting")
# ---- th02-coordinates ----
# Use recorded township names for coastal and boundary cases; do not snap points.
coord_key <- paste(events$longitude, events$latitude)
pts <- st_as_sf(events[!duplicated(coord_key), ], coords = c("longitude", "latitude"), crs = 4326)
hits <- st_intersects(pts, towns)
hit_index <- match(coord_key, unique(coord_key))
coord_match <- vapply(seq_len(nrow(events)), function(i)
  events$town_index[i] %in% hits[[hit_index[i]]], logical(1))
write_csv(events[!coord_match, ] %>% select(event_id_cnty, admin1, admin3, longitude, latitude),
          file.path(out, "coordinate-disagreements.csv"))

# ---- th02-cube ----
counts <- events %>% group_by(TS_PCODE, month) %>%
  summarise(events = n(), civilian = sum(civilian), fatalities = sum(fatalities), .groups = "drop")
cube_data <- expand_grid(month = months, TS_PCODE = towns$TS_PCODE) %>%
  left_join(counts, by = c("TS_PCODE", "month")) %>%
  mutate(across(c(events, civilian, fatalities), ~replace_na(.x, 0L)))
cube <- sfdep::spacetime(cube_data, towns, .loc_col = "TS_PCODE", .time_col = "month")
stopifnot(sfdep::is_spacetime_cube(cube), nrow(cube_data) == 330 * 57,
          sum(cube_data$events) == nrow(events), !anyNA(cube_data),
          identical(cube_data$TS_PCODE, rep(towns$TS_PCODE, length(months))))
saveRDS(cube, file.path(out, "space-time-cube.rds"))
write_csv(cube_data, file.path(out, "township-month-counts.csv"))
totals <- cube_data %>% group_by(TS_PCODE) %>%
  summarise(across(c(events, civilian, fatalities), sum), .groups = "drop")
towns <- towns %>% left_join(totals, by = "TS_PCODE") %>% st_transform(projection)

# ---- th02-neighbours ----
message("Local Moran's I and Gi*: Queen neighbours, 999 permutations")
nb <- poly2nb(towns, queen = TRUE, snap = 1, row.names = towns$TS_PCODE)
supported <- card(nb) > 0
lw <- nb2listw(nb, style = "W", zero.policy = TRUE)
nb_star <- include.self(nb)
lw_star <- nb2listw(nb_star, style = "W", zero.policy = TRUE)
# ---- th02-local-tests ----
class_lisa <- function(x, lag, p) case_when(
  is.na(p) ~ "No shared-boundary neighbour",
  p >= alpha ~ "Not significant",
  x > mean(x) & lag > mean(x) ~ "High-high",
  x > mean(x) & lag <= mean(x) ~ "High-low",
  x <= mean(x) & lag > mean(x) ~ "Low-high",
  TRUE ~ "Low-low")
gi_test <- function(x, listw, tested, random_seed) {
  g <- localG_perm(x, listw, nsim = nsim, alternative = "two.sided",
                  iseed = random_seed, zero.policy = TRUE, no_repeat_in_row = TRUE)
  p <- attr(g, "internals")[, "Pr(z != E(Gi)) Sim"]
  p[!tested] <- NA_real_
  tibble(gi = as.numeric(g), p = p, q = p.adjust(p, "BH"),
         cluster = case_when(is.na(p) ~ "No shared-boundary neighbour", p >= alpha ~ "Not significant",
                             as.numeric(g) > 0 ~ "Hotspot", TRUE ~ "Coldspot"))
}
local_results <- function(x, random_seed) {
  m <- localmoran_perm(x, lw, nsim = nsim, alternative = "two.sided", iseed = random_seed,
                       zero.policy = TRUE, no_repeat_in_row = TRUE)
  p <- m[, "Pr(z != E(Ii)) Sim"]; p[!supported] <- NA_real_
  lag <- lag.listw(lw, x, zero.policy = TRUE)
  g <- gi_test(x, lw_star, supported, random_seed + 1L)
  tibble(TS_PCODE = towns$TS_PCODE, moran_i = m[, "Ii"], moran_p = p,
         moran_q = p.adjust(p, "BH"), lisa = class_lisa(x, lag, p),
         gi = g$gi, gi_p = g$p, gi_q = g$q, gi_cluster = g$cluster)
}
# ---- th02-local-run ----
local_all <- local_results(towns$events, seed)
# ---- th02-civilian-run ----
local_civilian <- local_results(towns$civilian, seed + 10L)

# ---- th02-neighbour-checks ----
# Neighbourhood sensitivity: a symmetric six-nearest-centroid graph.
centres <- st_coordinates(st_centroid(st_geometry(towns)))
nb_knn <- knn2nb(knearneigh(centres, k = 6), sym = TRUE)
knn_g <- gi_test(towns$events, nb2listw(include.self(nb_knn), style = "W"),
                 rep(TRUE, nrow(towns)), seed + 20L)
precise_totals <- events %>% filter(geo_precision == 1) %>% count(TS_PCODE, name = "precise")
precise_x <- replace_na(precise_totals$precise[match(towns$TS_PCODE, precise_totals$TS_PCODE)], 0L)
precise_g <- gi_test(precise_x, lw_star, supported, seed + 30L)

# ---- th02-time-neighbours ----
# EHSA uses an explicit space-time neighbourhood. The same and previous month
# are included, with Gi* standardised against the entire cube. Unlike calling
# local_gstar_perm() separately by month, this is a space-time Gi* statistic.
make_st_listw <- function(spatial_nb, n_times, lag = 1L) {
  n <- length(spatial_nb)
  neighbours <- lapply(seq_len(n * n_times), function(i) {
    tt <- (i - 1L) %/% n + 1L; loc <- (i - 1L) %% n + 1L
    times <- seq.int(max(1L, tt - lag), tt)
    sort(as.integer(unlist(lapply(times, function(t) (t - 1L) * n + spatial_nb[[loc]]))))
  })
  class(neighbours) <- "nb"
  attr(neighbours, "region.id") <- as.character(seq_along(neighbours))
  attr(neighbours, "call") <- match.call()
  attr(neighbours, "sym") <- FALSE
  attr(neighbours, "self.included") <- TRUE
  nb2listw(neighbours, style = "W", zero.policy = TRUE)
}

# ---- th02-classification-rules ----
# Rules follow the ArcGIS EHSA definitions, with strict p < 0.05.
# Trend p-values and bin p-values answer different questions.
classify_ehsa <- function(z, p, tau, trend_p, threshold = alpha) {
  if (anyNA(p) || anyNA(z)) return("No shared-boundary neighbour")
  hot <- z > 0 & p < threshold; cold <- z < 0 & p < threshold
  n <- length(z)
  for (side in c("hot", "cold")) {
    hit <- if (side == "hot") hot else cold
    opposite <- if (side == "hot") cold else hot
    suffix <- paste0(side, "spot")
    share <- mean(hit); final <- hit[n]
    direction <- if (side == "hot") tau else -tau
    if (final && sum(hit) == 1) return(paste("New", suffix))
    trailing <- if (final) rle(rev(hit))$lengths[1] else 0L
    if (final && trailing >= 2 && sum(hit) == trailing && share < .9)
      return(paste("Consecutive", suffix))
    if (share >= .9 && !final) return(paste("Historical", suffix))
    if (share >= .9 && final) {
      if (trend_p >= threshold) return(paste("Persistent", suffix))
      return(paste(if (direction > 0) "Intensifying" else "Diminishing", suffix))
    }
    if (final && share < .9)
      return(paste(if (any(opposite)) "Oscillating" else "Sporadic", suffix))
  }
  "No pattern detected"
}
# ---- th02-rule-tests ----
# Meaningful checks for sign, final-month status and the 90% persistence rule.
stopifnot(
  classify_ehsa(c(rep(0, 9), 2), c(rep(1, 9), .01), .2, .01) == "New hotspot",
  classify_ehsa(c(rep(0, 8), 2, 2), c(rep(1, 8), .01, .01), .2, .01) == "Consecutive hotspot",
  classify_ehsa(rep(-2, 10), rep(.01, 10), -.8, .01) == "Intensifying coldspot",
  classify_ehsa(rep(-2, 10), rep(.01, 10), .8, .01) == "Diminishing coldspot",
  classify_ehsa(rep(2, 10), rep(.01, 10), .1, .5) == "Persistent hotspot",
  classify_ehsa(c(rep(2, 9), .1), c(rep(.01, 9), 1), -.2, .01) == "Historical hotspot",
  classify_ehsa(c(2,0,2,0,2), c(.01,1,.01,1,.01), .1,.5) == "Sporadic hotspot",
  classify_ehsa(c(2,0,2,0,0), c(.01,1,.01,1,1), -.1,.5) == "No pattern detected",
  classify_ehsa(c(2,-2,0,2), c(.01,.01,1,.01), .1,.5) == "Oscillating hotspot")

# ---- th02-time-gi ----
st_lw <- make_st_listw(nb_star, length(months))
stopifnot(all(vapply(seq_along(st_lw$neighbours), function(i)
  i %in% st_lw$neighbours[[i]] && all((st_lw$neighbours[[i]] - 1L) %/% 330 <= (i - 1L) %/% 330), logical(1))))
message("Space-time Gi*: 18,810 bins, current/previous month, 999 permutations")
gi_key <- digest(list(cube_data$events, st_lw, supported, seed, nsim, body(gi_test), packageVersion("spdep")))
gi_cache_path <- file.path(out, "space-time-gi-cache.rds")
gi_cache <- if (file.exists(gi_cache_path)) readRDS(gi_cache_path) else NULL
if (!is.null(gi_cache) && identical(gi_cache$key, gi_key)) {
  st_g <- gi_cache$result
} else {
  st_g <- gi_test(cube_data$events, st_lw, rep(supported, length(months)), seed + 40L)
  saveRDS(list(key = gi_key, result = st_g), gi_cache_path)
}
# ---- th02-mann-kendall ----
gi_series <- bind_cols(cube_data %>% select(TS_PCODE, month, events, civilian),
                       st_g %>% select(gi, p)) %>%
  group_by(month) %>% mutate(q = p.adjust(p, "BH")) %>% ungroup()
mk <- gi_series %>% group_by(TS_PCODE) %>% group_modify(~ {
  x <- .x$gi
  if (all(is.na(.x$p))) return(tibble(tau = NA_real_, trend_p = NA_real_, lag1 = NA_real_))
  m <- Kendall::MannKendall(x)
  tibble(tau = as.numeric(m$tau), trend_p = as.numeric(m$sl),
         lag1 = as.numeric(acf(x, lag.max = 1, plot = FALSE)$acf[2]))
}) %>% ungroup() %>% mutate(trend_q = p.adjust(trend_p, "BH"))

# ---- th02-block-function ----
# A secondary trend check preserves short runs of correlated months. Resample
# six-month blocks of residuals after removing the Sen slope, then recompute tau.
# This is an approximate dependence check, not proof that seasonality is absent.
block_trend_p <- function(x, B = 999L, block = 6L) {
  if (length(unique(x)) < 2L) return(1)
  n <- length(x); pairs <- combn(seq_len(n), 2)
  slope <- median((x[pairs[2, ]] - x[pairs[1, ]]) / (pairs[2, ] - pairs[1, ]))
  residual <- x - slope * seq_len(n)
  obs <- as.numeric(Kendall::MannKendall(x)$tau)
  reps <- replicate(B, {
    starts <- sample.int(n, ceiling(n / block), replace = TRUE)
    ids <- unlist(lapply(starts, function(s) (s - 1L + 0:(block - 1L)) %% n + 1L))[seq_len(n)]
    y <- residual[ids]
    if (length(unique(y)) < 2L) 0 else as.numeric(Kendall::MannKendall(y)$tau)
  })
  (1 + sum(abs(reps) >= abs(obs))) / (B + 1)
}
# ---- th02-block-run ----
message("Checking the effect of correlated months on trend tests")
set.seed(seed + 50L)
block_p <- vapply(towns$TS_PCODE, function(id) {
  x <- gi_series[gi_series$TS_PCODE == id, ]
  if (all(is.na(x$p))) NA_real_ else block_trend_p(x$gi)
}, numeric(1))
mk$block_p <- block_p[match(mk$TS_PCODE, towns$TS_PCODE)]
mk$block_q <- p.adjust(mk$block_p, "BH")
# ---- th02-classify ----
ehsa <- gi_series %>% left_join(mk, by = "TS_PCODE") %>% group_by(TS_PCODE) %>%
  summarise(tau = first(tau), trend_p = first(trend_p), trend_q = first(trend_q),
            block_p = first(block_p), block_q = first(block_q), lag1 = first(lag1),
            hot_months = sum(gi > 0 & p < alpha, na.rm = TRUE),
            cold_months = sum(gi < 0 & p < alpha, na.rm = TRUE),
            classification = classify_ehsa(gi, p, first(tau), first(trend_p)),
            classification_fdr = classify_ehsa(gi, q, first(tau), first(trend_q)), .groups = "drop") %>%
  mutate(map_class = case_when(is.na(trend_p) ~ "No shared-boundary neighbour",
                              trend_p >= alpha ~ "Trend not significant",
                              TRUE ~ classification))

# ---- th02-save-results ----
comparison <- tibble(TS_PCODE = towns$TS_PCODE,
                     queen = local_all$gi_cluster, knn6 = knn_g$cluster,
                     precise_only = precise_g$cluster,
                     queen_fdr = if_else(local_all$gi_q < alpha, local_all$gi_cluster, "Not significant"),
                     civilian = local_civilian$gi_cluster)
validation <- list(
  rows_supplied = nrow(raw), country_counts = as.list(table(raw$country)),
  core_rows = nrow(core), retained = nrow(events), exclusions = as.list(table(core$exclusion)),
  civilian_records = sum(events$civilian), coordinate_disagreements = sum(!coord_match),
  months = length(months), bins = nrow(cube_data), zero_event_bins = sum(cube_data$events == 0),
  islands = as.list(towns$TS[!supported]), neighbours = as.list(summary(card(nb)[supported])),
  queen_knn_agreement = mean(comparison$queen[supported] == comparison$knn6[supported]),
  queen_precise_agreement = mean(comparison$queen[supported] == comparison$precise_only[supported]),
  simulations = nsim, seed = seed, input_sha256 = digest(csv, algo = "sha256", file = TRUE),
  methods = "Two-sided conditional randomisation; Queen W weights; current and preceding month; whole-cube Gi* reference; MK on Gi*; BH and six-month block checks.")
write_json(validation, file.path(out, "validation.json"), pretty = TRUE, auto_unbox = TRUE)
for (name in c("local_all", "local_civilian", "gi_series", "ehsa", "comparison"))
  write_csv(get(name), file.path(out, paste0(name, ".csv")))
# ---- th02-descriptive-counts ----
monthly <- events %>% count(month, event_type, name = "events") %>%
  complete(month = months, event_type = core_types, fill = list(events = 0))
annual <- events %>% mutate(year = lubridate::year(event_date)) %>%
  group_by(year) %>% summarise(events = n(), civilian = sum(civilian), fatalities = sum(fatalities),
                              jan_sep = sum(lubridate::month(event_date) <= 9), .groups = "drop")
state_totals <- events %>% group_by(admin1) %>% summarise(events = n(), civilian = sum(civilian), .groups = "drop") %>% arrange(desc(events))
# ---- th02-save-analysis ----
saveRDS(list(towns = towns, cube = cube_data, monthly = monthly, annual = annual,
             state_totals = state_totals, local = local_all, civilian = local_civilian,
             gi_series = gi_series, ehsa = ehsa, comparison = comparison,
             validation = validation, nb = nb), file.path(out, "analysis.rds"))
writeLines(capture.output(sessionInfo()), file.path(out, "session-info.txt"))
message("Analysis saved to ", out)
print(validation)
print(annual)
print(ehsa %>% count(classification, map_class))
print(towns %>% st_drop_geometry() %>% select(TS, ST, events, civilian) %>% arrange(desc(events)) %>% head(12))
