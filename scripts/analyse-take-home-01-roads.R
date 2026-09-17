# Run after the report has prepared the accidents and the road download is ready.
library(sf)
library(tidyverse)
library(spNetwork)

derived_dir <- "data/take-home-01/derived"
accidents <- readRDS(file.path(derived_dir, "accidents_2022_bmr.rds"))
centre <- st_transform(st_sfc(st_point(c(100.714, 13.733)), crs = 4326), 32647)
study_window <- st_as_sfc(st_bbox(st_buffer(centre, 5000)))
accidents <- accidents[lengths(st_intersects(accidents, study_window)) > 0, ]
roads <- readRDS(file.path(derived_dir, "eastern_bangkok_roads_raw.rds"))
drive_classes <- c("motorway", "trunk", "primary", "secondary", "tertiary",
                  "motorway_link", "trunk_link", "primary_link", "secondary_link",
                  "tertiary_link", "unclassified")
roads <- roads %>% filter(fclass %in% drive_classes, !st_is_empty(geometry))
roads <- roads[lengths(st_intersects(roads, st_buffer(study_window, 1500))) > 0, ]
roads <- st_cast(roads, "LINESTRING", warn = FALSE)
roads <- roads[as.numeric(st_length(roads)) > 0, ]
cat("Motor-road features:", nrow(roads), "\n")

# Record the displacement before moving a point to a line. An expressway report
# beside a surface road or between different levels needs a more careful match.
nearest <- st_nearest_feature(accidents, roads)
offset <- as.numeric(st_distance(accidents, roads[nearest, ], by_element = TRUE))
nearby <- st_intersects(st_buffer(accidents, 60), roads)
different_level <- vapply(seq_len(nrow(accidents)), function(i) {
  ids <- nearby[[i]]
  ids <- ids[roads$layer[ids] != roads$layer[nearest[i]] |
               roads$bridge[ids] != roads$bridge[nearest[i]] |
               roads$tunnel[ids] != roads$tunnel[nearest[i]]]
  length(ids) > 0 && any(as.numeric(st_distance(accidents[i, ], roads[ids, ])) <= offset[i] + 5)
}, logical(1))
expressway_conflict <- grepl("expressway", accidents$agency, ignore.case = TRUE) &
  !roads$fclass[nearest] %in% c("motorway", "motorway_link", "trunk", "trunk_link")
match_audit <- accidents %>% st_drop_geometry() %>%
  transmute(acc_code, agency, province_geom, route, offset_m = offset,
            osm_id = roads$osm_id[nearest], road_class = roads$fclass[nearest],
            road_ref = roads$ref[nearest], road_name = roads$name[nearest],
            different_level, expressway_conflict,
            retained = offset <= 50 & !different_level & !expressway_conflict)
write_csv(match_audit, file.path(derived_dir, "road_match_audit.csv"))
cat("Distance to nearest road (m):\n")
print(quantile(offset, c(0, .5, .9, .95, .99, 1)))
print(match_audit %>% count(retained, different_level, expressway_conflict))
print(match_audit %>% filter(retained) %>% count(road_ref, road_name, sort = TRUE) %>% head(12))

# Split at shared mapped vertices, not at every geometric crossing. Bridges
# crossing surface roads therefore do not acquire an invented intersection.
coords <- st_coordinates(roads)
keys <- unlist(roads$node_ids, use.names = FALSE)
stopifnot(length(keys) == nrow(coords))
shared <- unique(keys[duplicated(keys)])
by_line <- split(seq_len(nrow(coords)), coords[, "L1"])
pieces <- lapply(by_line, function(ids) {
  cuts <- sort(unique(c(1L, which(keys[ids] %in% shared), length(ids))))
  lapply(seq_len(length(cuts) - 1L), function(j) {
    st_linestring(coords[ids[cuts[j]:cuts[j + 1L]], 1:2, drop = FALSE])
  })
})
road_index <- rep(seq_len(nrow(roads)), lengths(pieces))
network <- st_sf(st_drop_geometry(roads)[road_index, ],
                 geometry = st_sfc(unlist(pieces, recursive = FALSE), crs = st_crs(roads)))
network <- network[as.numeric(st_length(network)) > 0, ]
network$node_ids <- NULL
stopifnot(abs(sum(as.numeric(st_length(network))) - sum(as.numeric(st_length(roads)))) < 0.01)
saveRDS(list(roads = network, audit = match_audit), file.path(derived_dir, "road_preparation.rds"))
cat("Connected road pieces:", nrow(network), "\n")

events <- accidents[match_audit$retained, ]
events$offset_m <- offset[match_audit$retained]
events <- snapPointsToLines2(events, network)
cat("Estimating road density for", nrow(events), "matched reports...\n")
segments <- lixelize_lines(network, lx_length = 200, mindist = 50)
samples <- lines_center(segments)
# Compute in kilometres with a fixed bandwidth of 1 km. The discontinuous
# implementation divides by bandwidth internally, so a unit bandwidth avoids
# an extra scaling factor. A single-event integration check verifies the units.
crs_km <- "+proj=utm +zone=47 +datum=WGS84 +units=km +no_defs"
segments$density <- nkde(st_transform(network, crs_km),
                         events = st_transform(events, crs_km), w = rep(1, nrow(events)),
                         samples = st_transform(samples, crs_km),
                         kernel_name = "quartic", bw = 1,
                         method = "discontinuous", div = "bw", digits = 6,
                         tol = 0.000001, grid_shape = c(3, 3), max_depth = 20,
                         sparse = TRUE, verbose = FALSE)
stopifnot(all(is.finite(segments$density)), all(segments$density >= 0))
density_mass <- sum(segments$density * as.numeric(st_length(segments)) / 1000) / nrow(events)
cat("Approximate KDE mass / matched events:", density_mass, "\n")
stopifnot(density_mass > 0.90, density_mass < 1.10)
saveRDS(list(segments = segments, events = events), file.path(derived_dir, "road_density.rds"))
cat("Density estimated. Computing neighbours within 1 km along roads...\n")
nb <- network_listw(events, network, maxdistance = 1000, mindist = 0.001,
                    direction = NULL, matrice_type = "I", dist_func = "identity",
                    grid_shape = c(3, 3), digits = 3, tol = 0.001)
pairs <- map_dfr(seq_along(nb$nb_list), function(i) {
  js <- nb$nb_list[[i]]
  if (length(js) == 0 || all(js == 0)) return(NULL)
  distances <- nb$weights[[i]]
  tibble(i = i, j = js, distance = distances)
})
stopifnot(setequal(paste(pairs$i, pairs$j), paste(pairs$j, pairs$i)))
pairs <- pairs %>% filter(j > i)
xy <- st_coordinates(events)
straight <- as.matrix(dist(xy))
stopifnot(all(pairs$distance + 0.01 >= straight[as.matrix(pairs[c("i", "j")])]))
euclidean_ids <- which(upper.tri(straight) & straight <= 1000, arr.ind = TRUE)
euclidean <- tibble(i = euclidean_ids[, 1], j = euclidean_ids[, 2])
stopifnot(anyDuplicated(paste(pairs$i, pairs$j)) == 0L)

compare_pairs <- function(keep, seed = 62602) {
  selected <- which(keep)
  columns <- list("Straight line" = euclidean, "Along roads" = pairs)
  columns <- lapply(columns, function(p) p[p$i %in% selected & p$j %in% selected, ])
  times <- as.numeric(events$incident_time) / 86400
  groups <- split(selected, interaction(events$agency[keep], month(events$incident_time[keep])))
  count_pairs <- function(t) vapply(columns, function(p) sum(abs(t[p$i] - t[p$j]) <= 7), integer(1))
  observed <- count_pairs(times)
  set.seed(seed)
  simulations <- replicate(999, {
    shuffled <- times
    for (g in groups) shuffled[g] <- times[g[sample.int(length(g))]]
    count_pairs(shuffled)
  })
  tibble(measure = names(columns), records = length(selected),
         spatial_pairs = lengths(lapply(columns, function(p) p$i)),
         observed = observed, expected = rowMeans(simulations),
         excess_pct = 100 * (observed / expected - 1),
         p_value = (1 + rowSums(simulations >= observed)) / 1000)
}
comparison <- compare_pairs(rep(TRUE, nrow(events)))
tighter_matches <- compare_pairs(events$offset_m <= 25)
print(comparison)
print(tighter_matches)

# A straight-line neighbour can be on an unconnected road or require a longer
# route. This comparison uses the same snapped points and treats roads as
# undirected: it measures proximity along the network, not driving time.
inside <- lengths(st_intersects(samples, study_window)) > 0
segments <- segments[inside, ]
peak_order <- order(segments$density, decreasing = TRUE)
peaks <- segments[peak_order[seq_len(min(20, length(peak_order)))], ]
write_csv(st_drop_geometry(peaks), file.path(derived_dir, "road_density_peaks.csv"))
write_csv(comparison, file.path(derived_dir, "road_pair_comparison.csv"))
write_csv(tighter_matches, file.path(derived_dir, "road_pair_tighter_matches.csv"))
saveRDS(list(segments = segments, events = events, comparison = comparison,
             tighter_matches = tighter_matches, audit = match_audit, peaks = peaks,
             window = study_window, network_pairs = pairs, straight_pairs = euclidean),
        file.path(derived_dir, "road_analysis.rds"))
cat("Road analysis saved.\n")
