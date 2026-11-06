# Use the existing housing and POI data to locate demand within each subzone.
# ---- th03-distributed-demand ----
make_demand_locations <- function(prepared, cell_size = 500) {
  origins <- prepared$origins
  buildings <- prepared$buildings %>%
    left_join(st_drop_geometry(origins) %>% select(origin_id, hdb_residents), by = "origin_id") %>%
    group_by(origin_id) %>%
    mutate(residents = hdb_residents * total_dwelling_units / sum(total_dwelling_units)) %>% ungroup()
  remaining <- st_drop_geometry(origins) %>%
    left_join(st_drop_geometry(buildings) %>% group_by(origin_id) %>%
                summarise(allocated = sum(residents)), by = "origin_id") %>%
    mutate(residents = residents - replace_na(allocated, 0))
  b <- st_coordinates(buildings)
  o <- st_coordinates(origins)
  p <- st_coordinates(prepared$pois)
  locations <- bind_rows(
    tibble(subzone_id = buildings$origin_id, x = b[, 1], y = b[, 2],
           weight = buildings$residents, kind = "residential"),
    tibble(subzone_id = origins$origin_id, x = o[, 1], y = o[, 2],
           weight = remaining$residents, kind = "residential"),
    tibble(subzone_id = prepared$pois$origin_id, x = p[, 1], y = p[, 2],
           weight = 1, kind = "activity")) %>% filter(weight > 0)
  # Group nearby points separately for housing and activity, retaining their totals.
  locations <- locations %>%
    group_by(subzone_id, kind, gx = floor(x / cell_size), gy = floor(y / cell_size)) %>%
    summarise(x = weighted.mean(x, weight), y = weighted.mean(y, weight),
              weight = sum(weight), .groups = "drop") %>%
    mutate(origin_id = sprintf("D%04d", row_number()),
           residents = if_else(kind == "residential", weight, 0),
           activity = if_else(kind == "activity", weight, 0)) %>%
    st_as_sf(coords = c("x", "y"), crs = 3414)
  totals <- st_drop_geometry(locations) %>% group_by(subzone_id) %>%
    summarise(residents = sum(residents), activity = sum(activity))
  expected <- st_drop_geometry(origins) %>% filter(residents > 0 | activity > 0) %>%
    select(subzone_id = origin_id, residents, activity)
  check <- left_join(expected, totals, by = "subzone_id", suffix = c("", "_allocated"))
  stopifnot(!anyNA(check), !anyDuplicated(locations$origin_id),
            max(abs(check$residents - check$residents_allocated)) < 1e-6,
            all(check$activity == check$activity_allocated))
  locations
}

# Office footprints provide a separate activity proxy. Ground area is not floor space.
make_office_locations <- function(prepared) {
  buildings <- st_read("data/take-home-03/raw/osm-singapore.gpkg",
    query = "SELECT other_tags, geom FROM multipolygons WHERE building = 'office'", quiet = TRUE) %>%
    st_make_valid() %>% st_transform(3414)
  buildings <- buildings[lengths(st_intersects(buildings, prepared$subzones)) > 0, ]
  coverage <- tibble(office_buildings = nrow(buildings),
    with_levels = sum(grepl('"building:levels"=>"[0-9.]+"', buildings$other_tags)))
  locations <- st_as_sf(st_cast(st_union(buildings), "POLYGON"))
  locations$office_area <- as.numeric(st_area(locations))
  locations <- st_point_on_surface(locations)
  hits <- st_intersects(locations, prepared$subzones)
  stopifnot(all(lengths(hits) > 0))
  locations <- locations %>% mutate(origin_id = sprintf("W%04d", row_number()),
    subzone_id = prepared$subzones$origin_id[vapply(hits, `[`, integer(1), 1)],
    kind = "office", residents = 0, activity = 0)
  locations <- st_sf(st_drop_geometry(locations), geometry = st_geometry(locations))
  list(locations = locations, coverage = coverage)
}

make_interactions <- function(origins, destinations) {
  distances <- units::drop_units(st_distance(origins, destinations)) / 1000
  expand_grid(origin_id = origins$origin_id, station_id = destinations$station_id) %>%
    mutate(distance_km = as.vector(t(distances))) %>%
    left_join(st_drop_geometry(destinations) %>%
                select(station_id, lines, codes, starts_with("bus_")), by = "station_id")
}

# Return demand-weighted probabilities to the 332 subzones for the catchment maps.
subzone_probabilities <- function(probabilities, locations, subzones, residential_share,
                                  fallback, office_share = 0, employment_share = 0) {
  demand <- origin_weights(locations, residential_share, office_share, employment_share)
  totals <- rowsum(probabilities * demand, locations$subzone_id, reorder = FALSE)
  weights <- rowsum(matrix(demand), locations$subzone_id, reorder = FALSE)[, 1]
  active <- weights > 0
  index <- match(rownames(totals)[active], subzones$origin_id)
  fallback[index, ] <- totals[active, , drop = FALSE] / weights[active]
  stopifnot(max(abs(rowSums(fallback) - 1)) < 1e-10)
  fallback
}
