suppressPackageStartupMessages({
  library(sf)
  library(tidyverse)
  library(leaflet)
  library(htmlwidgets)
  library(ggrepel)
})
analysis <- readRDS("data/take-home-03/derived/analysis.rds")
prepared <- analysis$prepared
ridership <- analysis$ridership
details <- analysis$details
subzones <- prepared$subzones
stations <- prepared$stations
dir.create("take-home-03", showWarnings = FALSE)
theme_set(theme_minimal(base_size = 12) +
            theme(panel.grid.minor = element_blank(), plot.title.position = "plot",
                  plot.title = element_text(face = "bold"), legend.position = "bottom"))
save_plot <- function(plot, name, width = 10, height = 6) {
  ggsave(file.path("take-home-03", paste0(name, ".png")), plot, width = width, height = height,
         dpi = 160, bg = "white")
}

# ---- th03-network-map ----
new_stations <- stations$Both %>% filter(!existing) %>%
  mutate(line = if_else(startsWith(codes, "J"), "JRL", "CRL Phase 1"))
upgrades <- stations$Both %>% filter(existing) %>%
  filter(lines > stations$Existing$lines[match(station_id, stations$Existing$station_id)])
network_labels <- bind_rows(upgrades, new_stations %>%
  filter(station_id %in% c("AVIATIONPARK", "SERANGOONNORTH", "NANYANGCRESCENT", "TENGAH")))
network_map <- ggplot() +
  geom_sf(data = subzones, fill = "grey97", colour = "grey83", linewidth = 0.15) +
  geom_sf(data = stations$Existing, colour = "grey55", size = 1.2) +
  geom_sf(data = new_stations, aes(colour = line), size = 2.5) +
  geom_sf(data = upgrades, shape = 21, fill = "white", size = 2.6) +
  geom_label_repel(data = network_labels, aes(label = station, geometry = geometry),
    stat = "sf_coordinates", size = 2.7, seed = 626, min.segment.length = 0, max.overlaps = Inf) +
  scale_colour_brewer(palette = "Dark2") +
  coord_sf(datum = NA) + labs(colour = NULL, x = NULL, y = NULL,
    title = "Where the proposed stations would extend the network",
    subtitle = "New locations in colour; outlined stations gain a connecting line")

# ---- th03-access-map ----
local_access <- subzones %>% left_join(prepared$local_access %>% select(origin_id, gain_m), by = "origin_id")
access_map <- ggplot(local_access) +
  geom_sf(aes(fill = gain_m), colour = "white", linewidth = 0.15) +
  geom_sf(data = new_stations, size = 0.8, colour = "grey20") +
  scale_fill_viridis_c(option = "C", na.value = "grey90", breaks = scales::breaks_pretty(n = 3),
                       labels = scales::label_number(suffix = " m", big.mark = ","),
                       guide = guide_colourbar(barwidth = grid::unit(6, "cm"))) +
  coord_sf(datum = NA) + labs(fill = "Mean distance saved", x = NULL, y = NULL,
    title = "How much closer would the nearest station be?",
    subtitle = "JRL and CRL Phase 1 combined; averages weighted by mapped HDB dwellings")

# ---- th03-access-coverage ----
coverage <- prepared$coverage %>%
  mutate(scenario = factor(scenario, levels = c("Existing", "JRL", "CRL", "Both")),
         threshold = factor(threshold, levels = c(400, 800, 1200), labels = c("400 metres", "800 metres", "1,200 metres")))
coverage_chart <- ggplot(coverage, aes(scenario, share, fill = threshold)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = scales::percent(share, accuracy = 0.1)),
            position = position_dodge(width = 0.8), vjust = -0.5, size = 3) +
  scale_fill_brewer(palette = "Blues", direction = -1) +
  scale_y_continuous(labels = scales::label_percent(), limits = c(0, 1.08), breaks = seq(0, 1, 0.2)) +
  labs(x = NULL, y = "Share of mapped HDB dwellings", fill = "Distance to nearest station",
       title = "More homes within reach of a station",
       subtitle = "Straight-line distances to station locations")

# ---- th03-age-comparison ----
equity_chart <- prepared$equity %>% filter(scenario %in% c("Existing", "Both")) %>%
  mutate(scenario = factor(scenario, levels = c("Existing", "Both"), labels = c("Existing network", "With both lines"))) %>%
  ggplot(aes(age_group, mean_distance, colour = scenario, group = age_group)) +
  geom_line(colour = "grey70", linewidth = 1) + geom_point(size = 3) +
  scale_colour_brewer(palette = "Dark2") +
  coord_flip() + labs(x = "Subzones grouped by the share of HDB residents aged 65+",
                      y = "Mean distance to nearest station (metres)", colour = NULL,
                      title = "Do older neighbourhoods receive similar improvements?")

# ---- th03-local-gains ----
local_chart <- prepared$local_access %>% slice_max(gain_m, n = 8, with_ties = FALSE) %>%
  left_join(st_drop_geometry(subzones) %>% select(origin_id, planning_area), by = "origin_id") %>%
  mutate(place = paste0(str_to_title(subzone), " (", str_to_title(planning_area), ")")) %>%
  ggplot(aes(gain_m, reorder(place, gain_m))) +
  geom_col(fill = "#237a83", width = 0.65) +
  geom_text(aes(label = paste0(round(gain_m), " m")), hjust = -0.15, size = 3.5) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(x = "Mean distance saved per mapped dwelling (metres)", y = NULL,
       title = "The largest improvements in neighbourhood access",
       subtitle = "Both proposed lines added to the existing network")

# ---- th03-export-access ----
save_plot(network_map, "network")
save_plot(network_map + labs(title = NULL, subtitle = NULL) + theme(text = element_text(size = 17)),
          "network-summary", width = 7, height = 5)
save_plot(access_map, "access-map")
save_plot(coverage_chart, "coverage")
save_plot(equity_chart, "equity")
save_plot(local_chart, "local-gains")
summary_coverage <- prepared$coverage %>% filter(threshold == 800) %>%
  mutate(scenario = factor(scenario, levels = c("Existing", "JRL", "CRL", "Both"))) %>%
  ggplot(aes(scenario, share)) + geom_col(fill = "#237a83", width = 0.65) +
  geom_text(aes(label = scales::percent(share, accuracy = 0.1)), vjust = -0.5, size = 5) +
  scale_y_continuous(labels = scales::label_percent(), limits = c(0, 1)) +
  labs(x = NULL, y = "Dwellings within 800 metres") + theme(text = element_text(size = 17))
save_plot(summary_coverage, "coverage-summary", width = 7, height = 5)
save_plot(equity_chart + labs(title = NULL, x = NULL, y = "Mean distance (metres)") +
            theme(text = element_text(size = 17), legend.text = element_text(size = 13)),
          "equity-summary", width = 7, height = 5)

# ---- th03-scenario-explorer ----
scenario_map <- leaflet(options = leafletOptions(minZoom = 10)) %>%
  addPolygons(data = st_transform(subzones, 4326), weight = 0.5, color = "#b8c2c4",
              fillColor = "#f5f5f0", fillOpacity = 1,
              label = ~paste(str_to_title(subzone), str_to_title(planning_area), sep = ", ")) %>%
  addControl("Boundaries: URA", position = "bottomleft") %>%
  setView(lng = 103.83, lat = 1.35, zoom = 11)
palette <- colorFactor("Dark2", c("Existing", "New"))
probability_palette <- colorNumeric("Blues", domain = c(0, 1), na.color = "#eeeeee")
for (scenario in names(stations)) {
  points <- st_transform(stations[[scenario]], 4326) %>%
    mutate(status = if_else(existing, "Existing", "New"),
           label = paste0(station, "<br>", codes, "<br>", lines, " connecting line(s)"))
  if (!is.null(ridership)) {
    values <- ridership$predictions %>% filter(.data$scenario == .env$scenario) %>% select(station_id, predicted)
    points <- points %>% left_join(values, by = "station_id") %>%
      mutate(label = paste0(label, "<br>Estimated weekday entries: ", scales::comma(round(predicted, -2))))
    areas <- subzones %>% st_simplify(dTolerance = 20) %>% st_transform(4326) %>%
      left_join(details$catchments %>% filter(.data$scenario == .env$scenario), by = "origin_id") %>%
      mutate(label = if_else(active,
        paste0(str_to_title(subzone), "<br>Most likely station: ", station,
               "<br>Probability: ", scales::percent(probability, accuracy = 0.1)),
        paste0(str_to_title(subzone), "<br>No population or activity demand in the model")))
    scenario_map <- scenario_map %>% addPolygons(data = areas, group = scenario,
      color = "#aab4b6", weight = 0.4, fillOpacity = 0.65,
      fillColor = ~probability_palette(if_else(active, probability, NA_real_)), popup = ~label)
  }
  scenario_map <- scenario_map %>% addCircleMarkers(data = points, group = scenario,
    radius = 4, color = ~palette(status), weight = 1, fillOpacity = 0.85, popup = ~label)
}
scenario_map <- scenario_map %>%
  addLayersControl(baseGroups = names(stations), options = layersControlOptions(collapsed = FALSE)) %>%
  hideGroup(c("JRL", "CRL", "Both")) %>%
  addLegend(position = "bottomright", pal = palette, values = c("Existing", "New"), title = "Station location")
if (!is.null(ridership)) scenario_map <- scenario_map %>%
  addLegend(position = "bottomleft", pal = probability_palette, values = c(0, 1), title = "Highest choice probability")
saveWidget(scenario_map, "take-home-03/network-explorer.html", selfcontained = FALSE)

if (!is.null(ridership)) {
# ---- th03-observed-map ----
  observed_map <- ggplot() +
    geom_sf(data = subzones, fill = "grey97", colour = "grey85", linewidth = 0.15) +
    geom_sf(data = ridership$observed, aes(size = entries, colour = entries), alpha = 0.8) +
    scale_colour_viridis_c(labels = scales::label_comma(), breaks = scales::breaks_pretty(n = 3),
                          guide = guide_colourbar(barwidth = grid::unit(6, "cm"))) +
    scale_size_area(max_size = 10, guide = "none") + coord_sf(datum = NA) +
    labs(colour = "Weekday entries", size = "Weekday entries", x = NULL, y = NULL,
         title = "Where passengers enter the network", subtitle = "Average weekday, September 2026")

# ---- th03-station-ranking ----
  ranking_chart <- ridership$observed %>% st_drop_geometry() %>%
    slice_max(entries, n = 12) %>% ggplot(aes(entries, reorder(station, entries))) +
    geom_col(fill = "#237a83", width = 0.7) + scale_x_continuous(labels = scales::label_comma()) +
    labs(x = "Average weekday entries", y = NULL, title = "The busiest boarding stations")

# ---- th03-validation-chart ----
  validation_values <- ridership$validation %>% filter(model == ridership$fit$model) %>%
    left_join(st_drop_geometry(stations$Existing) %>% select(station_id, station), by = "station_id")
  labelled_errors <- validation_values %>% slice_max(abs(predicted - observed), n = 4)
  validation_chart <- ggplot(validation_values, aes(observed, predicted)) +
    geom_abline(slope = 1, intercept = 0, colour = "grey60") +
    geom_point(alpha = 0.65, colour = "#237a83") + coord_equal() +
    geom_text_repel(data = labelled_errors, aes(label = station), seed = 626, size = 3,
      min.segment.length = 0) +
    scale_x_continuous(labels = scales::label_comma()) + scale_y_continuous(labels = scales::label_comma()) +
    labs(x = "Observed entries", y = "Predicted entries", title = "How well does the model predict held-out stations?",
         subtitle = "Each station was predicted without using its own ridership to fit the model")
# ---- th03-error-map ----
  errors <- stations$Existing %>% left_join(ridership$validation %>% filter(model == ridership$fit$model), by = "station_id")
  error_limit <- max(abs(errors$predicted - errors$observed))
  error_map <- ggplot() + geom_sf(data = subzones, fill = "grey97", colour = "grey85", linewidth = 0.15) +
    geom_sf(data = errors, aes(colour = predicted - observed, size = abs(predicted - observed)), alpha = 0.8) +
    geom_label_repel(data = errors %>% filter(station_id %in% labelled_errors$station_id),
      aes(label = station, geometry = geometry), stat = "sf_coordinates", seed = 626,
      size = 3, min.segment.length = 0) +
    scale_size_area(max_size = 8, guide = "none") +
    scale_colour_gradient2(low = "#2166ac", mid = "white", high = "#b2182b", midpoint = 0,
                           limits = c(-error_limit, error_limit), breaks = c(-40000, -20000, 0, 20000, 40000),
                           labels = scales::label_number(scale = 0.001, suffix = "k"),
                           guide = guide_colourbar(barwidth = grid::unit(6, "cm"))) + coord_sf(datum = NA) +
    labs(x = NULL, y = NULL, colour = "Prediction minus observation", title = "Where the model misses the observed counts")

# ---- th03-new-station-estimates ----
  ranges <- ridership$sensitivity %>% filter(scenario == "Both", check != "Residential demand only") %>% group_by(station_id) %>%
    summarise(low = min(predicted), high = max(predicted), .groups = "drop")
  residential <- ridership$sensitivity %>% filter(scenario == "Both", check == "Residential demand only") %>%
    select(station_id, residential = predicted)
  new_chart <- ridership$predictions %>% filter(scenario == "Both", !existing) %>%
    left_join(ranges, by = "station_id") %>%
    left_join(residential, by = "station_id") %>%
    mutate(line = if_else(startsWith(codes, "J"), "JRL", "CRL Phase 1")) %>%
    ggplot(aes(predicted, reorder(station, predicted))) +
    geom_errorbar(aes(xmin = low, xmax = high), orientation = "y", width = 0.25, colour = "grey60") +
    geom_point(aes(colour = line), size = 2) +
    geom_point(aes(x = residential, colour = line), shape = 1, size = 2) +
    scale_colour_brewer(palette = "Dark2", name = NULL) +
    scale_x_continuous(labels = scales::label_comma()) +
    labs(x = "Estimated average weekday entries", y = NULL, title = "Potential boardings at new stations",
         subtitle = "Filled: selected model. Hollow: residents only. Bars: buffer and distance-floor checks.")

# ---- th03-existing-station-changes ----
  changes_chart <- ridership$predictions %>% filter(scenario == "Both", existing) %>%
    slice_max(abs(change), n = 12) %>% ggplot(aes(change, reorder(station, change), fill = change > 0)) +
    geom_col(width = 0.7) + geom_vline(xintercept = 0, colour = "grey50") +
    scale_fill_brewer(palette = "Set2", guide = "none") + scale_x_continuous(labels = scales::label_comma()) +
    labs(x = "Change in estimated weekday entries", y = NULL, title = "Where existing stations could gain or lose boardings")

# ---- th03-catchment-changes ----
  choices <- details$catchments %>% select(origin_id, scenario, station_id, active) %>%
    pivot_wider(names_from = scenario, values_from = station_id) %>%
    mutate(changed = case_when(!active ~ "No modelled origin demand",
                              Existing == Both ~ "Same most likely station",
                              TRUE ~ "Different most likely station"))
  catchment_map <- subzones %>% left_join(choices, by = "origin_id") %>% ggplot() +
    geom_sf(aes(fill = changed), colour = "white", linewidth = 0.15) +
    geom_sf(data = new_stations, size = 0.8) +
    scale_fill_manual(values = c("Different most likely station" = "#1b9e77",
      "Same most likely station" = "#e6eee9", "No modelled origin demand" = "grey85")) + coord_sf(datum = NA) +
    labs(x = NULL, y = NULL, fill = NULL, title = "Where the most likely station changes",
         subtitle = "Subzone-level Huff probabilities; both proposed lines added")

# ---- th03-local-choice-chart ----
  local_probabilities <- details$local_choices %>% select(station, scenario, probability) %>%
    complete(station, scenario, fill = list(probability = 0)) %>%
    mutate(scenario = factor(scenario, levels = c("Existing", "Both"),
                             labels = c("Existing network", "With both lines")))
  local_choice_chart <- ggplot(local_probabilities, aes(probability, reorder(station, probability), colour = scenario)) +
    geom_line(aes(group = station), colour = "grey75") + geom_point(size = 3) +
    scale_colour_brewer(palette = "Dark2") + scale_x_continuous(labels = scales::label_percent()) +
    labs(x = "Modelled station-choice probability", y = NULL, colour = NULL,
         title = paste("How station choices change in", str_to_title(details$local_origin$subzone)),
         subtitle = "Selected alternatives; unopened stations have zero probability in the baseline")

# ---- th03-local-housing-map ----
  local_blocks <- prepared$buildings %>% filter(origin_id == details$top_access$origin_id) %>%
    left_join(prepared$access %>% filter(scenario == "Both") %>% select(address_key, gain_m), by = "address_key")
  local_stations <- stations$Both %>% filter(station %in% details$local_housing$nearest_station) %>%
    mutate(status = if_else(existing, "Existing", "New"))
  extent <- st_bbox(st_buffer(st_union(c(st_geometry(local_blocks), st_geometry(local_stations))), 500))
  local_housing_map <- ggplot() +
    geom_sf(data = subzones, fill = "grey96", colour = "grey80", linewidth = 0.3) +
    geom_sf(data = local_blocks, aes(colour = gain_m), size = 2) +
    geom_sf(data = local_stations, aes(shape = status), size = 3, colour = "grey20") +
    geom_sf_text(data = local_stations, aes(label = station), nudge_y = 150, size = 3.2) +
    scale_colour_viridis_c(option = "C", breaks = scales::breaks_pretty(n = 3),
                          labels = scales::label_number(suffix = " m", big.mark = ","),
                          guide = guide_colourbar(barwidth = grid::unit(6, "cm"))) +
    coord_sf(xlim = extent[c("xmin", "xmax")], ylim = extent[c("ymin", "ymax")], datum = NA) +
    labs(x = NULL, y = NULL, colour = "Distance saved", shape = "Station",
         title = paste("Closer stations for", str_to_title(details$top_access$subzone),
                       paste0("(", str_to_title(details$top_access$planning_area), ")")),
         subtitle = "Matched residential blocks and their nearest stations before and after expansion")
# ---- th03-export-ridership ----
  save_plot(observed_map, "observed-map")
  save_plot(ranking_chart, "station-ranking")
  save_plot(validation_chart, "validation")
  save_plot(error_map, "errors")
  save_plot(new_chart, "new-stations", height = 9)
  top_ten <- ridership$predictions %>% filter(scenario == "Both", !existing) %>%
    slice_max(predicted, n = 10, with_ties = FALSE) %>% pull(station)
  save_plot(new_chart + filter(new_chart$data, station %in% top_ten) +
              labs(title = NULL, subtitle = NULL) + theme(text = element_text(size = 17)),
            "new-stations-summary", width = 7, height = 5)
  save_plot(changes_chart, "station-changes")
  save_plot(catchment_map, "catchment-changes")
  save_plot(local_choice_chart, "local-choices")
  save_plot(local_housing_map, "local-housing")
  save_plot(observed_map + labs(title = NULL, subtitle = NULL) + theme(text = element_text(size = 17)),
            "observed-summary", width = 7, height = 5)
  save_plot(validation_chart + labs(title = NULL, subtitle = NULL) + theme(text = element_text(size = 17)),
            "validation-summary", width = 7, height = 5)
  save_plot(changes_chart + labs(title = NULL, subtitle = NULL) + theme(text = element_text(size = 17)),
            "station-changes-summary", width = 7, height = 5)
  save_plot(local_choice_chart + labs(title = NULL, subtitle = NULL) + theme(text = element_text(size = 17)),
            "local-choices-summary", width = 7, height = 5)
}
