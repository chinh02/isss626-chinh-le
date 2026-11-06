suppressPackageStartupMessages({library(sf); library(tidyverse)})
context <- readRDS("data/take-home-03/derived/context.rds")
analysis <- readRDS("data/take-home-03/derived/analysis.rds")
prepared <- analysis$prepared
theme_set(theme_minimal(base_size = 12) + theme(panel.grid.minor = element_blank(),
  legend.position = "bottom", plot.title.position = "plot", plot.title = element_text(face = "bold")))

# ---- th03-employment-comparison ----
activity <- context$origins %>% st_drop_geometry() %>% group_by(planning_area) %>%
  summarise(pois = sum(activity), .groups = "drop") %>% mutate(poi_share = pois / sum(pois))
employment_comparison <- context$mapped %>% mutate(worker_share = workers / sum(workers)) %>%
  left_join(activity, by = "planning_area")
shown <- union(head(arrange(employment_comparison, desc(workers))$planning_area, 8), "MARINE PARADE")
# ---- th03-employment-chart ----
employment_chart <- employment_comparison %>% filter(planning_area %in% shown) %>%
  mutate(planning_area = factor(str_to_title(planning_area), levels = str_to_title(planning_area[order(workers)]))) %>%
  pivot_longer(c(poi_share, worker_share), names_to = "measure", values_to = "share") %>%
  mutate(measure = recode(measure, poi_share = "Mapped activity POIs", worker_share = "Employed residents at workplaces")) %>%
  ggplot(aes(share, planning_area, fill = measure)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65) +
  scale_fill_brewer(palette = "Dark2") + scale_x_continuous(labels = scales::label_percent()) +
  labs(x = "Share of the respective mapped total", y = NULL, fill = NULL,
       title = "Facility counts and employment describe different distributions",
       subtitle = "2025 workplace counts for 44 named planning areas; 2026 OSM activity locations")
# ---- th03-employment-export ----
ggsave("take-home-03/employment-comparison.png", employment_chart, width = 10, height = 6, dpi = 160, bg = "white")

# ---- th03-marine-bus-map ----
routes <- readRDS("data/take-home-03/raw/BusRoutes-20261104.rds") %>%
  left_join(st_drop_geometry(context$stops) %>% select(BusStopCode, planning_area), by = "BusStopCode")
examples <- context$station_routes %>%
  filter(station_id == "MARINEPARADE", radius == 500, ServiceNo %in% c("36", "196"))
paths <- map_dfr(seq_len(nrow(examples)), function(i) {
  route <- routes %>% filter(ServiceNo == examples$ServiceNo[i], Direction == examples$Direction[i],
                             StopSequence >= examples$StopSequence[i]) %>% arrange(StopSequence)
  first_cbd <- min(route$StopSequence[which(route$planning_area == "DOWNTOWN CORE")])
  route %>% filter(StopSequence <= first_cbd)
}) %>% left_join(st_drop_geometry(context$stops) %>%
  mutate(x = st_coordinates(context$stops)[, 1], y = st_coordinates(context$stops)[, 2]) %>%
  select(BusStopCode, x, y), by = "BusStopCode")
rail_examples <- prepared$stations$Existing %>% filter(station_id %in% c("MARINEPARADE", "RAFFLESPLACE"))
cbd <- prepared$subzones %>% filter(planning_area == "DOWNTOWN CORE")
# ---- th03-bus-map-plot ----
bus_map <- ggplot() +
  geom_sf(data = prepared$subzones, fill = "grey97", colour = "grey85", linewidth = 0.2) +
  geom_sf(data = cbd, fill = "#f3e6c6", colour = "grey75", linewidth = 0.2) +
  geom_path(data = paths, aes(x, y, colour = ServiceNo, group = ServiceNo), linewidth = 1.1) +
  geom_point(data = paths, aes(x, y, colour = ServiceNo), size = 1.3) +
  geom_sf(data = rail_examples, shape = 23, size = 3, fill = "white") +
  geom_sf_text(data = rail_examples, aes(label = station), nudge_y = -300, size = 3.4) +
  scale_colour_brewer(palette = "Dark2") +
  coord_sf(xlim = range(paths$x) + c(-1000, 1000), ylim = range(c(paths$y, st_coordinates(rail_examples)[, 2])) + c(-800, 800), datum = NA) +
  labs(x = NULL, y = NULL, colour = "Bus service", title = "Two direct bus options from Marine Parade to Downtown Core",
       subtitle = "Lines join successive stops; shaded area is Downtown Core. Route snapshot: 4 November 2026.")
# ---- th03-bus-map-export ----
ggsave("take-home-03/marine-bus-routes.png", bus_map, width = 10, height = 5.5, dpi = 160, bg = "white")
saveRDS(employment_comparison, "data/take-home-03/derived/employment-comparison.rds")
print(employment_comparison %>% filter(planning_area %in% c("MARINE PARADE", "DOWNTOWN CORE")))
