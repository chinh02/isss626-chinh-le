suppressPackageStartupMessages({library(sf); library(tidyverse); library(ggrepel)})
analysis <- readRDS("data/take-home-03/derived/analysis.rds")
review <- readRDS("data/take-home-03/derived/review.rds")
prepared <- analysis$prepared
ridership <- analysis$ridership
theme_set(theme_minimal(base_size = 12) + theme(panel.grid = element_blank(),
  legend.position = "bottom", plot.title.position = "plot"))

# ---- th03-pioneer-surface ----
probabilities <- map_dfr(c("Existing", "Both"), function(network) {
  stations <- prepared$stations[[network]]
  tibble(origin_id = prepared$origins$origin_id, scenario = network,
    probability = ridership$probabilities[[network]][, match("PIONEER", stations$station_id)])
})
active <- analysis$details$catchments %>% filter(scenario == "Both") %>% select(origin_id, active)
pioneer <- prepared$stations$Existing %>% filter(station_id == "PIONEER")
areas <- prepared$subzones[st_intersects(prepared$subzones, st_buffer(pioneer, 5000), sparse = FALSE)[, 1], ] %>%
  left_join(probabilities, by = "origin_id", relationship = "many-to-many") %>%
  left_join(active, by = "origin_id") %>%
  mutate(probability = if_else(active, probability, NA_real_),
    scenario = factor(scenario, c("Existing", "Both"), c("Existing network", "With both lines")))
points <- map_dfr(c("Existing", "Both"), function(network)
  prepared$stations[[network]] %>% filter(station_id %in% c("PIONEER", "BOONLAY", "GEKPOH", "TAWAS")) %>%
    mutate(scenario = factor(network, c("Existing", "Both"), c("Existing network", "With both lines"))))
extent <- st_bbox(st_buffer(pioneer, 4500))
pioneer_map <- ggplot(areas) +
  geom_sf(aes(fill = probability), colour = "white", linewidth = 0.2) +
  geom_sf(data = points, size = 2) +
  geom_label_repel(data = points, aes(label = station, geometry = geometry), stat = "sf_coordinates",
    size = 3, seed = 626, min.segment.length = 0) +
  scale_fill_distiller(palette = "Blues", direction = 1, limits = c(0, 1),
    breaks = c(0, 0.5, 1), labels = scales::label_percent(), na.value = "grey85",
    guide = guide_colourbar(barwidth = grid::unit(6, "cm"))) +
  facet_wrap(~scenario) +
  coord_sf(xlim = extent[c("xmin", "xmax")], ylim = extent[c("ymin", "ymax")], datum = NA) +
  labs(fill = "Pioneer choice probability", x = NULL, y = NULL,
    title = "How Pioneer's catchment changes",
    subtitle = "Demand-weighted subzone probabilities; grey areas have no modelled demand")

# ---- th03-fold-map ----
fold_map <- ggplot() +
  geom_sf(data = prepared$subzones, fill = "grey97", colour = "grey85", linewidth = 0.15) +
  geom_sf(data = review$stations, aes(colour = factor(fold)), size = 2) +
  scale_colour_brewer(palette = "Dark2") + coord_sf(datum = NA) +
  labs(colour = "Held-out group", x = NULL, y = NULL,
    title = "Five geographical groups for station validation")

# ---- th03-review-plot-export ----
ggsave("take-home-03/pioneer-catchment.png", pioneer_map, width = 10, height = 5.8, dpi = 160, bg = "white")
ggsave("take-home-03/validation-folds.png", fold_map, width = 10, height = 5.8, dpi = 160, bg = "white")
