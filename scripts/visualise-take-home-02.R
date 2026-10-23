if (.Platform$OS.type == "windows") invisible(Sys.setlocale("LC_CTYPE", "English_United States.utf8"))
# ---- th02-plot-style ----
# Read the analysis results and set the chart style.
library(sf)
library(tidyverse)
library(patchwork)
library(jsonlite)
library(RColorBrewer)

results <- readRDS("data/take-home-02/derived/analysis.rds")
dir.create("take-home-02/figures", recursive = TRUE, showWarnings = FALSE)
towns <- results$towns
states <- towns %>%
  group_by(ST) %>%
  summarise(.groups = "drop")

theme_set(theme_minimal(base_size = 12))
theme_update(
  plot.title.position = "plot",
  plot.title = element_text(face = "bold"),
  panel.grid.minor = element_blank(),
  legend.position = "bottom"
)

# ---- th02-map-style ----
# Use the same layout for the statistical maps.
map_theme <- theme_void(base_size = 11) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 9),
    legend.key.size = grid::unit(.4, "cm"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 10),
    plot.margin = margin(10, 8, 10, 8)
  )

# Put the neutral category in the middle of the red-to-blue palette.
gi_levels <- c("Hotspot", "Not significant", "Coldspot")

# ---- th02-place-map ----
state_labels <- st_point_on_surface(states)
state_labels <- bind_cols(st_drop_geometry(state_labels), as_tibble(st_coordinates(state_labels)))
case_places <- towns %>% filter(TS %in% c("Monywa", "Kale", "Kawkareik", "Nawnghkio",
                                        "Kyaukpyu", "Maungdaw")) %>%
  st_point_on_surface()
case_labels <- bind_cols(st_drop_geometry(case_places), as_tibble(st_coordinates(case_places)))
state_map <- ggplot(states) +
  geom_sf(fill = "grey96", colour = "grey60", linewidth = .25) +
  ggrepel::geom_text_repel(data = state_labels, aes(X, Y, label = str_wrap(ST, 11)),
                          size = 3, seed = 62602, box.padding = .35,
                          min.segment.length = 0, segment.colour = "grey60") +
  coord_sf(datum = NA) + map_theme + labs(title = "States and regions")
case_map <- ggplot(states) +
  geom_sf(fill = "grey96", colour = "grey65", linewidth = .25) +
  geom_sf(data = case_places, colour = "firebrick", size = 2) +
  ggrepel::geom_label_repel(data = case_labels, aes(X, Y, label = TS), size = 3,
                           seed = 62602, nudge_x = 180000, min.segment.length = 0,
                           box.padding = .4, segment.colour = "grey40") +
  coord_sf(datum = NA) + map_theme + labs(title = "Townships discussed below")
ggsave("take-home-02/figures/study-area.png", state_map + case_map,
       width = 10, height = 7.8, dpi = 170, bg = "white")

# ---- th02-monthly-plot ----
monthly_plot <- ggplot(results$monthly, aes(month, events, colour = event_type)) +
  geom_line(linewidth = .8) +
  scale_colour_brewer(palette = "Dark2", name = NULL) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y",
               expand = expansion(mult = c(.01, .02))) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = NULL, y = "Recorded events per month",
       title = "How the mix of violence changed",
       subtitle = "Township-assigned records • January 2021–September 2025")
ggsave("take-home-02/figures/monthly.png", monthly_plot,
       width = 9, height = 5.5, dpi = 170, bg = "white")

# ---- th02-distribution-plot ----
count_map <- ggplot(towns) +
  geom_sf(aes(fill = events), colour = "white", linewidth = .06) +
  geom_sf(data = states, fill = NA, colour = "grey50", linewidth = .2) +
  scale_fill_distiller(palette = "YlOrRd", direction = 1, trans = "sqrt",
                       breaks = c(0, 100, 500, 1000), name = "Events") +
  coord_sf(datum = NA) +
  theme_void() +
  theme(legend.position = "bottom") +
  labs(title = "Recorded conflict by township")

# ---- th02-distribution-top ----
top <- towns %>%
  st_drop_geometry() %>%
  slice_max(events, n = 10) %>%
  mutate(label = paste0(TS, " · ", ST))

count_bar <- ggplot(top, aes(events, reorder(label, events))) +
  geom_col(fill = "steelblue", width = .65) +
  geom_text(aes(label = scales::comma(events)), hjust = -.15, size = 3.4) +
  scale_x_continuous(expand = expansion(mult = c(0, .2)), labels = scales::comma) +
  labs(x = "Recorded events", y = NULL, title = "Ten highest counts") +
  theme(panel.grid.major.y = element_blank(), axis.text.y = element_text(size = 10))

# ---- th02-distribution-save ----
count_figure <- count_map + count_bar + plot_layout(widths = c(1, 1.1))
ggsave("take-home-02/figures/distribution.png", count_figure,
       width = 10, height = 7, dpi = 170, bg = "white")

# ---- th02-local-plot ----
local_map <- towns %>%
  left_join(results$local, by = "TS_PCODE") %>%
  mutate(
    lisa = factor(lisa, levels = c("High-high", "High-low", "Not significant",
                                  "Low-high", "Low-low")),
    gi_cluster = factor(gi_cluster, levels = gi_levels)
  )

# ---- th02-lisa-map ----
lisa_map <- ggplot(local_map) +
  geom_sf(aes(fill = lisa), colour = "white", linewidth = .07, show.legend = TRUE) +
  geom_sf(data = states, fill = NA, colour = "grey50", linewidth = .2) +
  scale_fill_brewer(palette = "RdBu", drop = FALSE, na.value = "grey70",
                    labels = ~replace_na(.x, "No Queen neighbour"), name = NULL) +
  guides(fill = guide_legend(ncol = 2, byrow = TRUE)) +
  coord_sf(datum = NA) +
  map_theme +
  labs(title = "Local Moran’s I", subtitle = "Clusters and outliers • permutation p < 0.05")

# ---- th02-hotspot-map ----
hotspot_map <- ggplot(local_map) +
  geom_sf(aes(fill = gi_cluster), colour = "white", linewidth = .07) +
  geom_sf(data = states, fill = NA, colour = "grey50", linewidth = .2) +
  scale_fill_brewer(palette = "RdBu", drop = FALSE, na.value = "grey70",
                    labels = ~replace_na(.x, "No Queen neighbour"), name = NULL) +
  guides(fill = guide_legend(ncol = 2, byrow = TRUE)) +
  coord_sf(datum = NA) +
  map_theme +
  labs(title = "Getis–Ord Gi*", subtitle = "Hotspots and coldspots • permutation p < 0.05")

# ---- th02-local-save ----
ggsave("take-home-02/figures/local-association.png", lisa_map + hotspot_map,
       width = 10, height = 8, dpi = 170, bg = "white")

# ---- th02-ehsa-plot ----
ehsa_map <- towns %>%
  left_join(results$ehsa, by = "TS_PCODE") %>%
  mutate(trend = case_when(
    is.na(trend_p) ~ NA_character_,
    trend_p >= .05 ~ "Not significant",
    tau > 0 ~ "Increasing Gi*",
    TRUE ~ "Decreasing Gi*"
  )) %>%
  mutate(trend = factor(trend, levels = c("Increasing Gi*", "Not significant", "Decreasing Gi*")))

# ---- th02-trend-map ----
trend_map <- ggplot(ehsa_map) +
  geom_sf(aes(fill = trend), colour = "white", linewidth = .07) +
  geom_sf(data = states, fill = NA, colour = "grey50", linewidth = .2) +
  scale_fill_brewer(palette = "RdBu", drop = FALSE, na.value = "grey70",
                    labels = ~str_wrap(replace_na(.x, "No Queen neighbour"), 16), name = NULL) +
  guides(fill = guide_legend(ncol = 2, byrow = TRUE)) +
  coord_sf(datum = NA) +
  map_theme +
  labs(title = "Trend in space-time Gi*", subtitle = "Mann–Kendall p < 0.05")

# ---- th02-ehsa-labels ----
# Use matching colours in the overall and civilian maps.
present <- union(results$ehsa$map_class, results$civilian_time$ehsa$map_class)
cold_levels <- sort(present[grepl("coldspot$", present)])
hot_levels <- sort(present[grepl("hotspot$", present)])
ehsa_levels <- c(cold_levels, hot_levels, "No pattern detected",
                 "Trend not significant", "No Queen neighbour")
ehsa_colours <- setNames(c(
  rev(tail(brewer.pal(max(3, length(cold_levels) + 1), "Blues"), length(cold_levels))),
  tail(brewer.pal(max(3, length(hot_levels) + 1), "Reds"), length(hot_levels)),
  "grey65", "grey94", "grey30"), ehsa_levels)
ehsa_map <- ehsa_map %>%
  mutate(map_class = recode(map_class, "No shared-boundary neighbour" = "No Queen neighbour"),
         map_class = factor(map_class, levels = ehsa_levels))

# ---- th02-pattern-map ----
pattern_map <- ggplot(ehsa_map) +
  geom_sf(aes(fill = map_class), colour = "white", linewidth = .07) +
  geom_sf(data = states, fill = NA, colour = "grey50", linewidth = .2) +
  scale_fill_manual(
    values = ehsa_colours, drop = TRUE, labels = ~str_wrap(.x, 18), name = NULL
  ) +
  guides(fill = guide_legend(ncol = 2, byrow = TRUE)) +
  coord_sf(datum = NA) +
  map_theme +
  labs(title = "How the hotspots changed", subtitle = "EHSA classes shown where trend p < 0.05")

# ---- th02-ehsa-save ----
ggsave("take-home-02/figures/ehsa.png", trend_map + pattern_map,
       width = 10, height = 8.2, dpi = 170, bg = "white")

# ---- th02-civilian-time-map ----
civilian_time_map <- towns %>%
  left_join(results$civilian_time$ehsa, by = "TS_PCODE") %>%
  mutate(trend = case_when(is.na(trend_p) ~ NA_character_,
                           trend_p >= .05 ~ "Not significant",
                           tau > 0 ~ "Increasing Gi*", TRUE ~ "Decreasing Gi*"),
         trend = factor(trend, levels = levels(ehsa_map$trend)),
         map_class = recode(map_class, "No shared-boundary neighbour" = "No Queen neighbour"),
         map_class = factor(map_class, levels = ehsa_levels))
# ---- th02-civilian-time-plot ----
# Reuse the same scales and layout, replacing only the township results.
civilian_trend_map <- trend_map %+% civilian_time_map +
  labs(title = "Civilian-targeting trends")
civilian_pattern_map <- pattern_map %+% civilian_time_map +
  labs(title = "Civilian hotspot histories")
ggsave("take-home-02/figures/civilian-ehsa.png", civilian_trend_map + civilian_pattern_map,
       width = 10, height = 8.2, dpi = 170, bg = "white")
ggsave("take-home-02/figures/civilian-ehsa-patterns.png", civilian_pattern_map,
       width = 5.2, height = 7.3, dpi = 170, bg = "white")

# ---- th02-civilian-plot ----
civil_map <- towns %>%
  left_join(results$civilian, by = "TS_PCODE") %>%
  mutate(gi_cluster = factor(gi_cluster, levels = gi_levels))

# ---- th02-civilian-map ----
civilian_hotspot_map <- ggplot(civil_map) +
  geom_sf(aes(fill = gi_cluster), colour = "white", linewidth = .07) +
  geom_sf(data = states, fill = NA, colour = "grey50", linewidth = .2) +
  scale_fill_brewer(palette = "RdBu", drop = FALSE, na.value = "grey70",
                    labels = ~replace_na(.x, "No Queen neighbour"), name = NULL) +
  guides(fill = guide_legend(ncol = 2, byrow = TRUE)) +
  coord_sf(datum = NA) +
  map_theme +
  labs(title = "Events targeting civilians", subtitle = "Subset of the same records • permutation p < 0.05")

# ---- th02-civilian-save ----
overall_hotspot_map <- hotspot_map +
  labs(title = "All three conflict event types",
       subtitle = "Same study period, boundaries and neighbours")

ggsave("take-home-02/figures/civilian-hotspots.png", overall_hotspot_map + civilian_hotspot_map,
       width = 10, height = 8, dpi = 170, bg = "white")

# ---- th02-stories-plot ----
# Select the highest count in each observed significant hotspot class (up to
# two), alongside the highest-count township overall as a contrasting history.
candidates <- ehsa_map %>%
  st_drop_geometry() %>%
  filter(grepl("hotspot$", classification), trend_p < .05) %>%
  arrange(desc(events)) %>%
  distinct(classification, .keep_all = TRUE) %>%
  slice_head(n = 2)
largest_township <- ehsa_map %>%
  st_drop_geometry() %>%
  slice_max(events, n = 1, with_ties = FALSE)
candidates <- bind_rows(candidates, largest_township) %>%
  distinct(TS_PCODE, .keep_all = TRUE)
stopifnot(nrow(candidates) == 3)
# ---- th02-case-series ----
case_series <- results$gi_series %>%
  inner_join(select(candidates, TS_PCODE, TS, ST, classification), by = "TS_PCODE") %>%
  mutate(label = paste0(TS, " (", ST, ")\n", classification),
         status = case_when(p >= .05 ~ "Not significant", gi > 0 ~ "Hotspot", TRUE ~ "Coldspot"),
         status = factor(status, levels = gi_levels))
# ---- th02-case-count-plot ----
event_timeline <- ggplot(case_series, aes(month, events)) +
  geom_line(colour = "grey25", linewidth = .55) +
  facet_wrap(~label, ncol = 1, scales = "free_y") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(x = NULL, y = "Events / month", title = "Recorded events")
# ---- th02-case-gi-plot ----
gi_timeline <- ggplot(case_series, aes(month, gi)) +
  geom_hline(yintercept = 0, colour = "grey70") +
  geom_line(colour = "grey40", linewidth = .5) +
  geom_point(aes(colour = status), size = 1.5) +
  facet_wrap(~label, ncol = 1, scales = "free_y") +
  scale_colour_brewer(palette = "RdBu", drop = FALSE, name = NULL) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(x = NULL, y = "Space-time Gi*", title = "Neighbourhood concentration")
# ---- th02-stories-save ----
ggsave("take-home-02/figures/township-stories.png", event_timeline + gi_timeline,
       width = 10, height = 8, dpi = 170, bg = "white")
write_csv(candidates %>%
  select(TS_PCODE, TS, ST, events, civilian, tau, trend_p, block_p, classification),
          "data/take-home-02/derived/case-townships.csv")

# ---- th02-subtype-plot ----
subtype_comparison <- results$subtypes %>%
  select(sub_event_type, matches("^(events|civilian)_202[45]$")) %>%
  pivot_longer(-sub_event_type, names_to = c("measure", "year"),
               names_sep = "_", values_to = "count") %>%
  mutate(measure = factor(measure, levels = c("events", "civilian"),
                          labels = c("All retained events", "Events targeting civilians")),
         sub_event_type = factor(sub_event_type,
           levels = results$subtypes$sub_event_type[order(results$subtypes$events_2024 + results$subtypes$events_2025)]))
# ---- th02-subtype-labels ----
subtype_labels <- subtype_comparison %>%
  pivot_wider(names_from = year, values_from = count) %>%
  group_by(measure) %>%
  mutate(label_x = max(c(`2024`, `2025`)) * 1.08,
         label = paste0(scales::comma(`2024`), " → ", scales::comma(`2025`),
                        sprintf(" (%+d)", `2025` - `2024`))) %>%
  ungroup()
# ---- th02-subtype-chart ----
subtype_plot <- ggplot(subtype_labels, aes(y = sub_event_type)) +
  geom_segment(aes(x = `2024`, xend = `2025`, yend = sub_event_type),
               colour = "grey65", linewidth = .8) +
  geom_point(data = subtype_comparison, aes(x = count, colour = year), size = 2.4) +
  geom_text(aes(x = label_x, label = label), hjust = 0, size = 3.1) +
  geom_blank(aes(x = label_x * 1.65)) +
  facet_wrap(~measure, scales = "free_x", ncol = 1) +
  scale_y_discrete(labels = ~str_wrap(.x, 26)) +
  scale_colour_brewer(palette = "Dark2", name = NULL) +
  scale_x_continuous(labels = scales::comma, expand = expansion(mult = c(.02, .02))) +
  labs(x = "Recorded events", y = NULL, title = "Different forms of violence moved in different directions",
       subtitle = "January–September in both years · labels show 2024 → 2025 (change)") +
  theme(panel.grid.major.y = element_blank(), axis.text.y = element_text(size = 10),
        strip.text = element_text(face = "bold", size = 12))
# ---- th02-subtype-save ----
ggsave("take-home-02/figures/subtype-changes.png", subtype_plot,
       width = 10, height = 11, dpi = 170, bg = "white")

# ---- th02-subtype-map ----
subtype_map <- towns %>% select(TS_PCODE) %>%
  left_join(results$subtype_townships %>%
              filter(sub_event_type %in% c("Air/drone strike", "Abduction/forced disappearance")),
            by = "TS_PCODE", relationship = "one-to-many")
subtype_limit <- ceiling(max(abs(subtype_map$civilian_change)) / 10) * 10
subtype_map_labels <- subtype_map %>% group_by(sub_event_type) %>%
  slice_max(civilian_change, n = 2, with_ties = FALSE) %>% ungroup()
# ---- th02-subtype-map-plot ----
subtype_map_plot <- ggplot(subtype_map) +
  geom_sf(aes(fill = civilian_change), colour = "white", linewidth = .07) +
  geom_sf(data = states, fill = NA, colour = "grey50", linewidth = .2) +
  geom_sf_label(data = subtype_map_labels, aes(label = TS), size = 3,
                fill = "white", linewidth = .15, show.legend = FALSE) +
  facet_wrap(~sub_event_type, labeller = label_wrap_gen(width = 30)) +
  scale_fill_distiller(palette = "RdBu", direction = -1,
                       limits = c(-subtype_limit, subtype_limit),
                       breaks = c(-subtype_limit, 0, subtype_limit),
                       name = "Change in civilian-targeting events") +
  guides(fill = guide_colourbar(title.position = "top", barwidth = grid::unit(5, "cm"))) +
  coord_sf(datum = NA) + map_theme +
  labs(title = "Where civilian-targeting events increased",
       subtitle = "January–September 2025 minus January–September 2024 · descriptive counts")
# ---- th02-subtype-map-save ----
ggsave("take-home-02/figures/civilian-subtype-changes.png", subtype_map_plot,
       width = 10, height = 8, dpi = 170, bg = "white")

# ---- th02-recurrence-plot ----
recurrence_map <- towns %>% select(TS_PCODE) %>%
  left_join(results$recurrence, by = "TS_PCODE")
recurrence_map_plot <- ggplot(recurrence_map) +
  geom_sf(aes(fill = months_affected), colour = "white", linewidth = .06) +
  geom_sf(data = states, fill = NA, colour = "grey50", linewidth = .2) +
  scale_fill_distiller(palette = "YlOrRd", direction = 1, limits = c(0, 57),
                       breaks = c(0, 12, 24, 36, 48, 57), name = "Months with recorded civilian targeting") +
  guides(fill = guide_colourbar(title.position = "top", barwidth = grid::unit(4.5, "cm"))) +
  coord_sf(datum = NA) + map_theme +
  labs(title = "How often civilian targeting was recorded")

# ---- th02-recurrence-cases ----
recurrence_names <- c("Monywa", "Kale", "Kawkareik", "Nawnghkio", "Kyaukpyu",
                      "Pakokku", "Myaing", "Maungdaw")
recurrence_cases <- results$recurrence %>% filter(TS %in% recurrence_names) %>%
  mutate(TS = factor(TS, levels = recurrence_names)) %>% arrange(TS)
stopifnot(nrow(recurrence_cases) == length(recurrence_names))
# ---- th02-recurrence-series ----
recurrence_series <- results$cube %>%
  inner_join(recurrence_cases %>% select(TS_PCODE, TS, ST, months_affected), by = "TS_PCODE") %>%
  mutate(label = paste0(TS, " · ", ST, " (", months_affected, "/57)"),
         label = factor(label, levels = rev(unique(label[order(TS)]))))
# ---- th02-recurrence-heatmap ----
recurrence_heatmap <- ggplot(recurrence_series, aes(month, label)) +
  geom_tile(fill = "grey94", width = 28, height = .85) +
  geom_tile(data = filter(recurrence_series, civilian > 0), aes(fill = civilian),
            width = 28, height = .85) +
  scale_fill_distiller(palette = "YlOrRd", direction = 1, trans = "sqrt",
                       breaks = c(1, 5, 10, 20), name = "Events per month") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = expansion(mult = .01)) +
  labs(x = NULL, y = NULL, title = "Civilian targeting month by month",
       subtitle = "Grey = no recorded events; brackets give months affected out of 57") +
  theme(panel.grid = element_blank(), axis.text.y = element_text(size = 10))
# ---- th02-recurrence-save ----
recurrence_figure <- recurrence_map_plot + recurrence_heatmap +
  plot_layout(widths = c(1, 1.65)) +
  plot_annotation(subtitle = "January 2021–September 2025 · all 57 months are included")
ggsave("take-home-02/figures/civilian-recurrence.png", recurrence_figure,
       width = 13, height = 7.2, dpi = 170, bg = "white")
ggsave("take-home-02/figures/civilian-recurrence-months.png", recurrence_heatmap,
       width = 11, height = 5, dpi = 170, bg = "white")

# ---- th02-explorer-code ----
# A small monthly explorer: only township aggregates, no event coordinates or notes.
simple <- st_simplify(towns, dTolerance = 700, preserveTopology = TRUE)
bb <- st_bbox(simple); span <- bb[["ymax"]] - bb[["ymin"]]
svg_path <- function(g) {
  polygons <- if (inherits(g, "POLYGON")) list(unclass(g)) else unclass(g)
  paste(vapply(polygons, function(poly) paste(vapply(poly, function(ring) {
    xy <- cbind((ring[,1] - bb[["xmin"]]) / span * 620 + 12,
                (bb[["ymax"]] - ring[,2]) / span * 620 + 12)
    paste0("M", paste(apply(round(xy, 2), 1, paste, collapse = ","), collapse = "L"), "Z")
  }, character(1)), collapse = " "), character(1)), collapse = " ")
}
town_data <- lapply(seq_len(nrow(towns)), function(i) {
  rows <- results$cube[results$cube$TS_PCODE == towns$TS_PCODE[i], ]
  list(id = towns$TS_PCODE[i], name = towns$TS[i], state = towns$ST[i],
       path = svg_path(st_geometry(simple)[[i]]), events = rows$events, civilian = rows$civilian)
})
payload <- list(months = format(sort(unique(results$cube$month)), "%b %Y"), towns = town_data,
                width = unname((bb[["xmax"]] - bb[["xmin"]]) / span * 620 + 24))
template <- readLines("take-home-02/explorer-template.html", warn = FALSE, encoding = "UTF-8")
template <- sub("__DATA__", toJSON(payload, auto_unbox = TRUE, digits = 3), template, fixed = TRUE)
writeLines(template, "take-home-02/conflict-explorer.html", useBytes = TRUE)

# ---- th02-package-record ----
packages <- c("sf", "tidyverse", "spdep", "sfdep", "Kendall", "jsonlite", "digest",
              "patchwork", "RColorBrewer", "ggrepel", "knitr", "rmarkdown")
versions <- data.frame(package = packages,
                       version = vapply(packages, function(x) as.character(packageVersion(x)), character(1)))
writeLines(c(capture.output(sessionInfo()), "", "Direct dependencies:",
             capture.output(print(versions, row.names = FALSE))), "take-home-02/session-info.txt")
