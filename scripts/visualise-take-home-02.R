if (.Platform$OS.type == "windows") invisible(Sys.setlocale("LC_CTYPE", "English_United States.utf8"))
# ---- th02-plot-style ----
# Read the analysis results and set the chart style.
library(sf)
library(tidyverse)
library(patchwork)
library(jsonlite)
library(RColorBrewer)

a <- readRDS("data/take-home-02/derived/analysis.rds")
dir.create("take-home-02/figures", recursive = TRUE, showWarnings = FALSE)
towns <- a$towns
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

# ---- th02-monthly-plot ----
monthly_plot <- ggplot(a$monthly, aes(month, events, colour = event_type)) +
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

count_figure <- count_map + count_bar + plot_layout(widths = c(1, 1.1))
ggsave("take-home-02/figures/distribution.png", count_figure,
       width = 10, height = 7, dpi = 170, bg = "white")

# ---- th02-local-plot ----
local_map <- towns %>%
  left_join(a$local, by = "TS_PCODE") %>%
  mutate(
    lisa = factor(lisa, levels = c("High-high", "High-low", "Not significant",
                                  "Low-high", "Low-low")),
    gi_cluster = factor(gi_cluster, levels = gi_levels)
  )

lisa_map <- ggplot(local_map) +
  geom_sf(aes(fill = lisa), colour = "white", linewidth = .07, show.legend = TRUE) +
  geom_sf(data = states, fill = NA, colour = "grey50", linewidth = .2) +
  scale_fill_brewer(palette = "RdBu", drop = FALSE, na.value = "grey70",
                    labels = ~replace_na(.x, "No Queen neighbour"), name = NULL) +
  guides(fill = guide_legend(ncol = 2, byrow = TRUE)) +
  coord_sf(datum = NA) +
  map_theme +
  labs(title = "Local Moran’s I", subtitle = "Clusters and outliers • permutation p < 0.05")

hotspot_map <- ggplot(local_map) +
  geom_sf(aes(fill = gi_cluster), colour = "white", linewidth = .07) +
  geom_sf(data = states, fill = NA, colour = "grey50", linewidth = .2) +
  scale_fill_brewer(palette = "RdBu", drop = FALSE, na.value = "grey70",
                    labels = ~replace_na(.x, "No Queen neighbour"), name = NULL) +
  guides(fill = guide_legend(ncol = 2, byrow = TRUE)) +
  coord_sf(datum = NA) +
  map_theme +
  labs(title = "Getis–Ord Gi*", subtitle = "Hotspots and coldspots • permutation p < 0.05")

ggsave("take-home-02/figures/local-association.png", lisa_map + hotspot_map,
       width = 10, height = 8, dpi = 170, bg = "white")

# ---- th02-ehsa-plot ----
ehsa_map <- towns %>%
  left_join(a$ehsa, by = "TS_PCODE") %>%
  mutate(trend = case_when(
    is.na(trend_p) ~ NA_character_,
    trend_p >= .05 ~ "Not significant",
    tau > 0 ~ "Increasing Gi*",
    TRUE ~ "Decreasing Gi*"
  )) %>%
  mutate(trend = factor(trend, levels = c("Increasing Gi*", "Not significant", "Decreasing Gi*")))

trend_map <- ggplot(ehsa_map) +
  geom_sf(aes(fill = trend), colour = "white", linewidth = .07) +
  geom_sf(data = states, fill = NA, colour = "grey50", linewidth = .2) +
  scale_fill_brewer(palette = "RdBu", drop = FALSE, na.value = "grey70",
                    labels = ~replace_na(.x, "No Queen neighbour"), name = NULL) +
  guides(fill = guide_legend(ncol = 2, byrow = TRUE)) +
  coord_sf(datum = NA) +
  map_theme +
  labs(title = "Trend in space-time Gi*", subtitle = "Mann–Kendall p < 0.05")

# Four coldspot classes use Blues; two hotspot classes use Reds.
ehsa_levels <- c("Intensifying coldspot", "Consecutive coldspot", "Oscillating coldspot",
                 "Sporadic coldspot", "Sporadic hotspot", "Oscillating hotspot",
                 "No pattern detected", "Trend not significant", "No Queen neighbour")
ehsa_map <- ehsa_map %>%
  mutate(map_class = recode(map_class, "No shared-boundary neighbour" = "No Queen neighbour"),
         map_class = factor(map_class, levels = ehsa_levels))

pattern_map <- ggplot(ehsa_map) +
  geom_sf(aes(fill = map_class), colour = "white", linewidth = .07) +
  geom_sf(data = states, fill = NA, colour = "grey50", linewidth = .2) +
  scale_fill_manual(
    values = c(rev(brewer.pal(5, "Blues")[2:5]), brewer.pal(3, "Reds")[2:3],
               "grey90", "grey90", "grey70"),
    drop = FALSE, labels = ~str_wrap(.x, 18), name = NULL
  ) +
  guides(fill = guide_legend(ncol = 2, byrow = TRUE)) +
  coord_sf(datum = NA) +
  map_theme +
  labs(title = "How the hotspots changed", subtitle = "EHSA classes shown where trend p < 0.05")

ggsave("take-home-02/figures/ehsa.png", trend_map + pattern_map,
       width = 10, height = 8.2, dpi = 170, bg = "white")

# ---- th02-civilian-plot ----
civil_map <- towns %>%
  left_join(a$civilian, by = "TS_PCODE") %>%
  mutate(gi_cluster = factor(gi_cluster, levels = gi_levels))

civilian_hotspot_map <- ggplot(civil_map) +
  geom_sf(aes(fill = gi_cluster), colour = "white", linewidth = .07) +
  geom_sf(data = states, fill = NA, colour = "grey50", linewidth = .2) +
  scale_fill_brewer(palette = "RdBu", drop = FALSE, na.value = "grey70",
                    labels = ~replace_na(.x, "No Queen neighbour"), name = NULL) +
  guides(fill = guide_legend(ncol = 2, byrow = TRUE)) +
  coord_sf(datum = NA) +
  map_theme +
  labs(title = "Events targeting civilians", subtitle = "Subset of the same records • permutation p < 0.05")

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
case_series <- a$gi_series %>%
  inner_join(select(candidates, TS_PCODE, TS, ST, classification), by = "TS_PCODE") %>%
  mutate(label = paste0(TS, " (", ST, ")\n", classification),
         status = case_when(p >= .05 ~ "Not significant", gi > 0 ~ "Hotspot", TRUE ~ "Coldspot"),
         status = factor(status, levels = gi_levels))
p1 <- ggplot(case_series, aes(month, events)) +
  geom_line(colour = "grey25", linewidth = .55) +
  facet_wrap(~label, ncol = 1, scales = "free_y") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(x = NULL, y = "Events / month", title = "Recorded events")
p2 <- ggplot(case_series, aes(month, gi)) +
  geom_hline(yintercept = 0, colour = "grey70") +
  geom_line(colour = "grey40", linewidth = .5) +
  geom_point(aes(colour = status), size = 1.5) +
  facet_wrap(~label, ncol = 1, scales = "free_y") +
  scale_colour_brewer(palette = "RdBu", drop = FALSE, name = NULL) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(x = NULL, y = "Space-time Gi*", title = "Neighbourhood concentration")
ggsave("take-home-02/figures/township-stories.png", p1 + p2,
       width = 10, height = 8, dpi = 170, bg = "white")
write_csv(candidates %>%
  select(TS_PCODE, TS, ST, events, civilian, tau, trend_p, block_p, classification),
          "data/take-home-02/derived/case-townships.csv")

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
  rows <- a$cube[a$cube$TS_PCODE == towns$TS_PCODE[i], ]
  list(id = towns$TS_PCODE[i], name = towns$TS[i], state = towns$ST[i],
       path = svg_path(st_geometry(simple)[[i]]), events = rows$events, civilian = rows$civilian)
})
payload <- list(months = format(sort(unique(a$cube$month)), "%b %Y"), towns = town_data,
                width = unname((bb[["xmax"]] - bb[["xmin"]]) / span * 620 + 24))
template <- readLines("take-home-02/explorer-template.html", warn = FALSE, encoding = "UTF-8")
template <- sub("__DATA__", toJSON(payload, auto_unbox = TRUE, digits = 3), template, fixed = TRUE)
writeLines(template, "take-home-02/conflict-explorer.html", useBytes = TRUE)
