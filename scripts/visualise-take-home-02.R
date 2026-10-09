# ---- th02-plot-style ----
# Read the verified analytical outputs and create report figures.
if (.Platform$OS.type == "windows") invisible(Sys.setlocale("LC_CTYPE", "English_United States.utf8"))
suppressPackageStartupMessages({library(sf); library(tidyverse); library(patchwork); library(jsonlite)})
a <- readRDS("data/take-home-02/derived/analysis.rds")
dir.create("take-home-02/figures", recursive = TRUE, showWarnings = FALSE)
towns <- a$towns
states <- towns %>% group_by(ST) %>% summarise(.groups = "drop")
ink <- "#17333d"; orange <- "#ba4937"; teal <- "#237b88"
theme_set(theme_minimal(base_size = 12))
theme_update(plot.title.position = "plot", plot.title = element_text(face = "bold", colour = ink),
             panel.grid.minor = element_blank(), legend.position = "bottom",
             plot.caption = element_text(hjust = 0, colour = "grey40", size = 9))
save_plot <- function(p, name, width = 9, height = 5.5) {
  ggsave(paste0("take-home-02/figures/", name, ".png"), p, width = width, height = height,
         dpi = 170, bg = "white", limitsize = FALSE)
}
# ---- th02-map-helper ----
palette <- c("High-high" = "#b32f34", "Low-low" = "#2875a2", "High-low" = "#e6a25c",
             "Low-high" = "#79b5cc", "Hotspot" = "#b32f34", "Coldspot" = "#2875a2",
             "Not significant" = "#e7e7e5", "No shared-boundary neighbour" = "#b6b4b0",
             "No pattern detected" = "#e7e7e5", "Trend not significant" = "#e7e7e5",
             "New hotspot" = "#ffb658", "Consecutive hotspot" = "#f16c47",
             "Intensifying hotspot" = "#800f25", "Persistent hotspot" = "#bb2737",
             "Diminishing hotspot" = "#d47d88", "Sporadic hotspot" = "#eaac96",
             "Oscillating hotspot" = "#c57221", "Historical hotspot" = "#dcc2b6",
             "New coldspot" = "#8ad5d8", "Consecutive coldspot" = "#4bacc3",
             "Intensifying coldspot" = "#123e69", "Persistent coldspot" = "#2875a2",
             "Diminishing coldspot" = "#80a6c6", "Sporadic coldspot" = "#bed9e8",
             "Oscillating coldspot" = "#657bad", "Historical coldspot" = "#cbcbdc")
map_plot <- function(d, column, title, subtitle = NULL) {
  ggplot(d) + geom_sf(aes(fill = .data[[column]]), colour = "white", linewidth = .07) +
    geom_sf(data = states, fill = NA, colour = "#596a6e", linewidth = .2) +
    scale_fill_manual(values = palette, na.value = "#e7e7e5", name = NULL,
                      labels = function(x) stringr::str_wrap(sub("No shared-boundary neighbour", "No Queen neighbour", x), 18)) +
    coord_sf(datum = NA) + theme_void(base_size = 11) +
    theme(legend.position = "bottom", plot.title = element_text(face = "bold", colour = ink),
          plot.subtitle = element_text(colour = "#52676e", size = 10),
          legend.text = element_text(size = 9), legend.key.size = grid::unit(.4, "cm"),
          plot.margin = margin(10, 8, 10, 8)) +
    guides(fill = guide_legend(ncol = 2, byrow = TRUE)) + labs(title = title, subtitle = subtitle)
}

# ---- th02-monthly-plot ----
type_palette <- c("Battles" = teal, "Explosions/Remote violence" = orange,
                  "Violence against civilians" = "#9c813b")
p <- ggplot(a$monthly, aes(month, events, colour = event_type)) + geom_line(linewidth = .8) +
  scale_colour_manual(values = type_palette, name = NULL) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = expansion(mult = c(.01,.02))) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = NULL, y = "Recorded events per month", title = "How the mix of violence changed",
       subtitle = "Township-assigned records • January 2021–September 2025")
save_plot(p, "monthly")

# ---- th02-distribution-plot ----
pmap <- ggplot(towns) + geom_sf(aes(fill = events), colour = "white", linewidth = .06) +
  geom_sf(data = states, fill = NA, colour = "#667779", linewidth = .2) +
  scale_fill_gradientn(colours = c("#faf5e9", "#edb07d", "#ca5941", "#7c1732"),
                       trans = "sqrt", breaks = c(0,100,500,1000,2000), name = "Events") +
  coord_sf(datum = NA) + theme_void() + theme(legend.position = "bottom") + labs(title = "Recorded conflict by township")
top <- towns %>% st_drop_geometry() %>% slice_max(events, n = 10) %>%
  mutate(label = paste0(TS, " · ", ST))
pbar <- ggplot(top, aes(events, reorder(label, events))) + geom_col(fill = orange, width = .65) +
  geom_text(aes(label = scales::comma(events)), hjust = -.15, size = 3.4) +
  scale_x_continuous(expand = expansion(mult = c(0,.2)), labels = scales::comma) +
  labs(x = "Recorded events", y = NULL, title = "Ten highest counts") +
  theme(panel.grid.major.y = element_blank(), axis.text.y = element_text(size = 10))
save_plot(pmap + pbar + plot_layout(widths = c(1,1.1)), "distribution", 10, 7)

# ---- th02-local-plot ----
local_map <- towns %>% left_join(a$local, by = "TS_PCODE")
save_plot(map_plot(local_map, "lisa", "Local Moran’s I", "Clusters and outliers • permutation p < 0.05") +
            map_plot(local_map, "gi_cluster", "Getis–Ord Gi*", "Hotspots and coldspots • permutation p < 0.05"),
          "local-association", 10, 8)

# ---- th02-ehsa-plot ----
ehsa_map <- towns %>% left_join(a$ehsa, by = "TS_PCODE")
trend_map <- ehsa_map %>% mutate(trend = case_when(
  is.na(trend_p) ~ "No shared-boundary neighbour", trend_p >= .05 ~ "Not significant",
  tau > 0 ~ "Increasing Gi*", TRUE ~ "Decreasing Gi*"))
palette <- c(palette, "Increasing Gi*" = "#b32f34", "Decreasing Gi*" = "#2875a2")
save_plot(map_plot(trend_map, "trend", "Trend in space-time Gi*", "Mann–Kendall p < 0.05") +
            map_plot(ehsa_map, "map_class", "How the hotspots changed", "EHSA classes shown where trend p < 0.05"),
          "ehsa", 10, 8.2)

# ---- th02-civilian-plot ----
civil_map <- towns %>% left_join(a$civilian, by = "TS_PCODE")
save_plot(map_plot(local_map, "gi_cluster", "All three conflict event types", "Same study period, boundaries and neighbours") +
            map_plot(civil_map, "gi_cluster", "Events targeting civilians", "Subset of the same records • permutation p < 0.05"),
          "civilian-hotspots", 10, 8)

# ---- th02-stories-plot ----
# Select the highest count in each observed significant hotspot class (up to
# two), alongside the highest-count township overall as a contrasting history.
candidates <- ehsa_map %>% st_drop_geometry() %>%
  filter(grepl("hotspot$", classification), trend_p < .05) %>%
  arrange(desc(events)) %>% distinct(classification, .keep_all = TRUE) %>% slice_head(n = 2)
candidates <- bind_rows(candidates, ehsa_map %>% st_drop_geometry() %>% slice_max(events, n = 1, with_ties = FALSE)) %>%
  distinct(TS_PCODE, .keep_all = TRUE)
stopifnot(nrow(candidates) == 3)
case_series <- a$gi_series %>% inner_join(candidates %>% select(TS_PCODE, TS, ST, classification), by = "TS_PCODE") %>%
  mutate(label = paste0(TS, " (", ST, ")\n", classification),
         status = case_when(p >= .05 ~ "Not significant", gi > 0 ~ "Hotspot", TRUE ~ "Coldspot"))
p1 <- ggplot(case_series, aes(month, events)) + geom_line(colour = ink, linewidth = .55) +
  facet_wrap(~label, ncol = 1, scales = "free_y") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") + labs(x = NULL, y = "Events / month", title = "Recorded events")
p2 <- ggplot(case_series, aes(month, gi)) + geom_hline(yintercept = 0, colour = "#b5b5b5") +
  geom_line(colour = "#647577", linewidth = .5) + geom_point(aes(colour = status), size = 1.5) +
  facet_wrap(~label, ncol = 1, scales = "free_y") + scale_colour_manual(values = palette, name = NULL) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") + labs(x = NULL, y = "Space-time Gi*", title = "Neighbourhood concentration")
save_plot(p1 + p2, "township-stories", 10, 8)
write_csv(candidates %>% select(TS_PCODE, TS, ST, events, civilian, tau, trend_p, block_p, classification),
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
