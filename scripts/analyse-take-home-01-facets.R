# Run after the report has prepared the 2022 Greater Bangkok records.
library(sf)
library(tidyverse)
library(spatstat.geom)
library(spatstat.explore)

derived_dir <- "data/take-home-01/derived"
figure_dir <- "take-home-01/figures"
events <- readRDS(file.path(derived_dir, "accidents_2022_bmr.rds"))
provinces <- readRDS(file.path(derived_dir, "bmr_provinces.rds"))
window <- readRDS(file.path(derived_dir, "bmr_window.rds"))
unlocated <- read_csv(file.path(derived_dir, "unlocated_region_2022.csv"),
                     show_col_types = FALSE)

# Zero means no casualty recorded, not independently confirmed absence of harm.
casualty_fields <- c("number_of_fatalities", "number_of_injuries")
for (field in casualty_fields) {
  stopifnot(!anyNA(events[[field]]), all(events[[field]] >= 0),
            all(events[[field]] == round(events[[field]])))
}
period_labels <- c("00:00-05:59", "06:00-11:59", "12:00-17:59", "18:00-23:59")
outcome_labels <- c("No injury or death recorded", "Injury, no death recorded", "At least one death")
events <- events %>% mutate(
  fatal_event = number_of_fatalities > 0,
  outcome = factor(case_when(fatal_event ~ outcome_labels[3],
    number_of_injuries > 0 ~ outcome_labels[2], TRUE ~ outcome_labels[1]),
    levels = outcome_labels),
  period = factor(period_labels[hour %/% 6 + 1L], levels = period_labels),
  night = hour >= 18 | hour < 6,
  motorcycle = vehicle_type == "motorcycle",
  quarter = quarter(incident_time), month = month(incident_time)
)
flat <- st_drop_geometry(events)

summarise_harm <- function(x) {
  x %>% summarise(reports = n(), fatal_reports = sum(fatal_event),
    deaths = sum(number_of_fatalities), injuries = sum(number_of_injuries),
    fatal_pct = 100 * mean(fatal_event), .groups = "drop")
}
totals <- summarise_harm(flat)
by_province <- flat %>% group_by(province_geom) %>% summarise_harm() %>%
  mutate(report_share = 100 * reports / sum(reports),
         fatal_share = 100 * fatal_reports / sum(fatal_reports)) %>%
  arrange(desc(fatal_reports))
by_period <- flat %>% group_by(period) %>% summarise_harm()
by_vehicle <- flat %>% group_by(vehicle_type) %>% summarise_harm()
by_agency_province <- flat %>% group_by(agency, province_geom) %>% summarise_harm()
by_agency_vehicle <- flat %>% group_by(agency, motorcycle) %>% summarise_harm()
by_night_vehicle <- flat %>% group_by(motorcycle, night) %>% summarise_harm()
quarter_days <- c(90, 91, 92, 92)
quarter_labels <- c("Jan-Mar", "Apr-Jun", "Jul-Sep", "Oct-Dec")
by_quarter <- flat %>% group_by(quarter) %>% summarise_harm() %>%
  mutate(days = quarter_days[quarter], reports_per_day = reports / days,
         fatal_per_day = fatal_reports / days)
quarter_outcomes <- flat %>% count(quarter, outcome, .drop = FALSE) %>%
  mutate(per_day = n / quarter_days[quarter],
         quarter_label = factor(quarter_labels[quarter], levels = quarter_labels))
quarter_agency <- flat %>% group_by(agency, quarter) %>% summarise_harm()
quarter_weather <- flat %>% count(quarter, weather_condition)
by_month <- flat %>% group_by(month) %>% summarise_harm()
month_province <- flat %>% group_by(month, province_geom) %>% summarise_harm()
unlocated_harm <- unlocated %>% summarise(reports = n(),
  fatal_reports = sum(number_of_fatalities > 0), deaths = sum(number_of_fatalities),
  injuries = sum(number_of_injuries))

ink <- "#17333d"
accent <- "#b94735"
theme_set(theme_minimal(base_size = 12))
theme_update(panel.grid.minor = element_blank(), plot.title.position = "plot",
             plot.caption = element_text(hjust = 0, colour = "grey35"))

period_plot_data <- bind_rows(
  by_period %>% transmute(period, view = "Number of reports", value = reports,
                         label = format(reports, big.mark = ",", trim = TRUE)),
  by_period %>% transmute(period, view = "Reports involving a death (%)",
                         value = fatal_pct, label = sprintf("%.1f%%", fatal_pct))
)
period_plot <- ggplot(period_plot_data, aes(period, value, fill = view)) +
  geom_col(width = 0.62, show.legend = FALSE) +
  geom_text(aes(label = label), vjust = -0.5, colour = ink, size = 3.8) +
  facet_wrap(~view, scales = "free_y", nrow = 1) +
  scale_fill_manual(values = c("Number of reports" = "#6b9299",
    "Reports involving a death (%)" = accent)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Fewer reports overnight, but a larger share involved a death",
       subtitle = "Four equal six-hour periods | Mapped reports, 2022",
       x = "Accident time (local)", y = NULL,
       caption = "Left: all 3,599 reports. Right: fatal reports / all reports in each time period.") +
  theme(axis.text.x = element_text(size = 9))
ggsave(file.path(figure_dir, "time-severity.png"), period_plot,
       width = 10, height = 4.8, dpi = 160)

# Keep source labels: a single recorded vehicle category is not a full vehicle roster.
vehicle_display <- by_vehicle %>% filter(reports >= 100, vehicle_type != "other") %>%
  mutate(vehicle = recode(vehicle_type,
    "private/passenger car" = "Passenger car", "4-wheel pickup truck" = "Pickup truck",
    "motorcycle" = "Motorcycle", "large truck with trailer" = "Truck with trailer",
    "6-wheel truck" = "6-wheel truck", "7-10-wheel truck" = "7-10-wheel truck"))
vehicle_plot <- ggplot(vehicle_display, aes(fatal_pct, reorder(vehicle, fatal_pct))) +
  geom_col(aes(fill = vehicle == "Motorcycle"), width = 0.55, show.legend = FALSE) +
  geom_text(aes(label = sprintf("%d / %s (%.1f%%)", fatal_reports,
    format(reports, big.mark = ",", trim = TRUE), fatal_pct)), hjust = -0.08,
    colour = ink, size = 3.5) +
  scale_fill_manual(values = c("FALSE" = "#6b9299", "TRUE" = accent)) +
  scale_x_continuous(limits = c(0, 28), breaks = seq(0, 20, 5)) +
  labs(title = "Motorcycle reports stand out when deaths are considered",
    subtitle = "Six most common named vehicle categories in the mapped records",
    x = "Reports involving a death (%)", y = NULL,
    caption = "Labels: fatal reports / reports with that vehicle label. Smaller categories and 'other' are omitted.")
ggsave(file.path(figure_dir, "vehicle-severity.png"), vehicle_plot,
       width = 9, height = 4.8, dpi = 160)

# Compare the distribution of each group after dividing by its own event count.
# These are shares per square kilometre, not fatality probability or traffic risk.
xy <- st_coordinates(events)
pattern <- ppp(xy[, 1], xy[, 2], window = as.owin(window), checkdup = FALSE)
groups <- list("All reports" = pattern, "Reports involving a death" = pattern[events$fatal_event])
severity_surfaces <- lapply(groups, function(p) {
  density(p, sigma = 2000, eps = 250, edge = TRUE, diggle = TRUE,
          kernel = "gaussian", positive = TRUE)
})
mass_checks <- map2_dbl(severity_surfaces, groups, ~integral.im(.x) / npoints(.y))
stopifnot(all(abs(mass_checks - 1) < 0.02))
severity_density <- imap_dfr(severity_surfaces, function(surface, label) {
  as.data.frame(surface) %>% transmute(x, y,
    share = value * 1e6 * 100 / npoints(groups[[label]]),
    group = factor(label, levels = names(groups)))
})
severity_peaks <- severity_density %>% group_by(group) %>%
  slice_max(share, n = 1, with_ties = FALSE) %>% ungroup() %>%
  st_as_sf(coords = c("x", "y"), crs = 32647) %>%
  st_join(provinces %>% select(province_geom)) %>% st_transform(4326)
province_labels <- st_point_on_surface(provinces) %>% mutate(
  label = str_replace(province_geom, " ", "\n"))
severity_plot <- ggplot(severity_density, aes(x, y, fill = share)) +
  geom_raster() +
  geom_sf(data = provinces, inherit.aes = FALSE, fill = NA,
          colour = "#697c80", linewidth = 0.25) +
  geom_sf_text(data = province_labels, aes(label = label), inherit.aes = FALSE,
               size = 2.7, colour = ink) +
  facet_wrap(~group, nrow = 1) +
  scale_fill_gradientn(colours = c("#fff8e8", "#f6d292", "#ed995c", "#cf513d", "#7e2340"),
                       name = "Share of each group / sq km (%)") +
  coord_sf(crs = st_crs(32647), datum = NA) +
  labs(title = "The concentration changes when I look only at fatal reports",
       subtitle = "3,599 reports on the left; 159 involving a death on the right",
       x = NULL, y = NULL,
       caption = "Each map is scaled to 100% of its own group. Same 2 km smoothing and colour scale; not risk per journey.") +
  theme(panel.grid.major = element_blank(), legend.position = "bottom")
ggsave(file.path(figure_dir, "severity-geography.png"), severity_plot,
       width = 10, height = 5.8, dpi = 180)

outcome_plot <- ggplot(quarter_outcomes, aes(quarter_label, per_day, fill = outcome)) +
  geom_col(width = 0.6, position = position_stack(reverse = TRUE)) +
  geom_text(aes(label = sprintf("%.2f", per_day)), size = 3.7,
    position = position_stack(vjust = 0.5, reverse = TRUE), colour = "white") +
  scale_fill_manual(values = c("#586e77", "#2a8583", accent)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.06))) +
  labs(title = "The late-year rise includes more reports of harm",
       subtitle = "Average reports per day, grouped by recorded outcome",
       x = NULL, y = "Reports per day", fill = NULL,
       caption = "A report involving both injuries and deaths is counted once, in the death category.") +
  theme(legend.position = "bottom", legend.text = element_text(size = 9))
ggsave(file.path(figure_dir, "quarter-outcomes.png"), outcome_plot,
       width = 9, height = 5.2, dpi = 160)

# Independent totals guard against lost records and overlapping outcome groups.
stopifnot(sum(by_province$reports) == nrow(events),
          sum(by_period$fatal_reports) == totals$fatal_reports,
          sum(by_vehicle$fatal_reports) == totals$fatal_reports,
          sum(quarter_outcomes$n) == nrow(events),
          sum(by_month$reports) == nrow(events),
          sum(month_province$fatal_reports) == totals$fatal_reports,
          totals$fatal_reports <= totals$deaths,
          sum(flat$outcome == outcome_labels[3]) == sum(flat$number_of_fatalities > 0))

facets <- list(totals = totals, by_province = by_province, by_period = by_period,
  by_vehicle = by_vehicle, by_agency_province = by_agency_province,
  by_agency_vehicle = by_agency_vehicle, by_night_vehicle = by_night_vehicle,
  by_quarter = by_quarter, quarter_outcomes = quarter_outcomes,
  quarter_agency = quarter_agency, quarter_weather = quarter_weather,
  by_month = by_month, month_province = month_province, unlocated_harm = unlocated_harm,
  density_mass_checks = mass_checks, severity_peaks = severity_peaks)
saveRDS(facets, file.path(derived_dir, "facets_analysis.rds"))
walk(names(facets)[vapply(facets, is.data.frame, logical(1))], function(name) {
  write_csv(st_drop_geometry(facets[[name]]), file.path(derived_dir, paste0("facets_", name, ".csv")))
})

# A small, self-contained browser map; raw records stay in the R workflow.
# Coordinates use the same UTM projection as the report. Only display boundaries are simplified.
bounds <- st_bbox(provinces)
map_width <- 760
map_height <- 440
map_scale <- min((map_width - 50) / (bounds["xmax"] - bounds["xmin"]),
                 (map_height - 40) / (bounds["ymax"] - bounds["ymin"]))
project_x <- function(x) round(25 + (x - bounds["xmin"]) * map_scale, 2)
project_y <- function(y) round(20 + (bounds["ymax"] - y) * map_scale, 2)
map_view_width <- unname(project_x(bounds["xmax"]) + 25)
map_view_height <- unname(project_y(bounds["ymin"]) + 20)
display_provinces <- st_simplify(provinces, dTolerance = 100, preserveTopology = TRUE)
paths <- map(seq_len(nrow(provinces)), function(i) {
  coords <- st_coordinates(display_provinces[i, ])
  rings <- interaction(as.data.frame(coords[, -(1:2), drop = FALSE]), drop = TRUE)
  path <- paste(vapply(split(seq_len(nrow(coords)), rings), function(idx) {
    paste0("M", paste(project_x(coords[idx, 1]), project_y(coords[idx, 2]),
                     sep = ",", collapse = "L"), "Z")
  }, character(1)), collapse = " ")
  centre <- st_coordinates(st_point_on_surface(provinces[i, ]))
  list(name = provinces$province_geom[i], path = path,
       x = unname(project_x(centre[1])), y = unname(project_y(centre[2])))
})
map_records <- flat %>% transmute(x = project_x(xy[, 1]), y = project_y(xy[, 2]),
  month, province = match(province_geom, provinces$province_geom),
  fatal = as.integer(fatal_event), deaths = number_of_fatalities,
  injuries = number_of_injuries)
payload <- jsonlite::toJSON(list(width = map_view_width, height = map_view_height,
  provinces = paths, records = map_records),
  dataframe = "rows", auto_unbox = TRUE, digits = 3)
template <- readLines("take-home-01/explorer-template.html", warn = FALSE, encoding = "UTF-8")
stopifnot(sum(grepl("__ACCIDENT_DATA__", template, fixed = TRUE)) == 1L)
template <- gsub("__ACCIDENT_DATA__", payload, template, fixed = TRUE)
writeLines(template, "take-home-01/accident-explorer.html", useBytes = TRUE)
print(by_province)
print(by_night_vehicle)
print(severity_peaks)
