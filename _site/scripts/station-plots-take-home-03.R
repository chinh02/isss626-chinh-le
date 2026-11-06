suppressPackageStartupMessages(library(tidyverse))
diagnostics <- readRDS("data/take-home-03/derived/station-diagnostics.rds")

# ---- th03-hourly-chart ----
hourly <- diagnostics$hourly %>% filter(month == "202609") %>%
  group_by(station, DAY_TYPE) %>%
  mutate(share = entries / sum(entries),
         day = if_else(DAY_TYPE == "WEEKDAY", "Weekday", "Weekend / holiday")) %>% ungroup()
hourly_chart <- ggplot(hourly, aes(TIME_PER_HOUR, share, colour = day)) +
  geom_line(linewidth = 1) + facet_wrap(~station, nrow = 1) +
  scale_colour_brewer(palette = "Dark2") +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
  scale_x_continuous(breaks = c(6, 9, 12, 15, 18, 21),
                     labels = c("6 am", "9 am", "Noon", "3 pm", "6 pm", "9 pm")) +
  coord_cartesian(xlim = c(5, 23)) +
  labs(x = NULL, y = "Share of that day's entries", colour = NULL,
       title = "Raffles Place has a much sharper weekday evening peak",
       subtitle = "September 2026; each curve sums to 100% across the full day") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom",
        plot.title.position = "plot", plot.title = element_text(face = "bold"))
ggsave("take-home-03/station-hours.png", hourly_chart, width = 10, height = 4.7, dpi = 160, bg = "white")
