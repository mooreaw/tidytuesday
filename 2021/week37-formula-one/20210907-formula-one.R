library(tidyverse)
library(tidytext)

tt <- tidytuesdayR::tt_load(2021, 37)

# races are nested in circuits
ps <- tt$pit_stops %>%
  janitor::clean_names() %>%
  arrange(race_id, driver_id, stop) %>%
  transmute(race_id, driver_id, stop, lap, time, duration = milliseconds / 1000)

ps <- tt$races %>%
  janitor::clean_names() %>%
  select(race_id, year, date, round, circuit_id, name) %>%
  right_join(ps, by = "race_id")

ps %>%
  ggplot(aes(x = duration, fill = factor(stop))) +
  geom_histogram(color = "white") +
  # scale_x_log10() +
  # scale_y_log10(breaks = seq(1000, )) +
  scale_fill_scico_d(palette = "tokyo")

# from the pit-stop data, count the number of races each driver has participated in
races_recorded <- ps %>%
  distinct(driver_id, race_id) %>%
  count(driver_id, sort = TRUE, name = "n_races")

set.seed(20210908)
drivers <- slice_sample(tt$drivers, n = 10)

ps_summary <- races_recorded %>%
  slice_head(n = 10) %>%
  inner_join(ps, by = "driver_id") %>%
  group_by(driver_id, race_id) %>%
  summarise(stops = n(), dur_avg = mean(duration), dur_sd = sd(duration)) %>%
  mutate(index = 1:n()) %>%
  ungroup()

ggplot(ps_summary, aes(x = index, y = dur_avg)) +
  geom_point() +
  scale_y_log10() +
  facet_wrap(~driver_id)

# run every year
gps <- tt$races %>%
  filter(between(year, 2019, 2020)) %>%
  group_by(name) %>%
  filter(identical(as.integer(sort(unique(year))), 2019:2020)) %>%
  ungroup() %>%
  select(race_id = raceId, name, year)

dat <- ps %>%
  semi_join(gps, by = "race_id") %>%
  filter(duration < 180) %>%
  mutate(
    name = str_remove(name, " Grand Prix")
  )

plot_theme <- theme_minimal(base_size = 14) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_line(color = "#e2e2e2", size = 0.69),
    plot.title.position = "plot",
    axis.text.y = element_text(face = "bold"),
    axis.line.y = element_line(color = "#e2e2e2", size = 0.69)
  )

ggplot(dat, aes(x = reorder_within(name, duration, year), y = duration, color = name)) +
  geom_boxplot(width = 0.2, position = position_nudge(x = -0.15), outlier.shape = NA) +
  geom_point(shape = 95, size = 6, position = position_nudge(x = 0.1)) +
  scale_x_reordered() +
  scale_y_continuous(breaks = c(0, 30, 45, 60), labels = c("0", "30", "45", "60\nseconds")) +
  scale_color_scico_d(palette = "berlin", guide = "none") +
  facet_wrap(~year, scales = "free_x") +
  labs(
    x = "",
    y = "",
    title = "Pit-Stop Durations, Formula One Grand Prix"
  ) +
  plot_theme

