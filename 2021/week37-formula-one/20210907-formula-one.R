library(tidyverse)

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

ggplot(ps_summary, aes(x = index, y = dur_sd)) +
  geom_point() +
  scale_y_log10() +
  facet_wrap(~driver_id)
