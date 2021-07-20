library(tidyverse)
library(lubridate)
library(sysfonts)
library(showtext)
library(ggtext)
library(scico)

drought <- tidytuesdayR::tt_load("2021-07-20")$drought

# the percentages count up from D4 to D1
# to clean, subtract the cumulative total from each level
drought_level <- drought %>%
  filter(!drought_lvl %in% c("None", "D0")) %>%
  group_by(state_abb, valid_end) %>%
  arrange(state_abb, valid_end, drought_lvl) %>%
  mutate(
    area_pct = ifelse(!is.na(lead(area_pct)), area_pct - lead(area_pct), area_pct),
    pop_pct  = ifelse(!is.na(lead(pop_pct)), pop_pct - lead(pop_pct), pop_pct)
  ) %>%
  ungroup() %>%
  select(state_abb, valid_end, drought_lvl, area_pct, pop_pct)

# calculate the % of population living under drought, focusing on western US after 2014
# there are 20 weeks where the total is 101%
# assuming this is error due to decimal precision, we'll push these down to 100%
w_us <- drought_level %>%
  filter(
    year(valid_end) > 2014,
    state_abb %in% c("CA", "CO", "ID", "NV", "WA", "OR", "MT", "UT", "WY")
  ) %>%
  group_by(state_abb, valid_end) %>%
  summarise(has_drought = sum(pop_pct) / 100) %>%
  ungroup() %>%
  mutate(has_drought = ifelse(has_drought > 1, 1, has_drought))

sysfonts::font_add_google("Raleway", "Raleway")
sysfonts::font_add_google("Roboto", "Roboto")
showtext::showtext_auto()

plot_theme <- theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(family = "Raleway"),
    plot.subtitle = element_text(family = "Raleway", face = "italic"),
    plot.caption = element_markdown(family = "Roboto"),
    plot.background = element_rect(fill = "#d1d1d1"),
    strip.text = element_blank(),
    axis.text = element_text(family = "Raleway", face = "bold"),
    axis.ticks.x = element_line(color = "#a8a8a8", lineend = "round"),
    panel.grid = element_blank(),
    legend.position = "top"
  )

ggplot(w_us) +
  geom_tile(aes(x = valid_end, y = state_abb, fill = has_drought)) +
  facet_wrap(~state_abb, ncol = 1, scales = "free_y") +
  scale_fill_scico(
    name = "",
    palette = "tokyo",
    labels = scales::percent,
    breaks = c(0, 0.25, 0.5, 0.75, 1)
  ) +
  scale_x_date(
    name = "",
    date_labels = "%b. %d %y",
    # date_labels = "%b. '%y",
    date_breaks = "12 months",
    limits = c(min(w_us$valid_end), max(w_us$valid_end))
  ) +
  scale_y_discrete(name = "") +
  guides(fill = guide_colorbar(barwidth = 15, barheight = 0.5)) +
  labs(
    title = "Percent of Western US populations living in areas affected by drought.",
    subtitle = "Weekly estimates from Jan. '15 to Jul. '21. Percentages reflect the share of the state's\npopulation experiencing a drought with severity from \"Moderate\" to \"Exceptional\".",
    caption = "**Source:** U.S. Drought Monitor **Plot:** @mooreaw_"
  ) +
  plot_theme

# Alt-text: This plot is a set of 9 heatmaps, dedicated to Western US
# states: California, Colorado, Idaho, Montana, Nevada, Oregon, Utah, Washington,
# and Wyoming. The timelines show weekly estimates from January 2015 to July 2021.
# Percentages are calculated for each week to indicate the share of people living
# in an area experiencing drought conditions between "Moderate" and "Exceptional".
# In Nevada and Utah, a cyclical pattern is observable, with over 80% of the
# population experiencing drought conditions roughly every 3 years, usually for
# at least 6 months at a time. Aside from Wyoming and Colorado, between 50%-90% of
# each state's population was experiencing some degree of drought during the entire
# summer 2015 and mid-2016. Notably, upwards of 70% residents in Utah, Nevada,
# Wyoming, and Oregon have experienced drought conditions for approximately 9 of
# the past 12 months. Currently, 5 of the 9 states have over 95% of their residents
# living in drought conditions.

ggsave("20210720-us-drought-monitor-heatmaps.png", width = 9, height = 8)
