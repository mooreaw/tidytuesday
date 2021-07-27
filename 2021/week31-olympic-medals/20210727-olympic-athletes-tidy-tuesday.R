library(tidyverse)
library(ggtext)
library(scico)

tt <- tidytuesdayR::tt_load("2021-07-27")

# to see how much age/weight/height vary, estimate SDs for each year by season & athlete sex
# go from wide -> tall, and clean up the text variables for plotting
ahw_sds <- tt$olympics %>%
  group_by(year, season, sex) %>%
  summarise(across(age:weight, .fns = ~sd(., na.rm = TRUE))) %>%
  pivot_longer(age:weight) %>%
  mutate(
    sex = factor(sex, levels = c("F", "M"), labels = c("Female", "Male")),
    name = factor(name, levels = c("age", "height", "weight"), labels = c("Age", "Height (cm)", "Weight (kg)"))
  )

sysfonts::font_add_google("Noto Sans JP", "Noto Sans JP")
sysfonts::font_add_google("Roboto", "Roboto")
showtext::showtext_auto()

plot_theme <- theme_minimal(base_size = 18) +
  theme(
    panel.spacing = unit(2, "lines"),  # ensure the axis text on the facets don't overlap
    panel.grid.minor.x = element_blank(),
    panel.grid.major = element_line(color = "#ffffff", linetype = "dotted"),
    panel.grid.minor = element_line(color = "#ffffff", linetype = "dotted"),
    legend.position = "top",
    legend.justification = "left",
    legend.text = element_text(family = "Noto Sans JP"),
    plot.background = element_rect(fill = "#7eceff"),
    plot.title = element_text(family = "Noto Sans JP"),
    plot.caption = element_markdown(family = "Roboto"),
    strip.text = element_text(family = "Noto Sans JP"),
    axis.text = element_text(family = "Noto Sans JP")
  )

ggplot(ahw_sds) +
  geom_line(aes(x = year, y = value, color = season), size = 1.1) +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "Standard Deviation") +
  scale_color_scico_d(name = "", palette = "tokyo", direction = -1) +
  facet_grid(sex ~ name) +
  labs(
    title = "Modern Olympic athletes tend to be of similar age, but vary in build.",
    subtitle = "Summer sports are more diverse in style, which seems reflected in higher variability.",
    caption = "**Source:** Kaggle.com **Plot:** @mooreaw_"
  ) +
  plot_theme

# Alt-text: A faceted line-plot, plotting the standard deviation (SD) of age
# height (cm), and weight (kg) for male & female Olympic athletes from 1896-2016.
# The plot is titled: "Modern Olympic athletes tend to be of similar age, but vary in build."
# Each of the 6 plot facets are dedicated to one variable/sex combination. Summer
# and winter games are tracked as separate lines within each panel, with winter games
# colored darker. For both male and female athletes, the SD for age peaked between
# 1920 and 1950, and has decreased to below 6 years as of 2016. The SDs for height and
# weight have been increasing in both male/female athletes competing in summer games
# since 1950. In winter games, the SD for height has remained relatively flat at 6cm, but
# the SD for weight has risen for both male/female athletes. Between 1960 to 2014, the SD
# for male athletes in the winter games has risen from 8kg to 12kg, compared to an increase
# from 6kg to 8kg in female athletes.

ggsave("20210727-kaggle-olympics.png", width = 11, height = 8)
