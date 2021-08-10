library(tidyverse)
library(ggtext)
library(scales)
library(scico)

tt <- tidytuesdayR::tt_load(2021, week = 33)
hc <- filter(tt$chain_investment, meta_cat == "Health", !str_detect(category, "S&L"))

sysfonts::font_add_google("Roboto", "Roboto")
showtext::showtext_auto()

pal <- scico(4, palette = "broc")

plot_theme <- theme_minimal(base_size = 18) +
  theme(
    axis.text = element_text(family = "Roboto", face = "bold"),
    plot.background = element_rect(fill = "#ffffff"),
    plot.title = element_text(family = "Roboto"),
    plot.subtitle = element_text(family = "Roboto"),
    plot.caption = element_markdown(family = "Roboto")
  )

ggplot(hc) +
  geom_line(aes(x = year, y = gross_inv_chain, color = category), size = 1.1) +
  annotate(geom = "text", x = 2015, y = 6000, size = 5.4, label = "All Federal", color = pal[1], family = "Roboto") +
  annotate(geom = "text", x = 2003, y = 73000, size = 5.4, label = "Private Equipment", color = pal[2], family = "Roboto") +
  annotate(geom = "text", x = 2009, y = 29000, size = 5.4, label = "Private Hospitals", color = pal[3], family = "Roboto") +
  annotate(geom = "text", x = 2010, y = 16000, size = 5.4, label = "Other Health Structures", color = pal[4], family = "Roboto") +
  scale_color_scico_d(palette = "broc", guide = "none") +
  scale_x_continuous(name = "", breaks = seq(1950, 2010, 10)) +
  scale_y_continuous(name = "", breaks = seq(0, 10, 2) * 10000, labels = ~dollar(., suffix = "B", scale = 1 / 10000)) +
  labs(
    title = "Here's a title about how expensive health-care is",
    subtitle = "It's a lot of money-- see how much it's grown in the past decade?",
    caption = "**Source:** BEA **Plot:** @ndrewwm"
  ) +
  plot_theme

# Alt-text: ...
# ...

ggsave("20210810-bea-health-care-investment.png", width = 11, height = 8)
