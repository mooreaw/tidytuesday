library(tidyverse)
library(ggtext)
library(scales)
library(scico)

tt <- tidytuesdayR::tt_load(2021, week = 33)

# focusing on federal investment, private structures, and equipment
hc <- filter(tt$chain_investment, meta_cat == "Health", !str_detect(category, "S&L"))

# we'll add an overall line that captures all categories of investment
hc_overall <- tt$chain_investment %>%
  filter(meta_cat == "Health") %>%
  group_by(year) %>%
  summarise(gross_inv_chain = sum(gross_inv_chain))

sysfonts::font_add_google("Roboto", "Roboto")
showtext::showtext_auto()

pal <- scico(4, palette = "broc")

plot_theme <- theme_minimal(base_size = 18) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(linetype = "dashed", color = "darkgrey"),
    panel.grid.minor.y = element_blank(),
    axis.text = element_text(family = "Roboto", face = "bold"),
    axis.text.x = element_text(vjust = 5),
    plot.background = element_rect(fill = "#ffffff"),
    plot.title = element_text(family = "Roboto"),
    plot.subtitle = element_text(family = "Roboto", size = 14),
    plot.caption = element_markdown(family = "Roboto")
  )

my_annotate <- function(x, y, lab, col) {
  annotate(
    geom = "text", x = x, y = y, size = 5.5, label = lab,
    color = col, family = "Roboto", fontface = "bold"
  )
}

ggplot(hc) +
  geom_hline(yintercept = 500, color = "black", size = 1.01) +
  geom_line(data = hc_overall, aes(x = year, y = gross_inv_chain), size = 1.1, color = "#9E9E65") +
  geom_line(aes(x = year, y = gross_inv_chain, color = category), size = 1.1) +
  my_annotate(x = 2004, y = 123000, lab = "Health (overall)", col = "#9E9E65") +
  my_annotate(x = 2015, y = 6000, lab = "All Federal", col = pal[1]) +
  my_annotate(x = 2013.25, y = 57000, lab = "Private Equipment", col = pal[2]) +
  my_annotate(x = 2009, y = 30000, lab = "Private Hospitals", col = pal[3]) +
  my_annotate(x = 2011, y = 16000, lab = "Other Health Structures", col = pal[4]) +
  scale_color_scico_d(palette = "broc", guide = "none") +
  scale_x_continuous(name = "", breaks = seq(1950, 2010, 10)) +
  scale_y_continuous(name = "Millions of chained 2012 dollars", labels = dollar, breaks = c(500, 50000, 100000, 150000)) +
  labs(
    title = str_glue("Real investment in health infrastructure, {str_c(range(hc$year), collapse = '-')}"),
    subtitle = "Spending on equipment (e.g., electromedical machinery & instruments) has increased drastically\nsince 1995, partially due to corresponding declines in prices.",
    caption = "**Source:** *Bennet et al.* (2020), BEA **Plot:** @ndrewwm"
  ) +
  plot_theme

# Alt-text: A line plot tracking investment in health infrastructure, with 4
# different series: health overall, private equipment, private hospitals, other health
# structures, and all federal. The plot has the following title: "Real investment
# in health infrastructure, 1947-2017", and subtitle: "Spending on equipment
# (e.g., electromedical machinery & instruments) has increased drastically since
# 1995, partially due to corresponding declines in prices." The x-axis marks decades
# from 1950 to 2010, and the y-axis measures chained 2012 dollars in millions (ranging
# from $500M to $150,000M). The series show that overall investment has been increasing
# steadily, driven largely by spending on private equipment. Spending on private
# equipment overtook investment in structures (e.g. hospitals, clinics) in the early
# 90s; investments in structures have been increasing much more slowly. As of 2017,
# overall investment is above $150,000M, investment in private equipment is around
# $128,000M. Federal spending is consistently the smallest of the series at each
# time point, usually close to the $500M mark.

ggsave("20210810-bea-health-care-investment.png", width = 11, height = 8)
