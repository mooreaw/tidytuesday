library(tidyverse)
library(ggtext)
library(scico)

# original values from the graphic, with my personal ordering of each symptom
orig <- tribble(
  ~odr, ~symptoms, ~cold, ~flu, ~allergy, ~covid,
  5, "Fever",   "r", "c", "n", "c",
  5, "Headache", "u", "c", "u", "c",
  10, "General Aches, Pains", "sl", "c", "n", "c",
  6, "Fatigue, Weakness", "s", "c", "s", "c",
  7, "Extreme Exhaustion", "n", "c", "n", "c",
  3, "Stuffy, Runny Nose", "c", "s", "c", "r",
  2, "Sore Throat", "c", "s", "s", "c",
  4, "Cough", "c", "c", "s", "c",
  8, "Chest Discomfort", "m", "c", "r", "c",
  1, "Loss of Taste, Smell", "r", "r", "r", "c"
)

ill_levs <- c("COVID-19", "Flu", "Cold", "Allergy")

# tidy the data, apply levels to each category
d <- orig %>%
  pivot_longer(cold:covid, names_to = "illness", values_to = "freq") %>%
  mutate(
    symptoms = fct_reorder(symptoms, odr),
    freq2 = freq %>%
      recode(sl = "u", m = "u") %>%
      factor(
        levels = c("n", "r", "u", "s", "c"),
        labels = c("Never", "Rare", "Uncommon", "Sometimes", "Common")
      ),
    txt = case_when(
      freq == "sl" ~ "Slight",
      symptoms == "General Aches, Pains" & illness == "flu" ~ "Common<br>(often severe)",
      symptoms == "Fatigue, Weakness" & illness == "flu" ~ "Common<br>(can last weeks)",
      symptoms == "Fatigue, Weakness" & illness == "flu" ~ "Common<br>(at beginning)",
      symptoms == "Chest Discomfort" & illness == "allergy" ~ "Rare<sup>**</sup>",
      symptoms == "Chest Discomfort" & illness == "covid" ~ "Common<sup>*</sup>",
      symptoms == "Chest Discomfort" & illness == "cold" ~ "Mild to<br>Moderate",
      symptoms == "Cough" & illness == "covid" ~ "Common<br>(dry)",
      TRUE ~ as.character(freq2)
    ),
    txt = ifelse(txt == "Never", str_glue("<b style='color:black;'>{txt}</b>"), txt),
    illness = illness %>%
      factor(
        levels = c("covid", "flu", "cold", "allergy"),
        labels = str_glue("<i style = 'font-size: 24px; text-decoration: underline;'>{ill_levs}</i>")
      ),
  )

sysfonts::font_add_google("Open Sans")
showtext::showtext_auto()

plot_theme <- theme_minimal(base_size = 15) +
  theme(
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "#ecf2e9"),
    plot.title = element_markdown(family = "Open Sans", padding = margin(0, 0, -10, 0)),
    plot.title.position = "plot",
    plot.caption = element_markdown(family = "Open Sans"),

    axis.text.x.top = element_markdown(face = "bold", family = "Open Sans", padding = margin(0, 0, -10, 0)),
    axis.text.y = element_markdown(face = "bold", family = "Open Sans", padding = margin(0, -10, 0, 0)),
    axis.title.y = element_text(angle = 0, hjust = -10)
  )

p <- ggplot(d, aes(x = illness, y = fct_rev(symptoms))) +
  geom_tile(aes(fill = freq2), color = "#ecf2e9", size = 2.5) +
  geom_richtext(aes(label = txt), fill = NA, label.color = NA, color = "white", family = "Open Sans", size = 4.1, fontface = "bold") +
  scale_fill_scico_d(name = "", palette = "berlin", guide = "none") +
  scale_x_discrete(position = "top") +
  labs(
    x = "",
    y = "",
    title = "<span style = 'font-size: 34px; color: #286886;'>**Is it COVID-19, Flu, a Cold, or Allergies?**</span>",
    caption = "<b>\\*</b> can cause trouble preathing or persistent pain or pressure<br>in the chest that calls for immediate emergency care<br><b>\\*\\*</b> except with asthma<br>**Source:** National Institutes of Health **Graphic:** @ndrewwm"
  ) +
  plot_theme

p

ggsave(filename = "symptoms.png", plot = p, height = 9, width = 8)

# Alt-text: A grid of symptoms and 4 illnesses: COVID-19, Flu, Cold, and Allergy,
# titled: "Is it COVID-19, Flu, a Cold, or Allergies?". Inside each cell of the
# grid, the cells are labeled "Never" to "Common". Example symptoms listed include
# "fever", "headache", "sore throat", and "loss of taste, smell". Symptoms between
# the flu and COVID-19 tend to be similar, however loss of taste and smell is common
# for COVID-19 and rare for the other 3. A stuffy and runny-nose is rare for COVID-19.
# The grid is color-coded to emphasize patterns across the columns.
