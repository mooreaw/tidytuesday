library(tidyverse)
library(survival)
library(ggtext)
library(broom)
library(scico)

lemurs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-24/lemur_data.csv')

dat <- lemurs %>%
  filter(birth_type != "Unk", !is.na(dob)) %>%
  select(dlc_id, taxon, birth_type, hybrid, sex, dob, dob_estimated, dod) %>%
  distinct() %>%
  mutate(
    dod = if_else(is.na(dod), max(dod, na.rm = TRUE) + 1, dod),
    time = as.numeric(dod - dob),
    event = as.numeric(dod != max(dod, na.rm = TRUE))
  )

s <- survfit(Surv(time = time, event = event) ~ birth_type, data = dat)

final <- tidy(s) %>%
  mutate(
    time = time / 365.25,
    birth_type = strata %>%
      str_remove("birth_type=") %>%
      str_trim() %>%
      factor(levels = c("CB", "WB"), labels = c("Born in Captivity", "Wild Born"))
  )

pal <- c("#4F759B", "#571F4E")

plot_theme <- theme_minimal(base_size = 16) +
  theme(
    legend.position = "top",
    legend.justification = "left",
    legend.margin = margin(0),
    axis.title.x = element_markdown(),
    axis.title.y = element_markdown(),
    axis.text = element_text(face = "bold"),
    panel.grid = element_line(color = "#92C9B1", size = 0.6),
    panel.grid.minor.y = element_blank(),
    plot.title.position = "plot",
    plot.title = element_markdown(),
    plot.subtitle = element_text(size = 13),
    plot.caption = element_markdown(),
    plot.background = element_rect(fill = "#A2FAA3")
  )

ggplot(data = final, aes(x = time, y = estimate, group = birth_type)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = birth_type), alpha = 0.25) +
  geom_step(aes(x = time, y = estimate, color = birth_type)) +
  scale_x_continuous(breaks = seq(0, 55, 10)) +
  scale_color_manual(name = "", values = pal) +
  scale_fill_manual(name = "", values = pal, guide = "none") +
  guides(color = guide_legend(override.aes = list(size = 1.5))) +
  labs(
    x = "**Time** (Years from Birth)",
    y = "**KM-Survival** (Probability & 95% CI)",
    title = "**Lifespans for lemurs housed by the Duke Lemur Center (DLC)**",
    subtitle = "The estimated probability of a lemur born in captivity reaching age 10 is below 0.75, but this group is\nabout 3 times more likely to reach age 40.",
    caption = "**Source:** Zehr et al. (2014), doi: 10.1038/sdata.2014.19 **Plot:** @ndrewwm"
  ) +
  plot_theme

# Alt-text: ...

ggsave(filename = "20210824-dlc-lemurs.png", width = 8, height = 6)
