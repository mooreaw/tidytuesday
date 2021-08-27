library(tidyverse)
library(survival)
library(ggtext)
library(broom)
library(scico)
library(gt)

tt <- tidytuesdayR::tt_load(2021, week = 35)

dat <- tt$lemur_data %>%
  filter(birth_type != "Unk", !is.na(dob)) %>%
  select(dlc_id, birth_type, dob, dod) %>%
  distinct() %>%
  mutate(
    dod = if_else(is.na(dod), max(dod, na.rm = TRUE) + 1, dod),
    time = as.numeric(dod - dob),
    event = as.numeric(dod != max(dod, na.rm = TRUE))
  )

s <- survfit(Surv(time = time, event = event) ~ birth_type, data = dat)

# create life-tables ------------------------------------------------------

life_table <- s %>%
  summary(times = 365 * c(0, 5, 10, 15, 20, 25, 40), scale = 365.25) %>%
  {
    tibble(
      birth_type = .$strata %>%
        str_remove("birth_type=") %>%
        str_trim() %>%
        factor(levels = c("CB", "WB"), labels = c("Born in Captivity", "Wild Born")),
      time = round(.$time, 0),
      n_risk = .$n.risk,
      n_event =.$n.event,
      n_censor =.$n.censor,
      p_surv = .$surv,
      ci95_lower = .$lower,
      ci95_upper = .$upper
    )
  }

gt_life_table <- gt(life_table, groupname_col = "birth_type", rowname_col = "time") %>%
  cols_merge(columns = c("ci95_lower", "ci95_upper"), pattern = "[{1}, {2}]") %>%
  cols_align(align = "center", columns = "n_risk") %>%
  cols_label(
    n_risk = "N",
    n_event = "# Events",
    n_censor = "# Censored",
    p_surv = "Survival",
    ci95_lower = "95% CI"
  ) %>%
  fmt_number(columns = c("p_surv", "ci95_lower", "ci95_upper")) %>%
  fmt_number(columns = "n_risk", decimals = 0) %>%
  tab_style(
    style = cell_text(color = "black", weight = "bold"),
    locations = list(
      cells_row_groups(),
      cells_column_labels(everything())
    )
  ) %>%
  tab_stubhead("Age (Years)") %>%
  tab_header(
    title = "Life tables for Lemurs housed by the Duke Lemur Center",
    subtitle = "Kaplan-meier Survival probabilities for selected years."
  ) %>%
  tab_source_note(
    source_note = md("***#tidytuesday*** 2021, Week 35<br>**Source:** Zehr et al. (2014) | **Table:** @ndrewwm")
  ) %>%
  tab_options(
    heading.border.bottom.color = "black",
    heading.border.bottom.width = px(2),
    heading.align = "left",
    table_body.border.bottom.color = "black",
    stub.border.width = 0,
    row_group.border.top.width = px(3),
    row_group.border.top.color = "black",
    row_group.border.bottom.color = "black",
    table_body.hlines.color = "white",
    table.border.top.color = "white",
    table.border.top.width = px(3),
    table.border.bottom.color = "white",
    table.border.bottom.width = px(3),
    column_labels.border.bottom.color = "black",
    column_labels.border.bottom.width = px(2)
  )

gt_life_table

# Alt-text: Life tables for groups of wild-born vs. captive-born lemurs housed by the
# Duke Lemur Center. Kaplan-Meier probabilities for each group are shown at years
# 0-40 in 5-year increments. The life-table data can be found here: https://github.com/ndrewwm/tidytuesday/blob/main/2021/week35-dlc-lemurs/20210824-dlc-lemurs-lifetables.csv
# The source data can be found here: https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-08-24/readme.md

write_csv(life_table, "20210824-dlc-lemurs-lifetables.csv")

# plot curves -------------------------------------------------------------

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
    caption = "**#tidytuesday** 2021, Week 35 | **Source:** Zehr et al. (2014) | **Plot:** @ndrewwm"
  ) +
  plot_theme

ggsave(filename = "20210824-dlc-lemurs.png", width = 8, height = 6)
