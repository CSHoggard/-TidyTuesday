library(tidyverse)
library(janitor)
library(ggtext)
library(extrafont)
library(here)

pumpkins <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-19/pumpkins.csv') %>%
  clean_names()

set.seed(5678)

pumpkins.clean <- pumpkins %>%
  filter(str_detect(id ,"2021")) %>%
  filter(!str_detect(country, "exhibition")) %>%
  filter(!country %in% c("United States", "Canada", "New Zealand")) %>%
  select(-id, -place, -city, -seed_mother, -pollinator_father, -ott, -est_weight, -pct_chart, -variety) %>%
  mutate(weight_lbs = parse_number(weight_lbs)) %>%
  mutate(weight_kg = (weight_lbs * 453.59237)/1000) %>%
  sample_n(nrow(.)) %>%
  rowid_to_column()

pumpkins.top.three <- pumpkins.clean %>%
  top_n(., 3, weight_kg)


ggplot(pumpkins.clean, aes(rowid, weight_kg)) +
  geom_segment(aes(x = rowid, xend = rowid, y = 0, yend = weight_kg), colour = "#F75F1C", size = 2) +
  geom_segment(aes(x = rowid, xend = rowid, y = 0, yend = weight_kg), colour = "#FAF9F6", size = 0.5) +
  geom_point(data = pumpkins.top.three, aes(rowid, weight_kg, size = weight_kg), shape = 21, stroke = 1, fill = "#F75F1C", colour = "#FAF9F6") +
  scale_y_reverse(lim = c(1750,0)) +
  labs(y = "weight (kg)",
       caption = "@CSHoggard | #TidyTuesday Week 43 | Source: Great Pumpkin Commonwealth") +
  geom_text(x = 550, y = -1450,
            label = "Europe's Biggest Pumpkins!",
            family = "Commissioner",
            size = 10,
            color = "#FAF9F6") +
  theme_minimal() +
  coord_cartesian(clip = "off") +
  theme(text=element_text(family = "Commissioner"),
        plot.margin = margin(20,20,20,20),
        legend.position = "none",
        plot.caption = element_textbox_simple(colour = "#FAF9F6", family = "Commissioner", size = 10, halign = 1, margin = margin(20,0,0,0)),
        plot.background = element_rect(fill = "#F75F1C",  colour = "#FAF9F6", size = 2),
        panel.background = element_rect(fill = "#F75F1C", colour = "#F75F1C"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(colour = "#FAF9F6", margin = margin(0,20,0,0)),
        axis.text.x = element_blank(),
        axis.text.y = element_text(colour = "#FAF9F6")
        )

ggsave("images/Week_43_Pumpkins.png", plot = last_plot(), width = 200, height = 270, units = "mm")
