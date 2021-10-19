library(tidyverse)
library(janitor)
library(ggtext)
library(here)

pumpkins <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-19/pumpkins.csv') %>%
  clean_names()


set.seed(1234)

pumpkins.clean <- pumpkins %>%
  filter(str_detect(id ,"2021")) %>%
  select(-id, -place, -city, -seed_mother, -pollinator_father, -ott, -est_weight, -pct_chart, -variety) %>%
  mutate(weight_lbs = parse_number(weight_lbs)) %>%
  mutate(weight_kg = (weight_lbs * 453.59237)/1000) %>%
  sample_n(nrow(.)) %>%
  rowid_to_column()

ggplot(pumpkins.clean, aes(rowid, weight_kg)) +
  geom_segment(aes(x = rowid, xend = rowid, y = 0, yend = weight_kg), size = 0.3) +
  scale_y_reverse(lim = c(1800,0)) +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())

         