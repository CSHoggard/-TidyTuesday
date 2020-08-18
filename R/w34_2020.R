library(tidyverse)
library(extrafont)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(viridis)
library(here)

plants <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-18/plants.csv')

plants.weighting <- plants %>%
  count(country, sort = T) %>%
  rename(count = n) %>%
  rename(name = country) %>%
  mutate(
    name = case_when(
      name == "Pitcairn" ~ "Pitcairn Is.",
      name == "Cook Islands" ~ "Cook Is.",
      name == "Cabo Verde" ~ "Cape Verde",
      name == "Viet Nam" ~ "Vietnam",
      name == "United States" ~ "United States of America",
      name == "Sao Tome and Principe" ~ "São Tomé and Principe",
      name == "Saint Helena, Ascension and Tristan da Cunha" ~ "Saint Helena",
      name == "French Polynesia" ~ "Fr. Polynesia",
      TRUE ~ name)
  )


world <- ne_countries(scale = "large", returnclass = "sf")
world <- left_join(world, plants.weighting, by = "name")

ggplot(world) + 
  geom_sf(color = NA, aes(fill = count)) +
  coord_sf(ylim = c(-50,80)) +
  scale_fill_viridis(option = "plasma", na.value = "grey80") +
  theme_void() +
  labs(title = "#TidyTuesday Week 34: Plants in Danger",
       subtitle = "Global Extinct Plant Species (1900-2020)",
       fill = "Species Count",
       caption = "Source: International Union for Conservation of Nature (IUCN) \n Data prepared by Florent Lavergne and R for Data Science (R4DS)") +
  theme(
    panel.background = element_rect(fill = "grey97", linetype = 0),
    plot.background = element_rect(fill = "grey97", linetype = 0),
    text = element_text(family = "Open Sans"),
    legend.position = c(0.15, 0.1),
    legend.direction = "horizontal",
    legend.title = element_text(
      vjust = .8,
      size = 8),
    legend.text = element_text(
      size = 8),
    plot.title = element_text(
      hjust = 0,
      size = 22, 
      margin = margin(5, 2, 5, 2),
      face = "bold"),
    plot.subtitle = element_text(
      hjust = 0,
      size = 10,
      margin = margin(2, 2, 2, 2)),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    plot.caption = element_text(
      size = 8,
      family = "Open Sans",
      color = "grey50")
  ) +
  annotate(
    geom = "text",
    x = 75,
    y = -25,
    label = "Madagascar is home\n to almost 20% of all\n extinct species!",
    family = "Open Sans",
    size = 3.5)

ggsave(here("images", "Week_33_Plants.png"), dpi = 400)
