library(extrafont)
library(geofacet)
library(gganimate)
library(ggtext)
library(here)
library(tidyverse)

mobile <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-10/mobile.csv')
landline <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-10/landline.csv')

d1 <- mobile %>% 
  select(-total_pop, -gdp_per_cap) %>% 
  filter(continent == "Europe") %>%
  filter(!entity %in% c("Åland Islands",
                        "Andorra",
                        "Guernsey",
                        "Isle of Man",
                        "Jersey",
                        "Liechtenstein",
                        "Monaco",
                        "San Marino",
                        "Serbia excluding Kosovo", 
                        "Svalbard and Jan Mayen", 
                        "USSR", 
                        "Vatican",
                        "West Germany")) %>%
  mutate(
    entity = case_when(
      entity == "Bosnia and Herzegovina" ~ "BiH",
      TRUE ~ entity)
  )

d2 <- landline %>% 
  select(-total_pop, -gdp_per_cap) %>% 
  filter(continent == "Europe") %>%
  filter(!entity %in% c("Åland Islands",
                        "Andorra",
                        "Guernsey",
                        "Isle of Man",
                        "Jersey",
                        "Liechtenstein",
                        "Monaco",
                        "San Marino",
                        "Serbia excluding Kosovo", 
                        "Svalbard and Jan Mayen", 
                        "USSR", 
                        "Vatican",
                        "West Germany")) %>%
  mutate(
    entity = case_when(
      entity == "Bosnia and Herzegovina" ~ "BiH",
      TRUE ~ entity)
  ) %>%
  full_join(d1, by = c("entity", "code", "year", "continent"))


new_grid <- europe_countries_grid1 %>%
  filter(!name %in% c("Turkey", "Cyprus")) %>%
  mutate(
    name = case_when(
      name == "Bosnia and Herzegovina" ~ "BiH",
      name == "Czechia" ~ "Czech Republic",
      name == "Moldova, Republic of" ~ "Moldova",
      name == "Macedonia, the former Yugoslav Republic of" ~ "Macedonia",
      name == "Russian Federation" ~ "Russia",
      TRUE ~ name
    )
  ) %>% 
  add_row(row = 2, col = 1, code = "FI", name = "Faeroe Islands") %>%
  add_row(row = 7, col = 2, code = "GI", name = "Gibraltar") %>%
  mutate(
    row = replace(row, name=="Sweden", 2),
    col = replace(col, name=="United Kingdom", 3),
    col = replace(col, name=="Ireland", 2)
    )


ggplot(d2, aes(x = year)) +
  geom_line(aes(y = landline_subs), colour = "#E69A8DFF") +
  geom_line(aes(y = mobile_subs), colour = "#5F4B8BFF") +
  facet_geo(~ entity, grid = new_grid, strip.position = 'bottom') +
  labs(
    y = "per 100 people",
    title = "Don't Leave Me Hanging On The...?",
    subtitle = "A Comparison of European <span style = 'color:#E69A8DFF;'>**Landline** </span>and<span style = 'color:#5F4B8BFF;'> **Mobile**</span> Subscriptions (1990-2020)",
    caption = "<span style = 'color:#5F4B8BFF;'>@CSHoggard</span> | <span style = 'color:#E69A8DFF;'>#TidyTuesday Week 46</span> | <span style = 'color:#5F4B8BFF;'>Source: OurWorldInData.org</span>"
  ) +
  theme_minimal() + 
  theme(
    plot.margin = margin(30,50,10,50),
    plot.background = element_rect(fill = "#f9f9f9", colour = "#f9f9f9"),
    plot.title = element_text(size = 42, family = "Lato", face = "bold", hjust = 0.5, margin = margin(10,0,20,0)),
    plot.subtitle = element_textbox_simple(size = 14, family = "Lato", halign = 0.5, margin = margin(0,0,40,0)),
    plot.caption = element_textbox_simple(size = 10, family = "Lato", halign = 0.5, margin = margin(40,0,10,0)),
    strip.text = element_text(size = 9, family = "Lato", face = "bold"),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.y = element_text(size = 9, family = "Lato")
  )
  
ggsave("images/Week_46_Phones.png", plot = last_plot(), width = 300, height = 300, units = "mm", dpi = 400) 

