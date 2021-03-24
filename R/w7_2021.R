library(tidyverse)
library(extrafont)
library(ggridges)
library(ggtext)

devtools::install_github("cshoggard/morris")
library(morris)

loadfonts(device = "win")

theme_set(theme_minimal(base_size = 12, base_family = "Open Sans"))

income_distribution <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/income_distribution.csv')

income.parsed <- income_distribution %>%
  filter(year == 2019,
         !race == "All Races",
         !race == "Asian Alone or in Combination",
         !race == "Black Alone or in Combination",
         !race == "White Alone, Not Hispanic",
         !race == "Hispanic (Any Race)") %>%
  mutate(race = case_when(
    race == "White Alone" ~ "White Americans",
    race == "Black Alone" ~ "Black Americans",
    race == "Asian Alone" ~ "Asian Americans",
    TRUE ~ race),
  income_bracket = case_when(
    income_bracket == "$15,000 to $24,999" ~ "$15000-$24999",
    income_bracket == "$25,000 to $34,999" ~ "$25000-$34999",
    income_bracket == "$35,000 to $49,999" ~ "$35000-$49999",
    income_bracket == "$50,000 to $74,999" ~ "$50000-$74999",
    income_bracket == "$75,000 to $99,999" ~ "$75000-$99999",
    income_bracket == "$100,000 to $149,999" ~ "$100000-$149999",
    income_bracket == "$150,000 to $199,999" ~ "$150000-$199999",
    income_bracket == "$200,000 and over" ~ "$200,000 +",
    TRUE ~ income_bracket
  )) %>%
  select(-number, 
         -income_med_moe, 
         -income_mean_moe, 
         -year) %>% 
  mutate(income_bracket = fct_relevel(income_bracket, 
                                      "Under $15,000",
                                      "$15000-$24999",
                                      "$25000-$34999",
                                      "$35000-$49999",
                                      "$50000-$74999",
                                      "$75000-$99999",
                                      "$100000-$149999",
                                      "$150000-$199999",
                                      "$200,000 +"))

ggplot(income.parsed, aes(income_bracket, 
                          income_distribution, 
                          fill = race)) +
  geom_col(alpha = 0.6, width = 0.7) +
  labs(title = "Quantifying Wealth Inequality",
       caption = "<span style = 'color:#668995;'>@CSHoggard</span>  |  Data: Urban Institute & US Census  |  <span style = 'color:#668995;'>#TidyTuesday Week 7 (2021)</span>") +
  scale_fill_peacock() + 
  facet_wrap(~race, nrow = 1) +
  theme(plot.margin = margin(30,30,10,30),
        panel.spacing = unit(2, "lines"),
        strip.text.x = element_text(size = 16, family = "Commissioner", hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(size = 24, family = "Commissioner", hjust = 0.5, margin = margin(0,0,50,0)),
        plot.caption = element_textbox_simple(family = "Commissioner", size = 12, halign = 0.5, margin = margin(20,0,0,0)),
        legend.position = "none")
