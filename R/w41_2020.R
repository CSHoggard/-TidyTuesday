library(tidyverse)
library(extrafont)
library(waffle)
library(here)

tournament <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-06/tournament.csv')

tournament.clean <- tournament %>% 
  filter(!is.na(seed)) %>%
  filter(year > 1993) %>%
  mutate(
    tournament_finish = case_when(
      tourney_finish == "OR" ~ "1st Round",
      tourney_finish == "1st" ~ "1st Round",
      tourney_finish == "2nd" ~ "2nd Round",
      tourney_finish == "RSF" ~ "Sweet Sixteen",
      tourney_finish == "RF" ~ "Elite Eight",
      tourney_finish == "NSF" ~ "Semi-Finals",
      tourney_finish == "N2nd" ~ "Runner-Ups",
      tourney_finish == "Champ" ~ "Champions"),
    seed_group = case_when(
      seed %in% c('1', '2', '3') ~ '1-3',
      seed %in% c('4', '5', '6') ~ '4-6',
      seed %in% c('7', '8', '9') ~ '7-9',
      seed %in% c('10', '11', '12') ~ '10-12',
      seed %in% c('13', '14', '15', '16') ~ '13-16'),
    seed_group = factor(seed_group, 
                        levels = c('1-3',
                                   '4-6',
                                   '7-9',
                                   '10-12',
                                   '13-16'),
                        ordered = TRUE),
    tournament_finish = factor(tournament_finish,,
                               levels = c("1st Round",
                                          "2nd Round",
                                          "Sweet Sixteen",
                                          "Elite Eight",
                                          "Semi-Finals",
                                          "Runner-Ups",
                                          "Champions"))) %>%
  count(seed_group, tournament_finish, sort = TRUE)
  
  
ggplot(tournament.clean, aes(fill = seed_group, values = n)) +
  geom_waffle(color = "white", size = 0.4, n_rows = 10, flip = TRUE) +
  scale_y_continuous(labels = function(x) x * 10,
                     expand = c(0,0)) +
  facet_wrap(~tournament_finish, nrow = 1, strip.position = "bottom") +
  labs(fill = "Seed",
       title = "The Plucky Underdogs?",
       subtitle = "Women's NCAA National Tournament Progress (1994-2020)",
       y = "Number of Schools",
       caption = "@CSHoggard  •  Data: FiveThirtyEight  •  #TidyTuesday Week 41") +
  scale_x_discrete() +
  coord_equal() +
  scale_fill_manual(values = c("#d5896f", "#dab785", "#70a288", "#075E9D", "#031d44")) +
  theme_minimal(base_family = "IBM Plex Sans") +
  theme(plot.margin = margin(20, 20, 20, 20),
        legend.position = "bottom",
        plot.title = element_text(size = 24, face = "bold", margin = margin(10,0,5,0)),
        plot.subtitle = element_text(size = 14, margin = margin(0,0,10,0)),
        plot.caption = element_text(size = 8, margin = margin(20,0,5,0)),
        panel.grid = element_blank(),
        axis.title.y = element_text(size = 10, margin = margin(0,20,0,0)),
        axis.ticks.y = element_line())

ggsave("images/Week_41_NCAA.png", plot = last_plot(), dpi = 400, height = 240, width = 250, units = "mm") 
