library(tidyverse)
library(lubridate)
library(magick)
library(extrafont)
library(cowplot)
library(ggtext)

fifteens <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-24/fifteens.csv')

fifteens.clean <- fifteens %>%
  filter(team_1 == "England" | team_2 == "England") %>%
  select(date, team_1, team_2, tournament, margin_of_victory, winner) %>%
  mutate(new_margin = case_when(
    winner != "England" ~ margin_of_victory * -1,
    TRUE ~ margin_of_victory
  )) %>%
  mutate(date = ymd(date)) %>%
  mutate(win = if_else(new_margin > 0, "Yes", "No")) %>%
  mutate(win = if_else(new_margin == 0, "Draw", win)) %>%
  filter(date > ymd(991231))

ggplot(fifteens.clean, aes(date, new_margin)) +
  geom_line(colour = "#F3F1F0", alpha = 0.3) +
  geom_point(aes(colour = win, fill = win), pch = 21, alpha = 0.8, size = 2.5) +
  scale_colour_manual(values = c("#F3F1F0",  "#2e1600", "#2e1600")) +
  scale_fill_manual(values = c("#F3F1F0", "#FF0000", "#F3F1F0")) +
  ylim(c(-50,100)) +
  labs(title = "THE WOMEN'S ENGLAND RUGBY TEAM <span style='font-size:18pt'>(2000-2022)</span>",
       subtitle = "Scoresheet : <span style='color:#F3F1F0;'><b>201</b></span> Wins | <span style='color:#FF0000;'><b>36</b></span> Losses | <span style='color:#7D6F63;'><b>3</b></span> Draws",
       caption = "@CSHoggard | TidyTuesday W21 | Data: ScrumQueens",
       x = NULL,
       y = "SCORE MARGIN") +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "#7D6F63") +
  theme_minimal() +
  theme(text = element_text(family = "Oswald"),
        plot.margin = margin(40,30,10,30),
        plot.caption.position = 'plot',
        plot.title.position = 'plot',
        plot.title = element_markdown(family = "Oswald", face = "bold", colour = "#F3F1F0", size = 30, margin = margin(0,0,10,0)),
        plot.subtitle = element_markdown(family = "Oswald", colour = "#7D6F63", margin = margin(0,0,80,0), size = 12),
        plot.caption = element_text(colour = "#F3F1F0", size = 11.5, margin = margin(40,0,10,0)),
        axis.title.y = element_text(colour = "#F3F1F0", family = "Oswald", margin = margin(0,10,0,0)),
        axis.text.x = element_text(colour = "#F3F1F0", family = "Oswald"),
        axis.text.y = element_text(colour = "#F3F1F0", family = "Oswald"),
        plot.background = element_rect(fill = "#2e1600"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none") 

ggsave("images/Week_21_Rugby.png", plot = last_plot(), dpi = 400)
