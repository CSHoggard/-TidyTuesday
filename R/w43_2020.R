library(tidyverse)
library(extrafont)
library(magick)
library(cowplot)
library(here)

beer_awards <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-20/beer_awards.csv')
logo <- image_read("https://i2.wp.com/thebeertravelguide.com/wp-content/uploads/2018/07/Great-American-Beer-Festival-Logo.jpg?ssl=1")

beer_awards %>% 
  select(medal, state) %>%
  mutate(
    state = case_when(
      state == "Ak" ~ "AK",
      state == "wa" ~ "WA",
      TRUE ~ state)) %>%
  count(state, sort = TRUE) %>%
  slice_head(n = 10) %>%
  mutate(pos = -5:4) %>% 
  rowwise() %>%
  mutate(
    x = list(c(-10, 0, 0, -10)),
    y = list(c(pos*4 - 1.4, pos*2 - 0.7, pos*2 + 0.7, pos*4 + 1.4))) %>% 
  unnest(cols = c(x, y)) %>%
  ggplot() +
  geom_rect(aes(xmin = -19, ymin = pos*4 - 1.4,
                xmax = -10, ymax = pos*4 + 1.4), fill = "#005F85", color = NA) +
  geom_polygon(aes(x, y, group = state), fill = "#6F94AA", color = NA) +
  geom_rect(aes(xmin = 0, ymin = pos*2 - 0.7,
                xmax = n/12, ymax = pos*2 + 0.7), fill = "#CD8D2A", color = NA) +
  geom_text(aes(-14.5, pos*4, label = state), family = "Futura Md BT", fontface = "bold", color = "#FFFFFF", hjust = 0.5, size = 8, check_overlap = TRUE) +
  geom_text(aes(n/25, pos*2, label = n), family = "Futura Bk BT", fontface = "bold", color = "#FFFFFF", size = 4, check_overlap = TRUE) +
  scale_x_continuous(breaks = seq(0, 80, 20), labels = seq(0, 1000, 250)) +
  labs(title = "AWARD WINNING BEERS",
       subtitle = "GABF Medals (1987-2020): Top Ten States",
       caption = "• Produced by @CSHoggard | #TidyTuesday Week 43 | Data: Great American Beer Festival • ") +
  theme_minimal() +
  theme(plot.title = element_text(colour = "#005F85", family = "Futura Md BT", face = "bold", size = 42, hjust = 0.5, margin = margin(10,0,10,0)),
        plot.subtitle = element_text(colour = "#6F94AA", family = "Futura Md BT", face = "bold", size = 24, hjust = 0.5, margin = margin(0,0,20,0)),
        plot.caption = element_text(colour = "#626262", family = "Bellota Text", face = "bold", size = 12, hjust = 0.5, margin = margin(40,0,0,0)),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(family = "Futura Md BT"),
        axis.text.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.margin = margin(30, 30, 30, 30)) +
  draw_image(logo, x = 55, y = 5, scale = 50)
  
ggsave("images/Week_43_GABF.png", plot = last_plot(), dpi = 400, height = 220, width = 220, units = "mm") 
