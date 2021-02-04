library(tidyverse)
library(extrafont)
library(ggtext)
library(here)

artwork <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-12/artwork.csv')
artists <- read_csv("https://github.com/tategallery/collection/raw/master/artist_data.csv")

artists.parsed <- artists %>%
  select(name, gender, yearOfBirth)

artwork.parsed <- artwork %>% 
  select(artist) %>%
  filter(!artist == "Turner, Joseph Mallord William") %>%
  rename("name" = artist) %>%
  group_by(name) %>%
  count() %>%
  left_join(artists.parsed, by = "name") %>%
  drop_na()

set.seed(3796)

ggplot(artwork.parsed, aes(yearOfBirth, 0, size = n, colour = gender)) +
  geom_point(alpha = 0.6, position = position_jitter(width=0.1, height= 0.25)) +
  theme_minimal() +
  scale_size_area(max_size = 20) +
  labs(title = "500 Years of the Tate Art Collection",
       subtitle = "<span style = 'color:#96ceb4;'>Male</span> vs. <span style = 'color:#4d648d;'>Female</span> Artists",
       caption = "Size: Number of Works \n\n\n @CSHoggard | #TidyTuesday Week 3 (2021) | Source: Tate Collection") +
  scale_colour_manual(values = c("#4d648d", "#96ceb4")) +
  scale_x_continuous(minor_breaks = seq(1500, 2020, 100)) +
  theme(panel.background = element_blank(),
        plot.margin = margin(30,20,20,30),
        plot.title = element_text(hjust = 0.5, size = 24, colour = "grey30", family = "Roboto Light", margin = margin(0,30,20,30)),
        plot.subtitle = element_textbox_simple(halign = 0.5, size = 16, colour = "grey30", family = "Roboto Light", margin = margin(0,30,20,30)),
        plot.caption = element_text(hjust = 0.5, size = 10, colour = "grey30", family = "Roboto Light", margin = margin(30,30,0,30)),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 10, family = "Roboto Light"),
        axis.text.y = element_blank(),
        legend.position = "none")

ggsave("images/Week_3_Tate.png", plot = last_plot(), dpi = 500) 
