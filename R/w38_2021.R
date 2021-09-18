library(tidyverse)
library(lubridate)
library(ggtext)
library(extrafont)
library(ggridges)
library(here)

billboard <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-14/billboard.csv')
features <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-14/audio_features.csv')

data_clean <- left_join(billboard, features, by = c('song_id', 'song', 'performer')) %>%
  mutate(date = mdy(week_id)) %>% 
  mutate(year = year(date)) %>%
  select(song_id, danceability, year) %>%
  filter(year > 1969) %>%
  mutate(year = as.factor(year)) %>%
  drop_na() %>%
  unique() 

test <- sample_n(data_clean, 1000)

ggplot(data_clean, aes(x = danceability, y = year)) + 
  geom_density_ridges(scale = 10, colour = "#FAF9F6", fill = "#000000") +
  coord_cartesian(clip = "off") +
  labs(title = "#TIDYTUESDAY",
       caption = "<b style='font-size:10pt;'> THE DANCEABILITY OF 18,486 SONGS <br>FROM THE BILLBOARD TOP 100 (1970-2021)</b><br><br><br><br>Bottom: 1970 • Top: 2021<br>Left: Low Danceability • Right: High Danceability <br><br><br><br><br><br> @CSHoggard | #TidyTuesday Week 38 | Source: Data.World") +
  theme_void() +
  theme(plot.margin = margin(60,160,40,160),
        plot.background = element_rect(fill = "#000000",  colour = "#FAF9F6"),
        panel.background = element_rect(fill = "#000000"),
        plot.title = element_textbox_simple(colour = "#FAF9F6", family = "Helvetica", size = 30, halign = 0.5, margin = margin(10,0,3,0)),
        plot.caption = element_textbox_simple(colour = "#FAF9F6", family = "Helvetica", size = 8, halign = 0.5, margin = margin(20,0,0,0)))


ggsave("images/Week_38_Billboard_Top100.png", plot = last_plot(), width = 190, height = 190, units = "mm", dpi = 400) 
