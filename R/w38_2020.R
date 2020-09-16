library(tidyverse)
library(ggrepel)
library(extrafont)
library(gganimate)
library(here)
library(scales)

kids <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-15/kids.csv')

kids.clean <- kids %>% filter(variable %in% "highered") %>%
  mutate(inf_adj_perchild = round(inf_adj_perchild, 3)) %>%
  mutate(inf_adj_perchild = inf_adj_perchild * 1000) %>%
  group_by(year) %>%  
  arrange(year, -inf_adj_perchild) %>%
  mutate(rank = 1:n()) %>%
  filter(rank <= 15)

p <- ggplot(kids.clean, aes(rank, inf_adj_perchild, fill = state, alpha = 0.5)) +
  geom_col(alpha = 0.8) +
  geom_text(aes(y = 100, label = paste(state, " "), family = "IBM Plex Sans"), vjust = 0.25, hjust = 0) +
  geom_text(aes(y=inf_adj_perchild, label = as.character(dollar(inf_adj_perchild)), family = "IBM Plex Sans"), hjust= -0.1) +
  geom_text(x = -14.75 , y = 4250, aes(label = as.character(year), family = "IBM Plex Sans"), size = 16, col = "grey18") +
  coord_flip(clip = "off", expand = TRUE) +
  scale_x_reverse() +
  ylim(0,5000) + 
  theme_minimal() +
  theme(legend.position = "none",
        plot.margin = margin(30, 30, 30, 30),
        plot.title = element_text(size = 20, family = "IBM Plex Sans", margin = margin(20,0,5,0)),
        plot.subtitle = element_text(size = 14, family = "IBM Plex Sans", margin = margin(0,0,15,0)),
        plot.caption = element_text(size = 10, family = "IBM Plex Sans", margin = margin(20, 0, 0, 0)),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 12, family = "IBM Plex Sans")) +
  transition_states(year) +
  labs(y = "Spending per individual",
       title = "US Public Spending on Higher Education",
       subtitle = "Top 15 States in...",
       caption = "Source: State and Local Government Finance Survey (United States Census Bureau) \n Courtesy of the Urban Institute and the tidykids R package \n @CSHoggard | #TidyTuesday Week 38") 

animate(p, nframes = 130, width = 450, height = 600)
anim_save("images/Week_38_Kids.gif", animation = last_animation())
