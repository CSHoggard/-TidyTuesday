library(rcartocolor)
library(tidyverse)
library(extrafont)
library(gganimate)
library(ggtext)
library(here)

datasaurus <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-13/datasaurus.csv')
colours <- c(carto_pal(12, "Vivid"), "#000000")

p <- datasaurus %>%
  group_by(dataset) %>%
  summarise(mean_x = mean(x),
            mean_y = mean(y),
            sd_x   =   sd(x),
            sd_y   =   sd(y),
            coefficient  = cor(x, y, method = "pearson")) %>%
  left_join(datasaurus) %>%
  ggplot(aes(x = x, y = y)) +
  coord_equal(clip = "off") +
  geom_point(aes(colour = dataset, group = 1L), size = 3, alpha = 0.6) +
  scale_color_manual(values = colours, guide = "none") +
  scale_x_continuous(limits = c(0, 120), breaks = seq(0, 100, by = 20)) +
  scale_y_continuous(limits = c(0, 140), breaks = seq(0, 100, by = 20)) +
  geom_text(aes(x = 50, y = 136, label = paste("mean (x):", round(mean_x,4)), family = "Commissioner"), size = 4.5, color = "grey70") +
  geom_text(aes(x = 50, y = 132, label = paste("mean (y):", round(mean_y,4)), family = "Commissioner"), size = 4.5, color = "grey70") +
  geom_text(aes(x = 50, y = 128, label = paste("standard deviation (x):", round(sd_x,4)), family = "Commissioner"), size = 4.5, color = "grey70") +
  geom_text(aes(x = 50, y = 124, label = paste("standard deviation (y):", round(sd_y,4)), family = "Commissioner"), size = 4.5, color = "grey70") +
  geom_text(aes(x = 50, y = 120, label = paste("correlation", round(coefficient,4)), family = "Commissioner"), size = 4.5, color = "grey70") +
  labs(title = "The Importance of Data Visualisation",
       subtitle = "<b style='font-size:24pt;'>Case Study: The DataSaurus Dozen</b><br><br>The DataSaurus Dozen highlights the importance of visualising data; while summary statistics (means, standard deviations and correlation measures) for various datasets can be the same, their distributions can be significantly varied. This was first elegantly demonstrated in 1973 by the English statistician Francis John Anscombe. Anscombe's four datasets (<span style = 'color:#068a84;'>Anscombe's Quartet</span>) appeared to be similar when using typical summary statistics, yet told four different stories when graphed. <br><br>In 2016, Alberto Cairo developed the DataSaurus Dozen: a series of 13 different datasets with almost-identical summary statistics and different visualisations. This was published, together with other datasets, by Justin Matejka and George Fitzmaurice in 2017.",
       caption = "• Produced by @CSHoggard  |   #TidyTuesday Week 42 • \n • Data: Francis Anscombe, Alberto Cairo, Justin Matejka & George Fitzmaurice •") +
  theme_minimal() +
  theme(plot.title = element_text(family = "Commissioner", 
                                  size = 42,
                                  color = "#068a84", 
                                  hjust = 0.5,
                                  margin = margin(15, 0, 40, 0)),
        plot.title.position = "plot",
        plot.subtitle = element_textbox_simple(family = "Commissioner", 
                                     size = 12,
                                     color = "grey30", 
                                     halign = 0.5,
                                     lineheight = 1.2,
                                     margin = margin(10, 0, 0, 0)),
        plot.caption = element_text(family = "Commissioner",
                                    size = 12,
                                    colour = "grey30",
                                    lineheight = 1.2,
                                    hjust = 0.5,
                                    margin = margin(40, 0, 20, 0)),
        plot.caption.position = "plot",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = margin(90, 70, 90, 70),
        plot.background = element_rect(color = "white", fill = "white")) +
  transition_states(dataset, 10, 3) + 
  ease_aes('cubic-in-out') 

animate(p, nframes = 120, fps = 8, detail = 5, width = 900, height = 1100)
anim_save("images/Week_42_Datasaurus.gif", animation = last_animation())
