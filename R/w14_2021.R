library(tidyverse)
library(extrafont)
library(ggtext)
library(here)

allShades <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-30/allShades.csv')

allshades.clean <- allShades %>%
  filter(str_detect(description, "Natural|natural")) %>%
  mutate(idx = row_number()) %>%
  mutate(idy = seq_along(id))

ggplot(allshades.clean, aes(idx, idy, fill = hex)) +
  geom_tile() +
  labs(caption = "<span style = 'color:#392928;'><b style='font-size:16pt;'>151 foundation shades which including the word 'natural' </b><br><br><br><br> <span style = 'color:#808080;'>Natural foundations are supposed to mimic the skin. <br><br> But biases are seen if words aren't used consistently across the colour spectrum.</span> <br><br><br><br><br> <span style = 'color:#808080;'> Source: The Pudding | @CSHoggard </span>") +
  scale_fill_identity() +
  ylim(c(-1.5, 1.5)) +
  geom_text(x = 132,
            y = -1.5,
            size = 11,
            family = "Lexend Medium",
            label = "What is natural?",
            colour = "#392928") + 
  coord_polar() +
  theme(plot.margin = margin(20,20,20,20),
        panel.background = element_blank(),
        plot.background = element_rect(fill = "#f3e9e1", colour = NA),
        plot.caption = element_textbox_simple(family = "Lexend Light", halign = 0.5, margin = margin(30,0,0,0), colour = "#808080"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())


ggsave("images/Week_14_makeup.png", plot = last_plot(), width = 200, height = 300, units = "mm", dpi = 450) 
