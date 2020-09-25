library(tidyverse)
library(extrafont)
library(ggtext)
library(here)

expeditions <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/expeditions.csv')

expeditions %>% select(year,
                       member_deaths,
                       hired_staff_deaths) %>%
  mutate(tragedy = hired_staff_deaths > 0 | member_deaths > 0) %>%
  ggplot(aes(x = year, 
             fill  = tragedy,
             colour = tragedy,
             alpha = tragedy)) +
  geom_dotplot(dotsize = 0.5,
               binwidth = 1,
               binpositions = "all") +
  labs(caption = "@CSHoggard  •  Data: The Himalayan Database  •  #TidyTuesday Week 39") +
  annotate("text",
           x = 1945,
           y = 0.75,
           size = 10,
           colour = "#EAD637",
           family = "Oswald",
           fontface = "bold",
           label = "CLIMBING TO DISASTER?") +
  annotate("text",
           x = 1945,
           y = 0.69,
           size = 2.9,
           colour = "grey50",
           family = "Oswald",
           label = "Between 1905 and Spring 2020 there have been a total of 10,346 expeditions in the Nepal Himalaya.\n711 expeditions have resulted in the loss of life, and 1106 fatalities are recorded.") +
  annotate("text",
           x = 1945,
           y = 0.652,
           size = 2.9,
           colour = "#EAD637",
           family = "Oswald",
           label = 
            "Each yellow dot is an expedition with at least one fatality.") +
  annotate("text",
           x = 1945,
           y = 0.576,
           size = 2.9,
           colour = "grey50",
           family = "Oswald",
           label = 
           "Of these fatalities, over a third (40%) are locally-hired guides. \n
           Being a Sherpa is a job of little glory. While lucrative, Sherpas do not receive certificates to prove
           they have reached summits: The Nepalese Tourism Ministry say that Sherpas are just 'doing their job'.\n
           And while there have been no Sherpa deaths on Everest since 2015 other 
           mountains continue to kill, and earthquakes and avalanches are too frequent.") + 
  scale_fill_manual(values = c("#918B76","#EAD637")) +
  scale_colour_manual(values = c("#918B76","#EAD637")) +
  scale_alpha_manual(values = c(0.2,0.8)) +
  scale_x_continuous(breaks = c(1905, 1950, 2000, 2020)) +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "black"),
        plot.margin = margin(50, 10, 25, 10),
        legend.position = "none",
        plot.caption = element_text(size = 8, colour = "grey50", hjust = 0.5, family = "Oswald", margin = margin(25, 0, -5, 0)),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 10, family = "Oswald", face = "bold", margin = margin(-25, 0, 0, 0)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave("-TidyTuesday/images/Week_39_Himalayas.png", plot = last_plot(), dpi = 500, width = 7, height = 13)       



