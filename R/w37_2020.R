library(tidyverse)
library(tidytext)
library(here)
library(extrafont)
library(cowplot)

friends <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-08/friends.csv')

friends.parsed <- friends %>% filter(speaker %in% c("Monica Geller",
                                  "Joey Tribbiani",
                                  "Chandler Bing",
                                  "Rachel Green",
                                  "Phoebe Buffay",
                                  "Ross Geller")) %>%
  mutate(
    word_count = str_count(text, "\\w+"),
    speaker = word(speaker)
  ) %>%
  select(-utterance, -scene) %>% 
  group_by(speaker, season) %>%
  summarise(word_sum = sum(word_count))

friends.parsed %>% 
  group_by(speaker) %>%
  summarise(total_sum = sum(word_sum))

img_friends <- "https://turbologo.com/articles/wp-content/uploads/2019/12/friends-logo-cover-678x381.png."

p <- ggplot(friends.parsed, aes(season, word_sum, colour = speaker)) + 
  geom_line(size = 1) +
  geom_point(size = 3) +
  lims(y = c(5000,14000)) +
  scale_x_continuous(n.breaks = 10) +
  scale_colour_manual(values = c("#3F9DD4", "#008F48", "#F74035", "#9787CD", "#F6D400", "#941205")) +
  labs(title = "The One About Who Spoke the Most?",
       subtitle = "Total number of words per season",
       caption = "@CSHoggard | Source: friends R package | #TidyTuesday Week 37",
       x = "Season",
       y = "Word count") +
  annotate("text", x = 9, y = 13500, size = 4, colour = "grey97", family = "IBM Plex Sans", label = "Rachel spoke over 17,000 words \n more than Pheobe!") +
  guides(colour = guide_legend(nrow = 1, override.aes = list(linetype = 0, size=4))) +
  theme_minimal() +
  theme(text = element_text(family = "IBM Plex Sans"),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(color = "grey97", size = 11),
        plot.margin = margin(20, 20, 20, 20),
        plot.background = element_rect(fill = "#000000"),
        panel.grid.major = element_line(size = 0.35, linetype = 'solid', colour = "grey10"),
        panel.grid.minor = element_line(size = 0.3, linetype = 'solid', colour = "grey10"),
        axis.text.x = element_text(color = "grey97"),
        axis.title.x = element_text(color = "grey97", margin = margin(20, 0, 5, 0)),
        axis.text.y = element_text(color = "grey97", margin = margin(0, 20, 0, 5)),
        axis.title.y = element_text(color = "grey97"),
        plot.title = element_text(color = "grey97", hjust = 0.5, family = "Gabriel Weiss' Friends Font",size = 32, margin = margin(0, 0, 10, 0)),
        plot.subtitle = element_text(color = "grey97", hjust = 0.5, size = 12, margin = margin(0, 0, 20, 0)),
        plot.caption = element_text(size = 9, colour = "grey97", margin = margin(20, 0, 0, 0)))

ggdraw() +
  draw_plot(p) +
  draw_image(img_friends, scale=0.2, y=-0.45, x=-0.4)

ggsave("images/Week_37_Friends.png", plot = last_plot(), dpi = 400, height = 180, width = 300, units = "mm")

