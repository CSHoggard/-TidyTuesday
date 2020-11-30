library(tidyverse)
library(extrafont)
library(ggtext)
library(here)

data <- read_rds(url('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-24/hike_data.rds'))

data_parsed <- data %>%
  mutate(
    rating = as.numeric(rating),
         location = str_extract(location, "[^-]+")) %>%
  filter(rating > 0) %>%
  group_by(location) %>%
  mutate(
    region_score = mean(rating)) %>%
  arrange(desc(region_score))

ggplot(data_parsed, aes(x = location, y = region_score)) +
  geom_segment(aes(x = reorder(location, desc(region_score)), xend = location, y = 0, yend = region_score), colour = "#51340a", size = 3) +
  lims(y = c(0,5)) +
  geom_point(fill = "#42692f",
             colour = "#213417",
             size = 24,
             shape = 21,
             stroke = 2) +
  labs(title = "WASHINGTON HAS OVER 3600 HIKING ROUTES\nBUT WHERE ARE THE 'BEST' TRAILS?",
       subtitle = "Washington Trails Association (WTA) Aggregated Ratings (out of 5)",
       caption = "@CSHoggard | #TidyTuesday Week 48 | Source: WTA") +
  geom_text(aes(location, region_score, label = round(region_score, digits = 2)), family = "Oswald", color = "#FFFFFF", size = 7, check_overlap = TRUE) +
  geom_text(aes(location, 0, label = location), family = "Oswald", fontface = "bold", angle = 90, vjust = 1.5, hjust = 0, color = "#692f42", size = 7, check_overlap = TRUE) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#F1F5F7", colour = NA),
    plot.margin = margin(20,20,20,20),
    plot.title = element_text(size = 56, colour = "#692f42", family = "Oswald", face = "bold", margin = margin(10,10,20,10)),
    plot.subtitle = element_text(size = 22, colour = "#EAA92B", family = "Oswald", margin = margin(0,10,10,10)),
    plot.caption = element_text(size = 14, colour = "#EAA92B", family = "Oswald", face = "bold", margin = margin(10,0,5,0)),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  )

ggsave("images/Week_48_WTA.png", plot = last_plot(), dpi = 400, height = 330, width = 410, units = "mm") 
