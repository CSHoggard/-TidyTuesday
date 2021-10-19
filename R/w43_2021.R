library(tidyverse)
library(janitor)
library(ggtext)
library(magick)
library(cowplot)
library(extrafont)
library(here)

pumpkins <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-19/pumpkins.csv') %>%
  clean_names()

pumpkin <- image_read("https://raw.githubusercontent.com/CSHoggard/-TidyTuesday/master/R/week_43_image_data/pumpkin.png") #https://iconscout.com/icons/pumpkin" 

set.seed(5678)

pumpkins.clean <- pumpkins %>%
  filter(str_detect(id ,"2021")) %>%
  filter(!str_detect(country, "exhibition")) %>%
  filter(!country %in% c("United States", "Canada", "New Zealand")) %>%
  select(-id, -place, -city, -seed_mother, -pollinator_father, -ott, -est_weight, -pct_chart, -variety) %>%
  mutate(weight_lbs = parse_number(weight_lbs)) %>%
  mutate(weight_kg = (weight_lbs * 453.59237)/1000) %>%
  sample_n(nrow(.)) %>%
  rowid_to_column()

pumpkins.top.two <- pumpkins.clean %>%
  top_n(., 2, weight_kg) %>%
  mutate(weight_kg = round(weight_kg, 2)) %>%
  separate(grower_name, c("Last Name", "First Name"), sep = ",") %>%
  mutate(label = glue::glue("<b><span style='font-size:12pt'><span style='color:#363636;'>{weight_kg} kg</span></span></b> <br><span style='font-size:9pt'>{`First Name`} {`Last Name`}</span><br><b><span style='font-size:7pt'>{gpc_site} ({country})</span></b>"))

ggplot(pumpkins.clean, aes(rowid, weight_kg)) +
  geom_segment(aes(x = rowid, xend = rowid, y = 0, yend = weight_kg), colour = "#ED9B34", size = 2) +
  geom_segment(aes(x = rowid, xend = rowid, y = 0, yend = weight_kg), colour = "#FAF9F6", size = 0.5) +
  geom_point(data = pumpkins.top.two, aes(rowid, weight_kg), shape = 21, stroke = 1, fill = "#ED9B34", colour = "#FAF9F6") +
  scale_y_reverse(lim = c(1750,0)) +
  labs(y = "weight (kg)",
       caption = "@CSHoggard | #TidyTuesday Week 43 | Source: Great Pumpkin Commonwealth") +
  annotate(geom = "richtext", 
           x = 550, y = 1500, 
           label = "Europe's Biggest Pumpkins!",
           family = "Commissioner",
           fontface = "bold",
           colour = "#363636",
           label.colour = NA,
           size = 10,
           fill = NA) +
  annotate(geom = "richtext", 
           x = 550, y = 1650, 
           label = "This year <span style='color:#363636;'><b>over 90</span></b> pumpkins have been officially <br> recorded as <span style='color:#363636;'><b>weighing more than 500kg!</span></b> <br><br>(or the same as a small caravan!)",
           family = "Commissioner",
           colour = "#FAF9F6",
           label.colour = NA,
           size = 5,
           fill = NA) +
  geom_richtext(data = pumpkins.top.two, aes(x = rowid, y = (weight_kg + 75), label = label),
                lineheight = 0.8,
                size = 4,
                hjust = 0.5,
                family = "Commissioner",
                colour = "white",
                fill = NA,
                label.color = NA) +
  theme_minimal() +
  draw_image(pumpkin, scale = 350, x = 100, y = -1700) +
  coord_cartesian(clip = "off") +
  theme(text=element_text(family = "Commissioner"),
        plot.margin = margin(20,20,20,20),
        plot.caption = element_textbox_simple(colour = "#FAF9F6", family = "Commissioner", size = 10, halign = 1, margin = margin(20,0,0,0)),
        plot.background = element_rect(fill = "#ED9B34",  colour = "#FAF9F6", size = 2),
        panel.background = element_rect(fill = "#ED9B34", colour = "#ED9B34"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(colour = "#FAF9F6", margin = margin(0,20,0,0)),
        axis.text.x = element_blank(),
        axis.text.y = element_text(colour = "#FAF9F6")
        )

ggsave("images/Week_43_Pumpkins.png", plot = last_plot(), width = 200, height = 270, units = "mm")
