library(tidyverse)
library(tidygeocoder)
library(here)
library(ggmap)
library(osmdata)
library(ggtext)
library(extrafont)

shelters <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-01/shelters.csv')

shelters.clean <- shelters %>% 
  select(shelter_address, shelter_city, shelter_province, capacity) %>%
  distinct() %>%
  filter(capacity > 0) %>%
  mutate_if(is.character, str_replace_all, pattern = "Bathrust", replacement = "Bathurst") %>%
  mutate(shelter_address = str_remove_all(shelter_address, "Toronto")) %>%
  mutate(shelter_address = str_remove_all(shelter_address, ", 2nd floor")) %>% 
  tidygeocoder::geocode(street = shelter_address, city = shelter_city, state = shelter_province, method = 'osm')

toronto_coords <- getbb("Toronto")

toronto_roads <- toronto_coords %>% 
  opq() %>% 
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", 
                            "secondary", "tertiary")) %>%
  osmdata_sf()


toronto_water <- toronto_coords %>% 
  opq() %>%
  add_osm_feature(key = "waterway", value = "river") %>%
  osmdata_sf()

toronto_land <- toronto_coords %>%
  opq() %>%
  add_osm_feature(key = "boundary", value = "political") %>%
  osmdata_sf()


ggplot(shelters.clean) +
  geom_sf(data = toronto_roads$osm_lines,
          size = 0.4, 
          alpha = 0.4,
          inherit.aes = FALSE) +
  geom_sf(data = toronto_water$osm_lines,
          size = 1,
          alpha = 0.4,
          inherit.aes = FALSE) +
  geom_sf(data = toronto_land$osm_lines,
          alpha = 0.6,
          inherit.aes = FALSE) +
  coord_sf(xlim = c(-79.62, -79.1),
           ylim = c(43.55, 43.85),
           expand = FALSE)+
  geom_point(aes(long, lat, 
                 size = capacity),
             shape = 21,
             fill = "#C40C0C",
             colour = "#940909",
             alpha = 0.6) +
  labs(caption = "<span style = 'color:#940909;'>@CSHoggard</span>  |  Data: opendatatronto  |  <span style = 'color:#940909;'>#TidyTuesday Week 49</span>",
       colour = "Shelter Type",
       size = "Capacity:") +
  annotate(geom = 'richtext', x = -79.22, y = 43.635, size = 6, colour = "#940909", fill = NA, label = "The Struggle for Accomodation in Toronto", family = "Roboto", fontface = "bold", label.colour = NA) +
  annotate(geom = 'richtext', x = -79.22, y = 43.605, size = 3.5, colour = "grey40", fill = NA, label = "There are over **10,000 homeless people in Toronto every night**<br>While it may appear that Toronto has sufficient capacity,<br>the city faces huge problems.<br><br>With reports of increasing violence in shelters,<br>attempts to shut down the **Toronto Tiny Shelters (TTS)** initiative,<br>and the on-going COVID-19 pandemic the situation is deteriorating quickly.", family = "Roboto", label.colour = NA) +
  annotate(geom = 'richtext', x = -79.22, y = 43.575, size = 3.5, colour = "grey40", fill = NA, label = "Please consider donating to charities including<br>**Covenant House**, **Na-Me-Res** and **Sojourn House** today.", family = "Roboto", label.colour = NA) +
  theme_minimal() +
  theme(
    plot.margin = margin(30,30,30,30),
    plot.background = element_rect(color = "grey97", fill = "grey98", size = 2),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(family = "Roboto", colour = "grey99"),
    axis.text.y = element_text(family = "Roboto", colour = "grey99"),
    legend.title = element_text(family = "Roboto"),
    legend.text = element_text(family = "Roboto"),
    legend.position = "bottom",
    panel.grid.major = element_line(colour = "grey99"),
    plot.caption = element_textbox_simple(family = "Roboto", face = "bold", halign = 0.5, margin = margin(15,0,0,0))
  ) 


ggsave("images/Week_49_Toronto.png", plot = last_plot(), width = 300, height = 300, units = "mm", dpi = 320) 
