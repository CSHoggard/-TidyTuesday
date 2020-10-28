library(tidyverse)
library(maps)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(extrafont)
library(cowplot)
library(magick)
library(here)
library(ggtext)

wind_turbine <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-27/wind-turbine.csv')
canada <- ne_states(country = "canada", returnclass = "sf")

wind_clean <- wind_turbine %>%
  group_by(project_name) %>%
  summarise(
   longitude = mean(longitude),
   latitude = mean(latitude),
   total_project_capacity_mw = total_project_capacity_mw,
   province = province_territory) %>%
  unique()

wind_sf <-  st_as_sf(wind_clean, coords = c("longitude", "latitude"))
st_crs(wind_sf) <- 4326

ggplot(canada) +
  geom_sf(colour = NA, 
          fill = "grey81", 
          lwd = 0.2) +
  geom_point(data = wind_clean,
             aes(longitude, 
                 latitude,
                 colour = "#C40C0C",
                 size = total_project_capacity_mw),
             alpha = 0.4) +
  labs(title = "Wind: A Revolution in Canada",
       subtitle = "Roughly 60% of Canada’s wind turbines have been commissioned since 2010, with enough energy to power approximately<span style = 'color:#C40C0C;'> **3.4 million homes!** </span>",
       caption = "<b style='font-size:11pt;'> The Distribution and Power of 6698 Wind Turbines in Canada </b><br><br><br><br> @CSHoggard | #TidyTuesday Week 44 | Source: open.canada.ca") +
  theme(plot.margin = margin(60,120,20,120),
        panel.grid = element_blank(),
        panel.background = element_rect(colour = "grey97", fill = "grey97"),
        plot.background = element_rect(color = "#C40C0C", fill = "grey97", size = 2),
        plot.title = element_text(colour = "#C40C0C", family = "IBM Plex Sans", face = "bold", size = 30, hjust = 0.5, margin = margin(10,0,30,0)),
        plot.subtitle = element_textbox_simple(colour = "grey40", family = "IBM Plex Sans", size = 9, halign = 0.5, margin = margin(-10,-20,20,-20)),
        plot.caption = element_textbox_simple(colour = "#C40C0C", family = "IBM Plex Sans", size = 8, halign = 0.5, margin = margin(20,-60,0,-60)),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "none") 

ggsave("images/Week_44_wind_power.png", plot = last_plot(), width = 190, height = 190, units = "mm", dpi = 400) 
