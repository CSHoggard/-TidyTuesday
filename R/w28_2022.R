library(tidyverse)
library(janitor)
library(ggdist)
library(lubridate)
library(ggtext)
library(tidytext)
library(extrafont)
library(magick)
library(cowplot)
library(pdftools)

flights <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-12/flights.csv') %>%
  clean_names()

flights.summary <- flights  %>%
  select(flt_date, apt_name, state_name, flt_dep_1) %>%
  mutate(month = floor_date(flt_date, unit = "week")) %>%
  group_by(apt_name) %>%
  summarise(departures = sum(flt_dep_1),
            mean = mean(flt_dep_1),
            median = median(flt_dep_1),
            max = max(flt_dep_1),
            min = min(flt_dep_1)) %>%
  slice_max(departures, n = 15) %>%
  mutate(max_95 = max * 0.95) %>%
  rowid_to_column() %>%
  mutate(y = rowid - .3)

img <- image_read("https://upload.wikimedia.org/wikipedia/commons/thumb/3/32/BSicon_exFLUG.svg/500px-BSicon_exFLUG.svg.png")

flights %>%
  semi_join(flights.summary, by = "apt_name") %>%
  select(flt_date, apt_name, state_name, flt_dep_1) %>%
  mutate(week = floor_date(flt_date, unit = "week")) %>%
  select(-flt_date) %>%
  group_by(apt_name, state_name, week) %>%
  mutate(apt_name = factor(apt_name, levels = flights.summary$apt_name)) %>%
  mutate(y = as.numeric(apt_name) - .3) %>%
  ggplot(aes(flt_dep_1, y)) +
  stat_interval(
    aes(y = y - .1),
    orientation = "horizontal",
    .width = c(.25, .5, .95, 1),
    stroke = 0,
    size = 1.4
  ) +
  geom_text(data = flights.summary, 
            aes(label = toupper(apt_name),
                x = min,
                y = y + 0.27),
            colour = "#2a4971",
            size = 8,
            hjust = 0,
            family = "Asap Condensed Medium") +
  geom_dots(orientation = "horizontal",
            normalize = "none",
            scale = 0.75,
            colour = "#A6AAAF") + 
  labs(x = NULL,
       y = NULL,
       caption = "@CSHoggard | Data: Eurocontrol") +
  geom_point(
    data = flights.summary,
    aes(x = median, y = y - .3),
    shape = 17,
    colour = "#FFFFFF",
    size = 2
  ) +
  geom_point(
    data = flights.summary,
    aes(x = max, y = y - .3),
    shape = 17,
    colour = "#A6AAAF",
    size = 1.8
  ) +
  geom_text(
    data = flights.summary,
    aes(
      x = median, 
      y = y - .4,
      label = glue::glue("{round(median, 1)} flights") 
    ),
    color = "#FFFFFF",
    size = 2.5,
    vjust = 1,
    hjust = 0.5,
    family = "Asap Condensed"
  ) +
  geom_text(
    data = flights.summary,
    aes(
      x = max, 
      y = y - .4,
      label = glue::glue("{round(max, 1)} flights") 
    ),
    color = "#A6AAAF",
    size = 2.5,
    vjust = 1,
    hjust = 0.5,
    family = "Asap Condensed"
  ) +
  geom_text(
    aes(x = 900,
        y = -1,
        label = "900 flights"),
    color = "#A6AAAF",
    size = 3,
    family = "Asap Condensed"
  ) +
  geom_text(
    aes(x = 0,
        y = -1,
        label = "0 flights"),
    color = "#A6AAAF",
    size = 3,
    family = "Asap Condensed"
  ) +
  scale_color_manual(
    values = c("#203655", "#2a4971", "#355b8e", "#4577b8"),
    guide = F
  ) +
  geom_textbox(
    data = tibble(
      x = 750,
      y = 11.5,
      label = "<b style='font-size:16pt;'><b style='color:#CED0D3;'>Weekly Departures at Europe's Busiest Airports</b></b><br>
      <b style='color:#CED0D3;'>In this graphic each week (from 2016 to June 2022) is represented by individual dots<br>Bands:  <b style='color:#4577b8;'>25%</b>/ <b style='color:#355b8e;'>50%</b>/ <b style='color:#2a4971;'>75%</b>/ <b style='color:#203655;'>100%</b> confidence intervals<br><b style='color:#FFFFFF;'>White triangle</b>: Median number of weekly flights<br> <b style='color:#A6AAAF;'>Off-white triangle</b>: Maximum number of weekly flights</b>"
    ),
    aes(
      x = x, 
      y = y, 
      label = label
    ),
    inherit.aes = F,
    size = 2.5,
    lineheight = 1.5,
    width = unit(4.5, "inch"),
    halign = 0.5,
    vjust = 0,
    fill = NA,
    box.colour = NA,
    family = "Asap Condensed Medium"
  ) +
  draw_image(img, x = 0, y = -0.75, scale = 30) +
  draw_image(img, x = 900, y = -0.75, scale = 30) +
  theme_minimal() + 
  theme(text = element_text(family = "Asap Condensed Medium"),
        plot.background = element_rect(fill = "#152438"),
        legend.position = "none",
        plot.caption.position = 'plot',
        plot.title.position = 'plot',
        plot.caption = element_markdown(hjust = 0.5, colour = "#ced0d3"),
        plot.margin = margin(20,10,20,10),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        strip.text = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave("images/Week_28_Flights.pdf", plot = last_plot(), width = 10.5, height = 8.5, device = cairo_pdf)

pdf_convert(pdf = "images/Week_28_Flights.pdf",
            format = "png", dpi = 400)
