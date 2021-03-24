library(extrafont)
library(ggtext)
library(ggstream)
library(here)
library(lubridate)
library(tidyverse)

games <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-16/games.csv')

games.clean <- games %>%
  filter(str_detect(gamename, "DARK SOULS")) %>% 
  mutate(
    gamename = case_when(
      gamename == "DARK SOULS\u0099: REMASTERED" ~ "Dark Souls I",
      gamename == "DARK SOULS\u0099 II" ~ "Dark Souls II",
      gamename == "DARK SOULS\u0099 II: Scholar of the First Sin" ~ "Dark Souls II: Scholar of the First Sin",
      gamename == "DARK SOULS\u0099 III" ~ "Dark Souls III",
      TRUE ~ gamename)
  ) %>%
  mutate(ym = lubridate::ymd(paste0(year, month, 1))) %>%
  select(gamename, ym, avg)

ggplot(games.clean, aes(ym,avg, fill = gamename)) + 
  geom_stream(bw = 0.45) +
  labs(title = "Popularity of the Dark Souls Franchise",
       subtitle = "Data for <span style = 'color:#214A4C;'>Dark Souls II</span>, <span style = 'color:#9E7C22;'>Scholar of the First Sin DLC</span>, <span style = 'color:#842323;'>Dark Souls III</span> and <span style = 'color:#498198;'>Dark Souls I (Remastered)</span>",
       caption = "<span style = 'color:#D1D1D1;'>@CSHoggard</span>  |  Data: Steam  |  <span style = 'color:#D1D1D1;'>#TidyTuesday Week 12</span>") +
  scale_x_date(date_labels = "%Y",
               limits=c(ymd("2014-01-01", ymd("2021-02-01"))),
               breaks=c(ymd("2014-01-01"),ymd("2016-01-01"),ymd("2018-01-01"),ymd("2020-01-01"),ymd("2021-01-01"))) +
  scale_fill_manual(values = c("#498198", "#214A4C", "#9E7C22", "#842323")) +
  theme_void() +
  theme(
    plot.margin = margin(50,50,50,50),
    plot.background = element_rect(colour = "#0C0C0C", fill = "#0C0C0C"),
    legend.position = "none",
    plot.title = element_text(size = 42, colour = "#D1D1D1", family = "EB Garamond", margin = margin(0,0,10,0)),
    plot.subtitle = element_textbox_simple(family = "Lato", colour = "#A1A1A1", size = 16, margin = margin(0,0,30,0)),
    plot.caption = element_textbox_simple(family = "Lato", size = 12, colour = "#A1A1A1", halign = 0.5, margin = margin(50,0,0,0)),
    axis.text.x = element_text(size = 11, family = "Lato", vjust = -0.5, colour = "#A1A1A1"),
    panel.grid.major.x = element_line(size = 0.5, color = "#212121", linetype = 3)
    )

ggsave("images/Week_12_Games.png", plot = last_plot(), dpi = 350)