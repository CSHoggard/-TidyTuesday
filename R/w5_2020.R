library(tidyverse)
library(treemap)
library(treemapify)
library(ggtext)
library(extrafont)
library(here)

plastics <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-26/plastics.csv')

plastics.parsed <- plastics %>%
  filter(country == "United Kingdom" | country == "United Kingdom of Great Britain & Northern Ireland",
         !parent_company == "null",
         !parent_company == "Unbranded",
         !parent_company == "SociâˆšÂ©tâˆšÂ© Bic") %>%
  filter(year == "2020") %>%
  mutate(country = case_when(
    country == "United Kingdom" ~ "United Kingdom of Great Britain & Northern Ireland",
    TRUE ~ country
  )) %>%
  mutate(parent_company = case_when(
    parent_company == "Floralys (Lidl)" ~ "Lidl",
    parent_company == "Costco Wholesale UK Ltd" ~ "Costco",
    parent_company == "Mars, Incorporated" ~ "Mars",
    parent_company == "frontier medical group" ~ "Frontier Medical Group",
    parent_company == "Kp Snacks / Intersnack" ~ "KP Foods Ltd",
    TRUE ~ parent_company
  )) %>%
  mutate(parent_company = case_when(
    grand_total < 2 ~ "Other Companies",
    TRUE ~ parent_company)) %>%
  group_by(parent_company) %>%
  summarise_each(funs(sum), 4:11)

ggplot(plastics.parsed, aes(area = grand_total, label = parent_company)) +
  geom_treemap(fill = "#f9f9f9") +
  geom_treemap_text(family = "Roboto",
                    colour = "#668995", 
                    place = "centre",
                    reflow = FALSE,
                    grow = TRUE) +
  labs(title = "<span style = 'color:#254554;'>Break Free from Plastic:</span><span style = 'color:#668995;'> <br>Cleaning up the United Kingdom</span> ",
       subtitle = "<span style = 'color:#254554;'>The <span style = 'color:#668995;'>#breakfreefromplastic </span> movement envisions a future free from plastic pollution. <br><br> Since its launch in 2016, more than 11,000 organizations and individual supporters from across the world have joined<br> the movement to demand massive reductions in single-use plastics and to push for lasting solutions to the plastic pollution crisis. <br><br> <span style = 'color:#668995;'>In this graphic, eight clean-up audits during 2020 by 90 volunteers in the UK are highlighted.<br><br> This graphic does not include 4117 unbranded instances.</span>",
       caption = "<span style = 'color:#668995;'>@CSHoggard</span>  |  Data: Break Free from Plastic  |  <span style = 'color:#668995;'>#TidyTuesday Week 5 (2021)</span>") + 
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.margin = margin(30,30,30,30),
    plot.title = element_textbox_simple(family = "Roboto", size  = 42, margin = margin(15,0,30,0)),
    plot.subtitle = element_textbox_simple(family = "Roboto Light", size = 12, margin = margin(15,0,30,0)),
    plot.caption = element_textbox_simple(family = "Roboto Light", size = 12, halign = 0.5, margin = margin(15,0,0,0)))

ggsave("images/Week_5_Plastics.png", plot = last_plot(), dpi = 400) 
  
