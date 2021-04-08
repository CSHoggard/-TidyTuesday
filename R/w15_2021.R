library(tidyverse)
library(extrafont)
library(ggtext)
library(scales)
library(here)
library(glue)

devtools::install_github("cshoggard/morris")

theme_set(theme_minimal(base_family = "Roboto"))

theme_update(plot.caption = element_text(color = "grey60", size = 10, margin = margin(20,0,-10,0)),
             plot.margin = margin(20,20,20,20),
             axis.title.x = element_text(size = 10, colour = "grey40", margin = margin(10,0,0,0)),
             axis.title.y = element_blank(),
             axis.text.y = element_markdown(),
             panel.grid.minor = element_blank(),
             legend.position = "none")

forest <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-06/forest.csv')

forest_clean <- forest %>%
  filter(entity %in% c("Austria", 
                       "Bulgaria", 
                       "Denmark", 
                       "Estonia", 
                       "Finland", 
                       "Germany",
                       "Hungary", 
                       "Iceland", 
                       "Ireland", 
                       "Italy", 
                       "Lithuania", 
                       "Netherlands", 
                       "Norway", 
                       "Poland", 
                       "Portugal", 
                       "Russia", 
                       "Slovenia", 
                       "Spain", 
                       "Sweden", 
                       "Switzerland", 
                       "United Kingdom")) %>%
  group_by(entity) %>%
  mutate(
    first = first(net_forest_conversion),
    last = last(net_forest_conversion),
    change = (last - first),
    gl = if_else(change > 0, "Positive", "Negative")) %>%
  select(-code, -year, -net_forest_conversion, -first, -last) %>%
  filter(!change == 0) %>%
  distinct() %>%
  mutate(entity = fct_reorder(toupper(entity), change)) %>%
  mutate(
    entity = case_when(
      entity %in% "NORWAY" ~ glue("<b style='color:#3A571F;'>{entity}</b>"),
      entity %in% "UNITED KINGDOM" ~ glue("<b style='color:#3A571F;'>{entity}</b>"),
      entity %in% "BULGARIA" ~ glue("<b style='color:#3A571F;'>{entity}</b>"),
      TRUE ~ glue("<b style='color:#70ACAB;'>{entity}</b>")
    ))

ggplot(forest_clean, aes(reorder(entity, change), -change, fill = gl)) +
         geom_col() +
  coord_flip() +
  labs(y = "Absolute Change in Forest Cover (Hectares)",
       caption = "@CSHoggard • TidyTuesday 15 • Data: Our World In Data") + 
  scale_fill_manual(values = peacock_palette) +
  scale_y_continuous(labels = comma) +
  geom_richtext(
    aes(x = 13, y = 300000,
        label = "<b style='font-size:18pt;'>Which European countries are <span style='color:#70ACAB;'>losing</span> their forests?</br><br><br><span style='font-size:12pt;'><span style='color:#70ACAB;'>Most of them.</span><br><br>Only <span style='color:#3A571F;'>Norway</span>, <span style='color:#3A571F;'>Bulgaria</span> and the <span style='color:#3A571F;'>United Kingdom</span> have seen increases in forest cover.</br>"), 
    inherit.aes = FALSE,
    color = "grey60",
    family = "Roboto",
    size = 6,
    label.padding = unit(2, "lines"),
    label.r = unit(0, "lines"),
    label.size = 2,
    label.color = "grey60"
  )

ggsave("images/Week_15_Deforestation.png", plot = last_plot(), width = 200, height = 400, units = "mm", dpi = 400)

