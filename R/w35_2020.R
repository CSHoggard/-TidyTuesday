library(tidyverse)
library(extrafont)
library(here)
library(tidytext)
library(magick)

chopped <- read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-25/chopped.tsv')

data("stop_words")

unique_sw <- data.frame(word = c("ice",
                              "dried",
                              "chips",
                              "white",
                              "black",
                              "red",
                              "sweet",
                              "paste",
                              "covered",
                              "baby",
                              "bars",
                              "frozen",
                              "green",
                              "blood",
                              "orange",
                              "mix",
                              "hot",
                              "potato",
                              "spread")) # unique stop words

chopped.clean <- chopped %>% 
  select(dessert, season) %>%
  unnest_tokens(word,dessert) %>%
  mutate(cut(season, breaks=c(-Inf, 10, 20, 30, 40, 45), labels=c("Seasons 1-10", "Seasons 11-20", "Seasons 21-30", "Seasons 31-40", "Seasons 41-45"))) %>%
  rename(category = `cut(...)`) %>%
  mutate(
    word = case_when(
      word == "strawberries" ~ "strawberry",
      word == "amaretti" ~ "amaretto",
      word == "almonds" ~ "almond",
      word == "apples" ~ "apple",
      word == "apricots" ~ "apricot",
      word == "avocadoes" ~ "avocado",
      word == "avocados" ~ "avocado",
      word == "bagels" ~ "bagel",
      word == "bananas" ~ "banana",
      word == "bean" ~ "beans",
      word == "beet" ~ "beets",
      word == "biscuits" ~ "biscuit",
      word == "blossoms" ~ "blossom",
      word == "blueberries" ~ "blueberry",
      word == "candies" ~ "candy",
      word == "carrots" ~ "carrot",
      word == "cashews" ~ "cashew",
      word == "cherries" ~ "cherry",
      word == "chestnuts" ~ "chestnut",
      word == "chickpea" ~ "chickpeas",
      word == "chocolates" ~ "chocolate",
      word == "coconuts" ~ "coconut",
      word == "cookies" ~ "cookie",
      word == "cracker" ~ "crackers",
      word == "cranberries" ~ "cranberry",
      word == "fig" ~ "figs",
      word == "lime" ~ "limes",
      word == "marshmallow" ~ "marshmallows",
      word == "pineapples" ~ "pineapple",
      word == "potatoes" ~ "potato",
      TRUE ~ word)) %>%
  count(word, category, sort = TRUE) %>%
  anti_join(stop_words) %>%
  anti_join(unique_sw) %>%
  arrange(desc(n)) %>%
  group_by(category) %>%
  filter(row_number() <= 5) %>%
  ungroup()

logo_raw <- image_read("R/chopped.png")
logo <- as.raster(logo_raw)

ggplot(chopped.clean, aes(n, reorder(word, n))) +
  facet_wrap(~category, scales="free", nrow = 1) +
  geom_col(fill = "#F75821", width = 0.75) + 
  xlim(0, 40) +
  labs(title = "What Makes A Good Dessert?",
       subtitle = "The most common ingredients in meals in the Chopped TV Series", 
       caption = "@CSHoggard | Data from Kaggle | #TidyTuesday Week 35",
       x = "Number of dishes",
       y = "") +
  theme_minimal() +
  theme(plot.margin = unit(c(1, 1, 0.5, 1), "cm"),
        text = element_text(family = "Open Sans"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 24, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12, face = "bold", colour = "#F75821", margin = margin(3, 2, 7, 2)),
        plot.caption = element_text(size = 9, colour = "grey40"),
        strip.text.x = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 8, colour = "grey40"),
        axis.title.x = element_text(size = 9, colour = "grey50", margin = margin(10, 0, 5, 0)))
grid.raster(logo, x = 0.5, y = 0.5)
