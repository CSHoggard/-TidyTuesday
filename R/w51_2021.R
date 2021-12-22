library(ggtext)
library(tidymodels)
library(tidyverse)
library(tidytext)
library(extrafont)
library(patchwork)
library(ggrepel)
library(here)

tracks <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-12-14/studio_album_tracks.csv')

tracks.clean <- tracks %>%
  select(track_name,
         album_name,
         danceability, 
         energy, 
         loudness, 
         speechiness, 
         acousticness, 
         instrumentalness, 
         liveness, 
         valence, 
         tempo, 
         duration_ms) %>%
  mutate(album_name = as.factor(album_name)) %>%
  rename("duration" = "duration_ms")

pca_prep <- recipe(~., data = tracks.clean) %>%
  update_role(track_name, album_name, new_role = "id") %>%
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors()) %>%
  prep()

tidied_pca <- tidy(pca_prep, 2)

a <- tidied_pca %>%
  filter(component %in% paste0("PC", 1:2)) %>%
  group_by(component) %>%
  top_n(8, abs(value)) %>%
  ungroup() %>%
  mutate(terms = reorder_within(terms, abs(value), component)) %>%
  ggplot(aes(abs(value), terms, fill = value > 0)) +
  geom_col() +
  facet_wrap(~component, scales = "free_y") +
  scale_y_reordered() +
  scale_fill_manual(values = c("grey30","grey80")) +
  labs(
    x = NULL,
    y = NULL,
    caption = "<span style='color:#4d4d4d;'>Dark: Positive</span> | <span style='color:#cccccc;'>Light: Negative</span>") +
  theme_minimal() +
  theme(text=element_text(family = "Quicksand"),
        legend.position = "none",
        legend.title = element_blank(),
        plot.caption = element_markdown(size = 10, face = "bold", hjust = 0.3),
        axis.text.x=element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank())

b <- juice(pca_prep) %>%
  ggplot(aes(PC1, PC2, label = track_name)) +
  geom_point(aes(colour = album_name), alpha = 0.7, size = 4) +
  labs(x = "Principal Component 1 (PC1)",
       y = "Principal Component 2 (PC2)",
       title = "Quantifying the Spice Girls Discography",
       subtitle = "Deconstructing the Musical Attributes of the <span style='color:#EE433B;'><b>Forever</b></span>, <span style='color:#A1DA09;'><b>Spice</b></span> and <span style='color:#EEA9C5;'><b>Spiceworld</b></span> albums",
       caption = "@CSHoggard | #TidyTuesday Week 51 | Data: Spice Girls by Jacquie Tran") +
  geom_text_repel(family = "Quicksand", size = 4, box.padding = 0.5, min.segment.length = Inf) +
  scale_colour_manual(values = c("#EE433B","#A1DA09","#EEA9C5")) +
  annotate(
    geom = "segment", x = -3, xend = 4, y = 0, yend = 0,
    col = "grey70",
    lty = "dashed",
    size=0.5, 
    lineend='round', 
    linejoin='round'
  ) +
  annotate(
    geom = "segment", x = 0, xend = 0, y = -3, yend = 4.5,
    col = "grey70",
    lty = "dashed",
    size=0.5, 
    lineend='round', 
    linejoin='round'
  ) + 
  annotate("text", x = 3.25, y = -2.45,  size = 4, fontface = "bold", family = "Quicksand", label = "Less energetic and less danceable") +
  geom_segment(aes(x = 3.25, xend = 3.25, y = -2.2, yend = -1.9), arrow = arrow(length = unit(0.01, "npc"))) +
  annotate("text", x = -2.5, y = 1.05,  size = 4, fontface = "bold", family = "Quicksand", label = "More energetic and more danceable") +
  geom_segment(aes(x = -2.5, xend = -2.5, y = 0.8, yend = 0.5), arrow = arrow(length = unit(0.01, "npc"))) +
  theme_minimal() +
  theme(text=element_text(family = "Quicksand"),
        plot.margin = margin(10,10,10,10),
        legend.position = "none",        
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_markdown(size = 32, face = "bold"),
        plot.subtitle = element_markdown(size = 14),
        plot.caption = element_markdown(size = 11, colour = "grey40"))

b + inset_element(a, left = 0.6, bottom = 0.75, right = 1, top = 1)

ggsave("images/Week_51_Spice.png", plot = last_plot(), dpi = 400, height = 230, width = 390, units = "mm")
