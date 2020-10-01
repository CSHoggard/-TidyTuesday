library(tidyverse)
library(tidytext)
library(tidymodels)
library(textrecipes)
library(themis)
library(stopwords)
library(SnowballC)
library(scales)
library(cowplot)
library(extrafont)
library(magick)
library(patchwork)
library(here)

beyonce_lyrics <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/beyonce_lyrics.csv')
taylor_swift_lyrics <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/taylor_swift_lyrics.csv')

swift <- image_read("https://seeklogo.com/images/T/taylor-swift-silhouette-logo-B5FDD83450-seeklogo.com.png")
swift <- image_colorize(swift, 100, "#4898D2")

beyonce <- image_read("https://creazilla-store.fra1.digitaloceanspaces.com/silhouettes/2886/beyonce-silhouette-f44545-md.png")
beyonce <- image_flop(beyonce)
beyonce <- image_colorize(beyonce, 100, "#F18099")

t.lyrics <- taylor_swift_lyrics %>%
  select(artist = Artist, lyrics = Lyrics) %>%
  unnest_tokens(word, lyrics) %>%
  filter(!(word %in% stopwords(source = "stopwords-iso")))

b.lyrics <- beyonce_lyrics %>%
  select(artist = artist_name, lyrics = line) %>%
  unnest_tokens(word, lyrics) %>%
  filter(!(word %in% stopwords(source = "stopwords-iso")))

all.lyrics <- bind_rows(b.lyrics, t.lyrics)

clean.lyrics <- all.lyrics[-grep("\\b\\d+\\b", all.lyrics$word),]

p1 <- clean.lyrics %>% 
  count(word, artist, sort = TRUE) %>%
  group_by(artist) %>%
  top_n(5) %>%
  ungroup() %>%
  ggplot(aes(reorder_within(word, n, artist), n,
             fill = artist)) +
  geom_col(alpha = 0.7,
           show.legend = FALSE) +
  scale_fill_manual(values = c("#F18099", "#4898D2")) +
  coord_flip() +
  scale_x_reordered() +
  facet_wrap(~artist, 
             scales = "free") +
  labs(title = "INVESTIGATING...\n523 BEYONCÉ AND TAYLOR SWIFT SONGS",
       subtitle = "WHAT WORDS WERE USED MOST OFTEN?") +
  geom_text(aes(
    label = n,
    hjust = 1.2,
    family = "Noto Sans"),
    size = 5,
    color = "white") +
  theme_minimal() +
  theme(plot.margin = margin(20,20,30,15),
        plot.background = element_rect(fill = "#F8F3F2", color = NA),
        plot.title = element_text(size = 32, family = "Source Sans Pro", face = "bold", colour = "#8E4F9A", hjust = 0.5, margin = margin(0,0,40,0)),
        plot.subtitle = element_text(size = 16, family = "Noto Sans", face = "bold", colour = "#8F7E84", hjust = 0.5, margin = margin(0,0,20,0)),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 12, family = "Noto Sans", face = "bold", colour = "#8F7E84", margin = margin(0,-5,0,0)),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        strip.text.x = element_text(size = 12, family = "Noto Sans", face = "bold", colour = "#8F7E84"))


p2 <- clean.lyrics %>% 
  count(word, artist, sort = TRUE) %>%
  pivot_wider(names_from = artist, values_from = n) %>%
  mutate(logratio = log(Beyoncé/`Taylor Swift`)) %>%
  arrange(desc(logratio)) %>%
  group_by(logratio < 0) %>%
  top_n(15, abs(logratio)) %>%
  ungroup() %>%
  mutate(word = reorder(word, logratio)) %>%
  ggplot(aes(word, logratio, fill = logratio < 0)) +
  geom_col(width=0.8, show.legend = FALSE) +
  labs(subtitle = "WHO IS MORE LIKELY TO SAY...",
       caption = "@CSHoggard  •  Data: Rosie Baillie and Dr. Sara Stoudt  •  #TidyTuesday Week 40") +
  scale_fill_manual(values = c("#F18099", "#4898D2")) +
  coord_flip() +
  ylim(-6,6) +
  theme_minimal() +
  theme(plot.margin = margin(10, 80, 20, 60),
        plot.background = element_rect(fill = "#F8F3F2", color = NA),
        plot.subtitle = element_text(size = 18, family = "Noto Sans", face = "bold", colour = "#8F7E84", hjust = 0.5, margin = margin(0, 0, 20, 0)),
        plot.caption = element_text(size = 10, family = "Noto Sans", colour = "#8F7E84", hjust = 0.5, margin = margin(40, 0, 5, 0)),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 10, family = "Noto Sans", face = "bold", colour = "#8F7E84"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()
  ) +
  draw_image(swift, scale=13, y=3, x=8.5) + 
  draw_image(beyonce, scale=13, y=-4, x=27)

(p1 / p2) + 
  plot_layout(heights = c(0.6,2))

ggsave("images/Week_40_Lyrics.png", plot = last_plot(), dpi = 400, height = 320, width = 240, units = "mm")  

### Machine Learning bonus!

set.seed(123)
artist_split <- initial_split(clean.lyrics, strata = artist)
artist_train <- training(artist_split)
artist_test <- testing(artist_split)

set.seed(234)
artist_folds <- vfold_cv(artist_train, strata = artist)
artist_folds

artist_recipe <- recipe(artist ~ word, data = artist_train) %>%
  step_downsample(artist) %>%
  step_textfeature(word) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_predictors())

artist_prep <- prep(artist_recipe)
artist_prep

juice(artist_prep)

rf_spec <- rand_forest(trees = 1000) %>%
  set_engine("ranger") %>%
  set_mode("classification")

svm_spec <- svm_rbf(cost = 0.5) %>%
  set_engine("kernlab") %>%
  set_mode("classification")

artist_wf <- workflow() %>%
  add_recipe(artist_recipe)

set.seed(1234)

rf_rs <- artist_wf %>%
  add_model(rf_spec) %>%
  fit_resamples(
    resamples = artist_folds,
    metrics = metric_set(roc_auc, accuracy, sens, spec),
    control = control_grid(save_pred = TRUE))

svm_rs <- artist_wf %>%
  add_model(svm_spec) %>%
  fit_resamples(
    resamples = artist_folds,
    metrics = metric_set(roc_auc, accuracy, sens, spec),
    control = control_grid(save_pred = TRUE))

collect_metrics(rf_rs)
conf_mat_resampled(rf_rs)
collect_metrics(svm_rs)
conf_mat_resampled(svm_rs)

artist_final <- artist_wf %>%
  add_model(svm_spec) %>%
  last_fit(artist_split)

