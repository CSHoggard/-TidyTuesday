library(tidyverse)
library(extrafont)
library(ggtext)
library(ggraph)
library(tidygraph)
library(widyr)

ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-25/ratings.csv')
details <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-25/details.csv')

board.games <- details %>%
  left_join(ratings, by = "id") %>%
  top_n(50, owned) %>%
  rownames_to_column() %>%
  mutate(top_seller = ifelse(owned > 100000, "Top Seller", "Not Top Seller"))

board.games.clean <- board.games %>%
  select(rowname, primary, owned, boardgamecategory) %>%
  rename("category" = "boardgamecategory") %>%
  rename("name" = "primary") %>%
  rename("id" = "rowname") %>%
  mutate(category = str_replace_all(category, "\\[|\\]", "")) %>%
  mutate(category = str_remove_all(category, "\'")) %>%
  separate_rows(category, sep = ",") %>%
  mutate(category = str_squish(category)) 

board.games.sim <- board.games.clean %>%
  mutate(n = 1) %>%
  pairwise_similarity(name, category, n, upper = F, sort = T) 

board.games.tbl <- 
  board.games.sim %>%
  as_tbl_graph() %>%
  activate(nodes) %>%
  left_join(board.games, by = c("name" = "name"))
  
set.seed(2345) 
layout <- create_layout(board.games.tbl, layout = 'igraph', algorithm = 'nicely')

ggraph(layout) +
  scale_size_continuous(range = c(2, 9)) +
  geom_edge_link(color = "grey40", alpha = 0.1, edge_width = 0.2) +
  geom_node_point(aes(size = owned, colour = top_seller), alpha = 0.8) +
  scale_colour_manual(values = c("#66b3d4", "#ebae46")) +
  annotate(geom = "richtext", 
           x = 1.4, y = 6.5, 
           label = "How different are the best Tabletop Games?",
           family = "Alegreya",
           colour = "grey20",
           size = 10,
           label.colour = NA,
           fill = NA) +
  annotate(geom = "richtext", 
           x = -1.83, y = 5.35, 
           label = "The BoardGameGeek (BGG) forum contains over <b>125,000 different tabletop games</b>.<br>Here, the <b><span style='color:#66b3d4;'>top owned games</span></b> are visualised according to their respective categories.<br> The <span style='font-size:14pt'<b>size</b></span> of the point indicates a greater number of people own that particular game, <br>while the number of branches indicates their degree of interconnectivity.<br><br>Interestingly, the <span style='color:#ebae46;'><b>top 10 owned tabletop games</b></span> are pretty different in their categories.",
           family = "Alegreya",
           colour = "grey20",
           hjust = 0,
           size = 4,
           label.colour = NA,
           fill = NA) +
  annotate(geom = "richtext",
           x = 7.6, y = 7.45,
           label = "<span style='color:#ebae46;'>Ticket To Ride</span> <span style='color:#66b3d4;'>(& Europe Edition)</span>",
           family = "Alegreya Sans",
           colour = "grey30",
           label.colour = NA,
           size = 4,
           fill = NA) +
  annotate(geom = "richtext",
           x = 7.85, y = -0.37,
           label = "Scrabble",
           family = "Alegreya Sans",
           colour = "grey30",
           label.colour = NA,
           size = 4,
           fill = NA) +
  annotate(geom = "richtext",
           x = 6, y = -3.62,
           label = "Patchwork",
           family = "Alegreya Sans",
           colour = "grey30",
           label.colour = NA,
           size = 4,
           fill = NA) +
  annotate(geom = "richtext",
           x = 5.97, y = -2,
           label = "Azul",
           family = "Alegreya Sans",
           colour = "grey30",
           label.colour = NA,
           size = 4,
           fill = NA) +
  annotate(geom = "richtext",
           x = 3.2, y = -1,
           label = "Catan",
           family = "Alegreya Sans",
           colour = "grey30",
           label.colour = NA,
           size = 4,
           fill = NA) +
  annotate(geom = "richtext",
           x = -1.05, y = 0.6,
           label = "Pandemic",
           family = "Alegreya Sans",
           colour = "grey30",
           label.colour = NA,
           size = 4,
           fill = NA) +
  annotate(geom = "richtext",
           x = 1.15, y = -0.01,
           label = "Pandemic Legacy: Season 1",
           family = "Alegreya Sans",
           colour = "grey30",
           label.colour = NA,
           size = 4,
           fill = NA) +
  annotate(geom = "richtext",
           x = 1.64, y = 1.2,
           label = "Carcassonne",
           family = "Alegreya Sans",
           colour = "grey30",
           label.colour = NA,
           size = 4,
           fill = NA) +
  annotate(geom = "richtext",
           x = 5.7, y = 3.2,
           label = "Betrayal at House on the Hill",
           family = "Alegreya Sans",
           colour = "grey30",
           label.colour = NA,
           size = 4,
           fill = NA) +
  annotate(geom = "richtext",
           x = 6.07, y = 0.75,
           label = "Codenames",
           family = "Alegreya Sans",
           colour = "grey30",
           label.colour = NA,
           size = 4,
           fill = NA) +
  labs(caption = "@CSHoggard  •   Source: Kaggle and BoardGameGeek") +
  theme(text = element_text(family = "Alegreya"),
        plot.margin = margin(10,10,10,10),
        plot.background = element_rect(fill = "grey98", colour = "grey98"),
        panel.background = element_rect(fill = "grey98", colour = "grey98"),
        legend.position = "none",
        plot.caption = element_text(hjust = 0.5, size = 11, colour = "grey30", family = "Alegreya Sans"))

ggsave(filename = "Week_4_Board_Games.png", plot = last_plot(), dpi = 400)
