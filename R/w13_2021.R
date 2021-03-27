library(tidyverse)
library(extrafont)
library(here)

issues <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-23/issues.csv')

palette <- c("#E6E045", "#C43675", "#FE4D15", "#814EAB", "#AEE464", "#CA1D31")

count <- issues %>%
  count(issue) %>%
  mutate(
    issue = case_when(
      issue == "Arms control and disarmament" ~ "Arms & Disarmament",
      issue == "Economic development" ~ "Economic Development",
      issue == "Human rights" ~ "Human Rights",
      issue == "Palestinian conflict" ~ "The Palestinian Conflict",
      issue == "Nuclear weapons and nuclear material" ~ "Nuclear Weapons & Material",
      TRUE ~ as.character(issue)
      )
    ) %>%
  mutate(percentage = paste0(round(n / sum(n),4) * 100, "%"))

ggplot(count, aes(issue, n, fill = issue)) +
  geom_col() +
  labs(title = "UN roll call votes by issue",
       caption = "Source: Harvard Datverse | @CSHoggard") +
  geom_text(aes(label = n, y = 500), size = 26, family = "Roboto Black") +
  geom_text(aes(label = issue, y = -40), size = 5, family = "Roboto Black", colour = palette) +
  scale_fill_manual(values = palette) +
  theme_void() +
  theme(plot.margin  = margin(30,30,30,30),
        plot.background = element_rect(fill = "#FAF6F3", colour = NA),
        plot.title = element_text(size = 48, margin = margin(0, 0, 100, 0), family = "Roboto Black"),
        plot.caption = element_text(size = 11, margin = margin(20, 0, -20, 0), family = "Roboto", colour = "#B5B3B0", face = "bold"),
        legend.position = "none")

ggsave("images/Week_13_UN.png", plot = last_plot(), width = 450, height = 250, units = "mm", dpi = 400)
