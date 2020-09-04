library(tidyverse)
library(tidymodels)
library(ggrepel)
library(extrafont)
library(gridExtra)
library(cowplot)
library(here)

key_crop_yields <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/key_crop_yields.csv')

yields_parsed <- key_crop_yields %>%
  filter(!Entity %in% c("Africa", "Americas", "Small island developing States", 
  "USSR", "Pacific Islands Trust Territory", "Net Food Importing Developing Countries", 
  "Northern Europe", "Northern America", "Northern Africa", "Oceania", "South Africa", "South America", 
  "South Eastern Asia", "Europe", "Europe, Western", "European Union", "Ethiopia PDR", 
  "Eastern Africa", "Middle Africa", "Eastern Asia", "Eastern Europe", "Czechoslovakia", "Central America", 
  "Caribbean", "Asia", "Asia, Central", "Australia & New Zealand", "Belgium-Luxembourg", 
  "Southern Africa", "Southern Asia", "Southern Europe", "Western Africa", "Western Asia", 
  "Western Sahara", "World", "Polynesia", "Melanesia", "Micronesia (country)", "Low Income Food Deficit Countries", 
  "Yugoslavia", "Sudan (former)", "Least Developed Countries", "Serbia and Montenegro", "Land Locked Developing Countries")) %>% 
  mutate(
    Entity = case_when(
    Entity == "Micronesia (region)" ~ "Micronesia",
    TRUE ~ Entity),
    Code = replace(Code, which(is.na(Code) & Entity == "Micronesia"), "MCR")) %>%
  rename(country = Entity,
         code = Code,
         wheat = `Wheat (tonnes per hectare)`,
         rice = `Rice (tonnes per hectare)`,
         maize = `Maize (tonnes per hectare)`,
         soybeans = `Soybeans (tonnes per hectare)`,
         potatoes = `Potatoes (tonnes per hectare)`,
         beans = `Beans (tonnes per hectare)`,
         peas = `Peas (tonnes per hectare)`,
         cassava = `Cassava (tonnes per hectare)`,
         barley = `Barley (tonnes per hectare)`,
         cocoa = `Cocoa beans (tonnes per hectare)`,
         bananas = `Bananas (tonnes per hectare)`) %>%
  select(-Year) %>%
  group_by(country, code) %>%
  summarise_all(funs(sum)) %>%
  mutate_at(vars(wheat,
                 rice,
                 maize,
                 soybeans,
                 potatoes,
                 beans,
                 peas,
                 cassava,
                 barley,
                 cocoa,
                 bananas),
            ~replace_na(., 0))

pca_rec <- recipe(~., data = yields_parsed) %>%
  update_role(country, code, new_role = "id") %>%
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors())

pca_prep <- prep(pca_rec)

pca_prep

tidied_pca <- tidy(pca_prep, 2)

base <- tidied_pca %>%
  filter(component %in% paste0("PC", 1:5)) %>%
  mutate(component = fct_inorder(component)) %>%
  ggplot(aes(value, terms, fill = terms)) +
  geom_col(show.legend = FALSE, fill = "#ABDF75") +
  facet_wrap(~component, nrow = 1) +
  labs(x = "Score",
       y = NULL,
       title = "Inertia",
       caption = "@CSHoggard | Source: Our World in Data | #TidyTuesday Week 36") + 
  theme_minimal() +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
        text = element_text(family = "IBM Plex Sans"),
        plot.title = element_text(size = 12, face = "bold"),
        plot.caption = element_text(size = 9, margin = margin(20, 0, 0, 0)))

top <- juice(pca_prep) %>%
  ggplot(aes(PC1, PC2, label = country)) +
  geom_point(alpha = 0.8, size = 2.5, colour = "#ABDF75") +
  lims(x = c(-9,9), y = c(-4,3.5)) +
  geom_text(check_overlap = TRUE,
            size = 3, 
            hjust = "outward", 
            family = "IBM Plex Sans") +
  labs(title = "Global Crop Varieties and Foci (1961-2018)",
       subtitle = "Data: UN Food and Agricultural Organization",
       x = "First Principal Component (PC1)",
       y = "Second Principal Component (PC2)") + 
  annotate("text", x = 7.5, y = 2.15, size = 3, fontface = "bold", family = "IBM Plex Sans", label = "What Are Principal Components?") +
  annotate("text", x = 7.5, y = 1.5, size = 3, family = "IBM Plex Sans", label = "These axes visualise the best way to \n describe variation within a dataset \n through lineal combinations of \n the initial variables (crops).") +
  theme_minimal() +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
        text = element_text(family = "IBM Plex Sans"),
        axis.title.x = element_text(size = 9, margin = margin(10, 0, 5, 0)),
        axis.title.y = element_text(size = 9, margin = margin(0, 10, 0, 5)),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 12, face = "bold", colour = "#ABDF75", margin = margin(2, 0, 2, 0)))

plot_grid(top, base, nrow = 2, rel_heights = c(2/3, 1/3))
ggsave("images/Week_36_Crops.png", plot = last_plot(), dpi = 400, height = 250, width = 220, units = "mm")
