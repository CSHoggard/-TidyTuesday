library(tidyverse)
library(extrafont)
library(ggtext)
library(glue)
library(here)

ikea <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-03/ikea.csv')

ikea.clean <- ikea %>%
  mutate(designer = strsplit(as.character(designer), "/")) %>% 
  unnest(designer) %>% 
  filter(!designer %in% "IKEA of Sweden") %>%
  filter(!str_detect(designer, "\\d")) %>%
  filter(!str_detect(designer, "cabinets")) %>%
  filter(!str_detect(designer, "glass")) %>%
  filter(!str_detect(designer, "handles")) %>%
  mutate(
    designer = case_when(
      designer == "Andreas Fredriksson" ~ "A Fredriksson",
      designer == "Ebba Strandmark" ~ "E Strandmark",
      designer == "Anna Efverlund" ~ "A Efverlund",
      designer == "Anna Palleschitz" ~ "A Palleschitz",
      designer == "Annie Huldén" ~ "A Huldén",
      designer == "Carina Bengs" ~ "C Bengs",
      designer == "Carl Öjerstam" ~ "C Öjerstam",
      designer == "Charlie Styrbjörn" ~ "C Styrbjörn",
      designer == "Chenyi Ke" ~ "C Ke",
      designer == "Chris Martin" ~ "C Martin",
      designer == "David Wahl" ~ "D Wahl",
      designer == "E Lilja Löwenhielm" ~ "E L Löwenhielm",
      designer == "Ehlén Johansson" ~ "E Johansson",
      designer == "Elizabet Gutierrez" ~ "E Gutierrez",
      designer == "Eva Lilja Löwenhielm" ~ "E L Löwenhielm",
      designer == "Eva Schildt" ~ "E Schildt",
      designer == "Francis Cayouette" ~ "F Cayouette",
      designer == "Fredriksson" ~ "A Fredriksson",
      designer == "Gillis Lundgren" ~ "G Lundgren",
      designer == "Gustav Carlberg" ~ "G Carlberg",
      designer == "Henrik Preutz" ~ "H Preutz",
      designer == "Johan Kroon" ~ "J Kroon",
      designer == "Johanna Asshoff" ~ "J Asshoff",
      designer == "Jomi Evers" ~ "J Evers",
      designer == "Jon Karlsson" ~ "J Karlsson",
      designer == "Johanna Jelinek" ~ "J Jelinek",
      designer == "Jonas Hultqvist" ~ "J Hultqvist",
      designer == "Jonas" ~ "J Hultqvist",
      designer == "Jooyeon Lee" ~ "J Lee",
      designer == "Karl Malmvall" ~ "K Malmvall",
      designer == "Lars Norinder" ~ "La Norinder",
      designer == "Lisa Hilland" ~ "L Hilland",
      designer == "Hilland" ~ "L Hilland",
      designer == "Lisa Norinder" ~ "Li Norinder",
      designer == "Lisel Garsveden" ~ "L Garsveden",
      designer == "Lycke von Schantz" ~ "L von Schantz",
      designer == "Magnus Elebäck" ~ "M Elebäck",
      designer == "Maja Ganszyniec" ~ "M Ganszyniec",
      designer == "Malin Unnborn" ~ "M Unnborn",
      designer == "Marcus Arvonen" ~ "M Arvonen",
      designer == "Maria Vinka" ~ "M Vinka",
      designer == "Mia Lagerman" ~ "M Lagerman",
      designer == "Mikael Axelsson" ~ "M Axelsson",
      designer == "Mikael Warnhammar" ~ "M Warnhammer",
      designer == "Monika Mulder" ~ "M Mulder",
      designer == "Nada Debs" ~ "N Debs",
      designer == "Nicholai Wiig Hansen" ~ "N W Hansen",
      designer == "Niels Gammelgaard" ~ "N Gammelgaard",
      designer == "Nike Karlsson" ~ "N Karlsson",
      designer == "Noboru Nakamura" ~ "N Nakamura",
      designer == "Ola Wihlborg" ~ "O Wihlborg",
      designer == "Olle Lundberg" ~ "O Lundberg",
      designer == "Paulin Machado" ~ "P Machado",
      designer == "Sarah Fager" ~ "S Fager",
      designer == "Synnöve Mork" ~ "S Mork",
      designer == "Thomas Sandell" ~ "T Sandell",
      designer == "Tina Christensen" ~ "T Christensen",
      designer == "Tom Dixon" ~ "T Dixon",
      designer == "Tord Björklund" ~ "T Björklund",
      designer == "Virgil Abloh" ~ "V Abloh",
      designer == "Wiebke Braasch" ~ "W Braasch",
      TRUE ~ designer)) %>%
  mutate(
    category = case_when(
      category == "Bar furniture" ~ "Bar Furniture",
      category == "Bookcases & shelving units" ~ "Bookcases & Shelving",
      category == "Cabinets & cupboards" ~ "Cabinets & Cupboards",
      category == "Café furniture" ~ "Café Furniture",
      category == "Chests of drawers & drawer units" ~ "Drawers (Units/Chests)",
      category == "Children's furniture" ~ "Furniture: Children",
      category == "Nursery furniture" ~ "Furniture (Nursery)",
      category == "Outdoor furniture" ~ "Furniture (Outdoor)",
      category == "Room dividers" ~ "Room Dividers",
      category == "Sideboards, buffets & console tables" ~ "Sideboards, Buffets & Console Tables",
      category == "Sofas & armchairs" ~ "Sofas & Armchairs",
      category == "Tables & desks" ~ "Tables & Desks",
      category == "TV & media furniture" ~ "TV & Media Furniture",
      TRUE ~ category)) %>% 
  count(designer, category) %>%
  group_by(designer) %>%
  mutate(sum = sum(n)) %>%
  filter(sum >=10) %>%
  group_by(category) %>%
  mutate(unique = length(unique(designer))) %>%
  mutate(
    designer = glue("{designer} <span style='font-size:6pt;color:#FFDA1A;'> ({sum}) </span>")
  )

ggplot(ikea.clean, aes(reorder(category, desc(unique)), reorder(designer, sum), size = n)) +
  geom_point(colour = "#FFDA1A") +
  labs(title = "THE IKEA FURNITURE CATÅLOGUE",
       subtitle = "THE KEY DESIGNERS",
       caption = "@CSHoggard | #TidyTuesday Week 45 | Source: Kaggle / IKEA",
       size = "Number of Designs")  +
  scale_y_discrete(expand = c(0,0.8)) +
  theme_minimal() +
  theme(
        plot.margin = margin(30,20,30,20),
        plot.background = element_rect(colour = "#0051BA", fill = "#0051BA"),
        plot.title = element_text(family = "Noto Sans", face = "bold", colour = "white", size = 28, hjust = 0.5, margin = margin(20,0,20,0)),
        plot.title.position = "plot",
        plot.subtitle = element_text(family = "Noto Sans", face = "bold", colour = "white", size = 18, hjust = 0.5, margin = margin(0,0,20,0)),
        plot.caption = element_text(family = "Noto Sans", face = "bold", colour = "white", size = 11, hjust = 0.13, margin = margin(40,0,10,0)),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(family = "Noto Sans", face = "bold", colour = "white", size = 10, angle = 90, hjust = 1, vjust = 0.2),
        axis.text.y = element_markdown(family = "Noto Sans", colour = "white", size = 8),
        panel.grid.major = element_line(colour = "#0064EA"),
        legend.position = c(0.4, -0.3),
        legend.direction = "horizontal",
        legend.text = element_text(family = "Noto Sans", colour = "white", size = 11),
        legend.title = element_text(family = "Noto Sans", face = "bold", colour = "white", size = 12))

ggsave("images/Week_45_Ikea.png", plot = last_plot(), width = 210, height = 380, units = "mm", dpi = 400) 

