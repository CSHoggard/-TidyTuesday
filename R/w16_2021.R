library(tidyverse)
library(sf)

devtools::install_github("UrbanInstitute/urbnmapr")

library(here)

post_offices <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-13/post_offices.csv')

po_clean <- post_offices %>%
  filter(coordinates == "TRUE") %>%
  filter(!state %in% c("AK","HI")) %>%
  filter(established > 1945) %>%
  filter(is.na(discontinued)) %>%
  rename(abb = "state",
         subregion = "county1")%>%
  group_by(abb, subregion) %>%
  tally() %>%
  left_join(maps::state.fips, by = "abb") %>%
  select(abb, polyname, subregion, n) %>%
  mutate(subregion = tolower(subregion)) %>%
  rename(region = "polyname") %>%
  mutate(region = case_when(
    region == "washington:main" ~ "washington",
    region == "massachusetts:martha's vineyard" ~ "massachusetts",
    region == "massachusetts:main" ~ "massachusetts",
    region == "massachusetts:nantucket" ~ "massachusetts",
    region == "michigan:north" ~ "michigan",
    region == "michigan:south" ~ "michigan",
    region == "new york:manhattan" ~ "new york",
    region == "new york:main" ~ "new york",
    region == "new york:staten island" ~ "new york",
    region == "new york:long island" ~ "new york",
    region == "north carolina:main" ~ "north carolina",
    region == "north carolina:knotts" ~ "north carolina",
    region == "north carolina:spit" ~ "north carolina",
    region == "virginia:chesapeake" ~ "virginia",
    region == "virginia:chincoteague" ~ "virginia",
    region == "washington:san juan island" ~ "washington",
    region == "washington:lopez island" ~ "washington",
    region == "washington:orcas island" ~ "washington",
    region == "washington:main" ~ "washington",
    TRUE ~ region
  ))
  
county <- map_data("county") %>%
  left_join(po_clean, by = c("region"="region","subregion"="subregion")) %>%
  mutate(n = replace(n, which(is.na(n)), 0)) %>%
  mutate(new = ifelse(n > 0, "Yes", "No"))

ggplot(county, aes(x=long, y=lat, group=group, fill = new)) + 
  geom_polygon(color = "#E4E4E4") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  labs(title = "test",
       subtitle = "Post Offices Established Following The Second World War",
       caption = "test") +
  scale_fill_manual(values = c("#EEEEEE", "#4c90c1")) + 
  theme_void() +
  theme(plot.margin = margin(20,20,20,20),
        plot.background = element_rect(fill = "#FBFEFF", colour = NA),
        legend.position = "none")
