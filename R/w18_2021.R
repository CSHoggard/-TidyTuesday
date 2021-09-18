library(rcartocolor)
library(tidyverse)
library(ggtext)
library(extrafont)
library(here)

departures <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-27/departures.csv')

departures.clean <- departures %>%
  select(-dismissal_dataset_id, -gvkey, -co_per_rol, -coname, -ceo_dismissal, 
         -leftofc, -notes, -sources, -eight_ks, -cik) %>%
  mutate(duration = fyear_gone - fyear) %>%
  mutate(duration2 = duration + 30) %>%
  filter(duration > 1) %>%
  mutate(departure_code = case_when(
    departure_code == 1 ~ "Involuntary (Death)",
    departure_code == 2 ~ "Involuntary (Illness)",
    departure_code == 3 ~ "Involuntary (Job Performance)",
    departure_code == 4 ~ "Involuntary (Concerns or Violations)",
    departure_code == 5 ~ "Voluntary (Retirement)",
    departure_code == 6 ~ "Voluntary (New Opportunities)",
    departure_code == 7 ~ "Other",
    departure_code == 8 ~ "Missing",
    departure_code == 9 ~ "Error",
    TRUE ~ as.character(departure_code)
  )) %>%
  filter(!departure_code %in% c("Missing", "Error")) %>%
  filter(!exec_fullname == "William G. Parzybok, Jr.") %>%
  mutate(id = row_number())

labels <- departures.clean %>%
  filter(id == 135 | id == 2 | id == 253) %>%
  mutate(label = glue::glue("<b>{exec_fullname}</b><br><span style='font-size:8pt'>Duration: {duration} years</span>")) %>% 
  add_column(
    x = c(135, 2, 253),
    y = c(68, 59, 60)
  )

ggplot(departures.clean, aes(id, duration2, colour = departure_code)) +
  geom_segment(aes(x = id, xend = id, y = 0, yend = duration2), size = 0.3) +
  geom_rect(aes(xmin = 1, xmax = 651, ymin = 0, ymax = 30), 
            fill = "white", color = "white") +
  geom_point(aes(size = duration)) + 
  labs(caption = "Departure: <span style = 'color:#16A87E;'>Death</span> | <span style = 'color:#3567AB;'>Illness</span> | <span style = 'color:#F2B700;'>Job Performance</span> | <span style = 'color:#7D398C;'>Concerns or Violations</span> | <span style = 'color:#A3A896;'>Retirement</span> | <span style = 'color:#87BF67;'>New Opportunities</span> | <span style = 'color:#E63A70;'>Other</span> <br><br> <b style='font-size:8pt;'><span style = 'color:#BCBCBC;'> Circles represent 10-year intervals </span><br><br> @CSHoggard | Data: Gentry et al. (2021)</b>") +
  geom_hline(aes(yintercept = (40)), size = 0.4, color = "grey80") +
  geom_hline(aes(yintercept = (50)), size = 0.4, color = "grey80") +
  geom_richtext(data = labels, aes(x = x, y = y, label = label, color = departure_code),
                lineheight = 0.8,
                size = 4,
                family = "Fira Sans",
                fill = NA,
                label.color = NA) +
  geom_text(x = 325, y = -5,
            label = "Why did the CEO leave?",
            family = "Fira Sans",
            size = 5.5,
            lineheight = 0.87,
            color = "grey60") +
  geom_text(x = 325, y = 0,
            label = "(for those who worked at least one year)",
            family = "Fira Sans",
            size = 3,
            lineheight = 0.87,
            color = "grey80") +
  scale_color_carto_d(palette = "Bold") +
  scale_size_continuous(range = c(1, 3)) +
  coord_polar() +
  theme_void() +
  theme(plot.margin = margin(20,20,20,20),
        plot.caption = element_textbox_simple(size = 10, halign = 0.5, colour = "grey50", family = "Fira Sans", margin = margin(-30,0,0,0)),
        legend.position = "none")

ggsave("images/Week_18_CEOs.png", plot = last_plot(), width = 200, height = 210, units = "mm")
