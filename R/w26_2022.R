library(tidyverse)
library(lubridate)
library(janitor)
library(extrafont)
library(ggtext)
library(here)

paygap <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-28/paygap.csv')

paygap.clean <- paygap %>%
  mutate(year = as.factor(year(due_date))) %>%
  filter(year == 2023 | year == 2018) %>%
  droplevels() %>%
  select(diff_mean_hourly_percent, year, employer_id, employer_size) %>%
  pivot_wider(names_from = year, values_from = diff_mean_hourly_percent) %>%
  na.omit() %>%
  mutate(employer_size = case_when(
    employer_size == "250 to 499" ~ "250-499",
    employer_size == "500 to 999" ~ "500-999",
    employer_size == "1000 to 4999" ~ "1000-4999",
    employer_size == "5000 to 19,999" ~ "5000-19999",
    TRUE ~ employer_size))%>%
  pivot_longer(cols = c(`2018`, `2023`), values_to = "diff_mean_hourly_percent", names_to = "year") %>%
  mutate(employer_size = fct_relevel(employer_size, c("Less than 250","250-499","500-999","1000-4999","5000-19999"))) %>%
  mutate(diff_mean_hourly_percent = diff_mean_hourly_percent/100)


ggplot(paygap.clean, (aes(year, diff_mean_hourly_percent))) +
  geom_point(aes(size = employer_size), pch = 21, colour = "#FFFFFF", fill = "grey30") +
  geom_line(aes(group = employer_id), colour = "grey30", alpha = 0.2) +
  labs(title = toupper("The Gender Pay Gap<br>Still Exists"),
       caption = "<span style='font-size:12pt'>Differences in pay for UK companies  with published data for 2018 and 2023 </span><br><br><br> Positive: Favouring Men | Negative: Favouring Women <br><br> Size: Relative Company Size (see #TidyTuesday for details) <br><br><br><br> <span style='font-size:9pt'> @CSHoggard | Data: gender-pay-gap.service.gov.uk </span>") +
  scale_y_continuous(labels = scales::percent, limits = c(-0.75, 0.75)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  coord_fixed(ratio = 0.8) +
  theme_minimal() +
  theme(text = element_text(family = "Commissioner"),
        plot.margin = margin(50,40,20,40),
        plot.background = element_rect(fill = "#F9FBFF", colour = "#F9FBFF"),
        plot.caption.position = 'plot',
        plot.title.position = 'plot',
        plot.title = element_markdown(size = 46, family = "Commissioner", face = "bold", hjust = 0.5, margin = margin(10,20,40,20)),
        plot.caption = element_markdown(size = 10, hjust = 0.5, margin = margin(40,0,5,0)),
        legend.position = "none",
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(), 
        axis.text.x = element_text(size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave("images/Week_26_Gender_Pay_Gap.png", plot = last_plot(), dpi = 400)
