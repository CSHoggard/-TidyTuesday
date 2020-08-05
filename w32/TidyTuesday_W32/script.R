# tidytuesday graphic #
# twitter: @cshoggard #

# library

library(tidyverse)

# data input
country_totals <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-04/country_totals.csv')

# data tidying

country_totals$type <- as_factor(country_totals$type) # convert to factor
country_totals <- country_totals %>% select(-level) # remove 'level' column

# data visualisation: country totals #

ggplot(country_totals, aes())

