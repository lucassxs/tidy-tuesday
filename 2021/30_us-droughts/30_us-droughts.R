# Install Packages
install.packages('tidyverse')
install.packages('ggtext')
install.packages('extrafont')


# Load Packages
library(tidyverse)
library(ggtext)
library(extrafont)
loadfonts(device = "win", quiet = TRUE) ## to load the font

# Get the Data-----

drought <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-20/drought.csv')

# Queries to inform plot captions-----
drought %>% 
  filter(state.abb == "AK") %>%
  select(area_total,drought_lvl, pop_total)
