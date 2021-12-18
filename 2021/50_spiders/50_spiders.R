
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

spiders <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-12-07/spiders.csv')
