# Install Packages
install.packages('tidyverse')
install.packages('ggtext')
install.packages('ggchicklet')
install.packages('showtext')

# Load Packages
library(tidyverse)
library(ggtext)
library(ggchicklet)
library(showtext)

#Load fonts
font_add_google("Schoolbell","bell")
showtext_auto()
f2 = "bell"

# Get the Data-----
starbucks <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-12-21/starbucks.csv')

# Caffine and caffine/ml by serving size and product name
# Contest
