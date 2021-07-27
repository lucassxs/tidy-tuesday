#date: July 26,2021
library(tidyverse)
library(rvest)
library(ggrepel)
library(usmap)
library(RColorBrewer)
library(extrafont)
fonttable <- fonttable()

loadfonts(device = "win", quiet = TRUE) ## to load the font

# Import the data

olympics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-27/olympics.csv')
