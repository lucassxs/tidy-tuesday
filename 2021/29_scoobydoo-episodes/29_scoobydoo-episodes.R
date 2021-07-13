#date July 13, 2021

# install packages
install.packages('tidyverse')
install.packages('ggrepel')
install.packages('extrafont')
install.packages('Cairo')
install.packages('gganimate')

# load packages
library(tidyverse)
library(ggrepel)
library(extrafont)
library(Cairo)
library(gganimate)

# read database  

scoobydoo <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-13/scoobydoo.csv')
