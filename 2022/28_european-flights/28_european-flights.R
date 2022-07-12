# Setup ------------------------------------------------------------------------

library(knitr)
library(tidytuesdayR)
library(tidyverse)
library(extrafont)
library(showtextdb)
library(sysfonts)
library(showtext)

theme_set(theme_minimal())
font_add_google("IBM Plex Sans", "IBM Plex Sans")
showtext_auto()
showtext_opts(dpi = 300)

# Import -----------------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load('2022-07-12')
tuesdata <- tidytuesdayR::tt_load(2022, week = 28)

flights <- tuesdata$flights

# Tidy -------------------------------------------------------------------------