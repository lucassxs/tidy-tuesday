# Setup ------------------------------------------------------------------------

library(knitr)
library(tidytuesdayR)
library(tidyverse)
library(extrafont)
library(showtextdb)
library(sysfonts)
library(showtext)
library(lubridate)


theme_set(theme_minimal())
font_add_google("IBM Plex Sans", "IBM Plex Sans")
showtext_auto()
showtext_opts(dpi = 300)

# Import -----------------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load('2022-07-12')
tuesdata <- tidytuesdayR::tt_load(2022, week = 28)

flights <- tuesdata$flights

# Tidy -------------------------------------------------------------------------
df <- flights %>%
  mutate(Date = make_date(YEAR,MONTH_NUM)) %>%
  group_by(Date, STATE_NAME) %>%
  summarise(total = sum(FLT_TOT_1))

# Plot -------------------------------------------------------------------------
df %>%
  ggplot(aes(x = Date, y = total, group = STATE_NAME)) +
  geom_boxplot() + 
  scale_y_continuous(expand = c(0,0), limits = c(NA,230000))+
  scale_x_date(expand = 01-01-2019, NA)+
  theme_minimal() +
  theme(text = element_text(size = 8, family = "IBM Plex Sans", 
                            color = "#000000"),
        plot.title = element_text(family = "IBM Plex Sans", size = 31.5, 
                                  hjust = 0.6, face = "bold"),
        plot.title.position = "plot",
        plot.subtitle = element_text())
  



