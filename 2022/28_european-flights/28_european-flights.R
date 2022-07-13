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
interrupcao <- flights %>%
  mutate(Date = make_date(YEAR,MONTH_NUM)) %>%
  group_by(Date, STATE_NAME) %>%
  summarise(total = sum(FLT_TOT_1))

# Plot -------------------------------------------------------------------------
interrupcao %>%
  ggplot(aes(x = Date, y = total, group = STATE_NAME)) +
  geom_line(colour = "white") + 
  scale_y_continuous(expand = c(0,0), limits = c(NA, 230000)) +
  scale_x_date(expand= c(2016-01-01, NA)) +
  theme_minimal() +
  theme(text = element_text(size = 8, family = "IBM Plex Sans", 
                            color = "#F5F5F5"),
        plot.title = element_text(family = "IBM Plex Sans", size = 31.5, 
                                  hjust = 0.6, face = "bold"),
        plot.title.position = "plot",
        plot.subtitle = element_text(family = "IBM Plex Sans", size = 14, 
                                     hjust = 0.6),
        plot.caption = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        axis.text = element_text(size = 10, family = "IBM Plex Sans", 
                                 color = "#F5F5F5"),
        axis.title.y = element_text(size = 10, family = "IBM Plex Sans",
                                    color = "#F5F5F5", angle = 90),
        legend.position = "NULL",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(color = "#F5F5F5", 
                                          size = 0.25, linetype = 3),
        axis.line.x.bottom = element_line(color = "#F5F5F5", size = 1),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        plot.background = element_rect(color = "#525252", fill = "#525252")) +
  annotate(geom = "curve", x = as.Date("2021-01-01"), y = 170000, 
           xend = as.Date("2020-04-01"), yend = 120000,
           curvature = -0.2, arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text", y = 180000, x = as.Date("2021-01-01"), 
           label = "Queda COVID", hjust = "center", family = "IBM Plex Sans", 
           fontface = "bold", size = 5,color = "#F7F7F7" ) +
  labs(title = "Interrupções nas Viagens Aéreas",
       subtitle = "Número Total de voos mensais por
       País na Europa de Janeiro de 2016 a Maio de 2022",
       caption = "\nData: Eurocontrol",
       y = "Nº total de voos (Chegadas + Partidas)\n",
       color = "#525252")
  
ggsave(paste0("28_european-flights_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 8, height = 6)



