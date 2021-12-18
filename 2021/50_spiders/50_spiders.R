# Install Packages
install.packages('tidyverse')
install.packages('ggtext')
install.packages('extrafont')


# Load Packages
library(tidyverse)
library(ggtext)
library(extrafont)
loadfonts(device = "win", quiet = TRUE) ## to load the font

# Set theme-----

theme_set(theme_minimal())
theme <- theme_update(text = element_text(family = "Source Sans Pro Light", size = 13),
                      title = element_text(family = "Fira Mono", size = 20, color = "gray20"),
                      plot.title = element_text(family = "CHANEY Ultra Extended", size = 20, color = "gray20"),
                      plot.subtitle = element_text(family = "Gruppo", size = 25, color = "gray20"),
                      plot.caption = element_text(family = "Source Sans Pro Light", size = 13, color = "gray20"),
                      legend.text = element_text("Gruppo", size = 16),
                      axis.text = element_text(size = 14),
                      axis.line.x = element_line(color = "gray80"),
                      axis.line.y = element_line(color = "gray80"))


# Get the Data-----

spiders <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-12-07/spiders.csv')

brazil <- spiders %>%
  head(6)

brazil1 <- spiders %>%
  filter(distribution == "Brazil") %>%
  head(6)

# Scatter plot 

spiders %>%
  ggplot(aes(x = year, y = genus)) +
  geom_point(aes(color = distribution), size = 4, aplha = 0.3) +
  geom_point(data = brazil1, aes(x = year, y = genus), color =  "#f35588", size = 4, alpha = 0.3) +
  labs(title = "Aranhas",
       x = "",
       y = "",
       color = "",
       )+
  scale_color_manual(values=c("#CC0000", "#006600", "#669999", "#00CCCC", 
                              "#660099", "#CC0066", "#FF9999", "#FF9900", 
                              "black", "black", "black", "black", "black")) +
  theme(
    plot.title = element_text(size = 30,),
    plot.subtitle = element_text(size = 35),
    legend.position = c(.85, .9),
    legend.justification = c("right", "top"),
    legend.margin = margin(6, 6, 6, 6)
  ) +
  geom_label_repel(data = labels, 
                   aes(label = label),
                   family = "Source Sans Pro Light",
                   size = 4.5,
                   nudge_y = 5.5,
                   color = "#3FDAC4",
                   nudge_x = ,
                   box.padding = 0.25,
                   segment.alpha = 0,
                   label.size = .25)

ggsave(here::here("2021", "50_spiders", "spiders-brazil.png"), device = "png", type = "cairo", width = 12, height = 10, dpi = 300)

