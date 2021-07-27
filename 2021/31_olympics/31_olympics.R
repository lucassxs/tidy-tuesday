#date: July 26,2021
library(tidyverse)
library(rvest)
library(ggrepel)
library(usmap)
library(RColorBrewer)
library(extrafont)
library(Cairo)
fonttable <- fonttable()

loadfonts(device = "win", quiet = TRUE) ## to load the font

# Import the data
olympics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-27/olympics.csv')


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

top <- olympics %>%
  head(6)

top_females <-olympics %>%
  filter(sex == "f") %>%
  head(2)

notable <- olympics %>%
  filter(year >= 2020 | year == min(year)) %>%
  head(3)

labels <- rbind(top,notable) %>%
  mutate(label = paste0(name,"by",age,"(",year,")"))


# Scatter plot 
olympics %>%
  ggplot(aes(x = year, y = age)) +
  geom_point(aes(color = sex),size = 4, alpha = 0.3) +
  geom_point(data = top_females, aes(x = year, y = age), color = "#f35588", size = 4, alpha = 0.3) +
  labs(title = " Gender of Participants in the Olympics",
       x = "",
       y = "",
       color = ""
       ) + 
  scale_color_manual(values = c("#f35588","#05dfd7"),
                     labels = c("female", "male")) +
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

ggsave(here::here("2021", "31_olympics", "gender-of-olympics.png"), device = "png", type = "cairo", width = 12, height = 10, dpi = 300)