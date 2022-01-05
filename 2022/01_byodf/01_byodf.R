# date: January 04, 2022

# Install Packages
install.packages("tidyverse")

library(schrute)
library(tidyverse)
library(splitstackshape) ## for cSplit()
library(treemapify) ## for geom_treemap()
library(extrafont)

loadfonts(device = "win", quiet = TRUE) ## to load the font

# Import the transcripts
office_transcript <- schrute::theoffice  ## 55130 lines
office_transcript <- office_transcript %>%
  mutate(season = as.numeric(season),
         episode = as.numeric(if_else(episode_name == "Finale", "23", episode)), ## match finale episode number to the one in office_ratings
  )

# Import the IMDb ratings from data.world
office_ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-17/office_ratings.csv')
## 188 episodes

# Join tables to get episode summary
office_episodes <- office_ratings %>%
  left_join(office_transcript, 
            by = c("season" = "season", "episode" = "episode"),
            suffix = c("", "2")) %>% 
  select(-c(character, text, text_w_direction, index)) %>% 
  unique()

# Set theme
theme_set(theme_gray())
theme <- theme_update(text = element_text(family = "American Typewriter", size = 13),
                      title = element_text("American Typewriter", size = 20, color = "gray20"),
                      plot.title = element_text("American Typewriter", size = 30, color = "gray20"),
                      axis.text = element_text(size = 16))


# ‘The Office’ IMDb Ratings
## WIP: need to make annotations more efficient
office_episodes %>% 
  mutate(season = as.factor(season)) %>% 
  ggplot(aes(x = season, y = imdb_rating, fill = season)) +
  geom_violin(show.legend = FALSE,
              draw_quantiles = TRUE,
              color = "gray90") +
  labs(title = "‘The Office’ - Avaliações do IMDb", x = "Temporada", y = "Avaliação do IMDb") + 
  scale_fill_brewer(palette = "Spectral") +
  scale_y_continuous(expand = expansion(mult = c(0,0)), limits = c(6,10))  +
  theme(axis.title.y = element_text(family = "Hack"),
        axis.title.x = element_text(size = 25)) +
  annotate(geom = "label", x = 1, y = 7.42, size = 5, ## label
           label = "Pilot:\nDiversity Day", 
           family = "American Typewriter",
           hjust = "center",
           lineheight = 0.9,
           color = "#9E0142",
           #label.size = NA,
           fill = "white") +
  annotate(geom = "label", x = 2, y = 9.4, size = 5, ## label
           label = "Casino Night", 
           family = "American Typewriter",
           hjust = "center",
           lineheight = 0.9,
           color = "#F46D43",
           #label.size = NA,
           fill = "white") +
  annotate(geom = "label", x = 3, y = 9.4, size = 5, ## label
           label = "The Job", 
           family = "American Typewriter",
           hjust = "center",
           lineheight = 0.9,
           color = "#FDAE61",
           #label.size = NA,
           fill = "white") +
  annotate(geom = "label", x = 4, y = 9.4, size = 5, ## label
           label = "Dinner Party", 
           family = "American Typewriter",
           hjust = "center",
           lineheight = 0.9,
           color = "#FDAE61",
           #label.size = NA,
           fill = "white")  +
  annotate(geom = "label", x = 5, y = 9.7, size = 5, ## label
           label = "Stress Relief", 
           family = "American Typewriter",
           #fontface = "bold",
           hjust = "center",
           lineheight = 0.9,
           color = "goldenrod2",
           #label.size = NA,
           fill = "white")  +
  annotate(geom = "label", x = 6, y = 9.4, size = 5, ## label
           label = "Niagara", 
           family = "American Typewriter",
           #fontface = "bold",
           hjust = "center",
           lineheight = 0.9,
           color = "darkolivegreen3",
           #label.size = NA,
           fill = "white") +
  annotate(geom = "label", x = 7, y = 9.8, size = 5, ## label
           label = "Goodbye, Michael", 
           family = "American Typewriter",
           hjust = "center",
           lineheight = 0.9,
           color = "#66C2A5",
           #label.size = NA,
           fill = "white") +
  annotate(geom = "label", x = 8, y = 6.6, size = 5, ## label
           label = "Get the Girl", 
           family = "American Typewriter",
           hjust = "center",
           lineheight = 0.9,
           color = "#66C2A5",
           #label.size = NA,
           fill = "white")  +
  annotate(geom = "label", x = 9, y = 9.8, size = 5, ## label
           label = "Finale", 
           family = "American Typewriter",
           hjust = "center",
           lineheight = 0.9,
           color = "#3288BD",
           #label.size = NA,
           fill = "white")

ggsave(paste0("byodf", format(Sys.time(), "%d%m%Y"), ".png"),
       dpi = 320,
       width = 12,
       height =  7)
