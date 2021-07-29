#date: July 26,2021
library(tidyverse)
library(rvest)
library(ggpubr)
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

ggpubr

#### medal counts in Swimming grouped by team


#Creating custom colour schemes
col1 <- c("tan4", "gold3", "gray80")
col2 <- c("#fbb32c", "#ef5c70", "#eb334c", "#040404", "#0484cb")


#Creating dataframe of medal counts in archery grouped by team
dat <- olympics %>%
  filter(sport == "Swimming") %>%
  group_by(team) %>%
  count(medal) %>%
  mutate(freq = n / sum(n)*100) #calculating percentages

dat


#Plotting donut chart of South Korea's medals
p <- dat %>%
  filter(team == "Brazil") %>%
  ggdonutchart("freq", label = "medal", fill="medal") +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_text(family="Lato", hjust=0.5, vjust=-48, size=14), #positioning  title
        plot.margin = margin(-3, -3, -3, -3, "cm")) + #adjusting margins
  xlim(c(-2, 4)) +  #making donut thinner
  scale_fill_manual(values = col1) +
  ggtitle("Brazil")

p


#Plotting donut chart of Belgium's medals
p2 <- dat %>%
  filter(team == "Argentina") %>%
  ggdonutchart("freq", label = "medal", fill="medal") +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_text(family="Lato", hjust=0.5, vjust=-48, size=14),
        plot.margin = margin(-3, -3, -3, -3, "cm")) +
  xlim(c(-2, 4)) +
  scale_fill_manual(values = col1) +
  ggtitle("Argentina")

p2


#Plotting donut chart of France's medals
p3 <- dat %>%
  filter(team == "Paraguay") %>%
  ggdonutchart("freq", label = "medal", fill="medal") +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_text(family="Lato", hjust=0.5, vjust=-48, size=14),
        plot.margin = margin(-3, -3, -3, -3, "cm")) +
  xlim(c(-2, 4)) +
  scale_fill_manual(values = col1) +
  ggtitle("Paraguay")

p3


#Plotting donut chart of USA's medals
p4 <- dat %>%
  filter(team == "Ecuador") %>%
  ggdonutchart("freq", label = "medal", fill="medal") +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_text(family="Lato", hjust=0.5, vjust=-107, size=14),
        plot.margin = margin(-7, -3.5, 0, 0, "cm")) +
  xlim(c(-2, 4)) +
  scale_fill_manual(values = col1) +
  ggtitle("Ecuador")

p4


#Plotting donut chart of China's medals
p5 <- dat %>%
  filter(team == "Uruguay") %>%
  ggdonutchart("freq", label = "medal", fill="medal") +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_text(family="Lato", hjust=0.5, vjust=-107, size=14),
        plot.margin = margin(-7, 0, 0, -3.5, "cm")) +
  xlim(c(-2, 4)) + 
  scale_fill_manual(values = col1) +
  ggtitle("Uruguay")

p5


#Arranging graphs
a1 <- ggarrange(p, p2, p3, ncol = 3)
a2 <- ggarrange(p4, p5)

a1
a2

rings <- ggarrange(a1, a2, ncol = 1, nrow = 2)
annotate_figure(rings,
                bottom = text_grob("Data source: Kaggle",
                                   x=0.92, y=1, family="Lato", face = "italic", size = 12),
                top = text_grob("Olympic Medals in Archery 1920-2016",
                                x=0.5, y=-16, family="Lato Semibold", size = 20))

#Saving plot
ggsave("Rings.png", height = 6, width = 10, units = "in", dpi = 300)



#Plotting line graph of cumulative medal count
p6 <- olympics %>%
  filter(team %in% c("Brazil", "Argentina", "Paraguay", "Ecuador", "Uruguay")) %>%  #filtering to 5 teams
  group_by(team) %>%
  ggplot(aes(x=year, y=n, color=team)) +
  geom_step(aes(y=..y..),stat="ecdf", size=1.5) + #making cumulative
  theme_classic() + #removing grid
  theme(text = element_text(family = "Lato", size=14), #changing font and size
        plot.title = element_text(family="Lato Semibold", hjust=0.5, size=18), #centring title, changing font
        plot.caption = element_text(family = "Lato", size=12, face = "italic"), #custom caption text
        legend.position = "top",  #positioning legend below title 
        legend.title = element_blank()) + #removing legend title
  theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")) +  #adding 0.5 cm margins to plot as stuff was being cut off
  scale_y_continuous(expand = expansion(mult = c(0,0))) +  #removing gap between bar and x axis
  scale_colour_manual(values = col2) +
  ylab("Cumulative Frequency") +    
  xlab("Count") + 
  ggtitle("Olympic Medals in Swimming Over Time in South America") +
  labs(caption = "Data source: Kaggle")

p6


#Saving plot
ggsave("Medal Frequency.png", height = 6, width = 10, units = "in", dpi = 300)
