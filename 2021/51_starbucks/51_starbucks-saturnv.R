install.packages('ggthemes')
install.packages('scales')
install.packages('cowplot')
install.packages('ggchicklet')


library(tidyverse)
library(ggplot2)
library(ggthemes)
library(scales)
library(magick)
library(cowplot)
library(ggchicklet)

# read starbucks csv
df_base <- read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-12-21/starbucks.csv") %>% as_tibble()

# filter for grande frappuccino's with milk and whip
df <- df_base %>% filter(milk == 1,
                         serv_size_m_l > 0,
                         size == "grande",
                         whip == 0,
                         grepl("Frappuccino", product_name))


# format the product name text for plotting neatly
df$product_name <- gsub(" Frappuccino","", df$product_name)
df$product_name <- gsub("Ã¨", "é", df$product_name)

# find the joules of energy for each drink
df$joules <- (df$calories * 4184)

# using https://en.wikipedia.org/wiki/Orders_of_magnitude_(power)#109_to_1014_W
# find the joules the saturn v used in its 168 second first stage
satv_joules <- 168 * (1.66*10^11)

# calculate the number of cups needed to match that
df$satv <- satv_joules/df$joules

# load our rock and fire
img <- image_read("https://cdn.pixabay.com/photo/2017/01/31/20/34/missile-2027068_960_720.png")
fire <- image_read("https://cdn.pixabay.com/photo/2017/11/11/16/23/fire-2939426_960_720.png") %>%
  image_rotate(180)

# plot the graph as object a
a <- ggplot(df, aes(x = reorder(product_name, -satv), y = satv)) +
  # use nice starbucks'y colours
  geom_chicklet(fill="#00704A") +
  # make horizontal bar chart
  coord_flip() +
  # label the graph
  labs(title = expression(bold("Frappuccinos necessários para lançar um foguete Saturno V")),
       subtitle = "(...em um mundo onde o café pode ser usado como combustível de foguete)",
       caption = expression(italic("Data Source: Starbucks, Wikipedia")),
       # remove x axis label
       x = "", 
       y = "Copos Necessários") +
  # make custom y axis scale
  scale_y_continuous(label=c("20000000" = "20 milhões",
                             "40000000" = "40 milhões",
                             "60000000" = "60 milhões"),
                     limits = c(0, 60000000),
                     breaks = c(20000000, 40000000, 60000000),
                     expand = c(0, 0)) +
  # set theming
  theme_tufte() +
  theme(plot.margin=unit(c(0.3,0.8,0.1,0.2), 'cm'),
        text = element_text(family="sans", size = 12),
        plot.title = element_text(size = 17),
        plot.subtitle = element_text(size = 13, colour = '#444444'),
        axis.title.y = element_text(size = 13),
        plot.background = element_rect(colour = 'white')) 
# plot the rocket with its flame
ggdraw() +
  draw_plot(a) +
  draw_image(fire, 0.35, -0.25, scale = 0.06) +
  draw_image(img, 0.35, 0.07, scale = 0.6)

# save output
ggsave(paste0("rocket", format(Sys.time(), "%d%m%Y"), ".png"),
       dpi = 320,
       width = 12,
       height =  7)
                      

