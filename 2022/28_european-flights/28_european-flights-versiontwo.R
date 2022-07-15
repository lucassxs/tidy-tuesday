# Setup ------------------------------------------------------------------------

library(tidyverse)
library(tidytuesdayR)
library(scales)
theme_set(theme_light())
library(dplyr)

theme_set(theme_minimal())
font_add_google("IBM Plex Sans", "IBM Plex Sans")
showtext_auto()
showtext_opts(dpi = 300)

# Import -----------------------------------------------------------------------

flights <- read.csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-12/flights.csv')


# Tidy -------------------------------------------------------------------------
head(flights)
names(flights)
flights %>% 
  group_by(STATE_NAME,YEAR,MONTH_NUM) %>% 
  dplyr::summarise(total_flights = sum(FLT_TOT_1,na.rm=T)) -> agg
head(agg)
date_base <- paste0(agg$YEAR,"-",agg$MONTH_NUM,"-01")
agg$date <- as.Date(date_base,format = "%Y-%m-%d")

# Plot -------------------------------------------------------------------------
agg %>% 
  ggplot(aes(x=date, y= log(total_flights), col = as.factor(STATE_NAME))) +
  geom_point() +
  geom_line() +
  theme_bw() +
  labs(col = "Ano") +
  ylab("Registro do total de voos") +
  xlab("Data") +
  theme(legend.position = "none") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  ggtitle("Registro do total de voos por país ao longo do tempo")

agg$post_2020 <- agg$date >= "2020-01-01"

agg %>% 
  arrange(total_flights) %>% 
  group_by(YEAR,MONTH_NUM) %>%
  dplyr::mutate(rank = row_number()) -> agg

agg %>% 
  group_by(STATE_NAME,post_2020) %>% 
  dplyr::mutate(vrank = var(rank,na.rm=T)) -> agg

summary(agg)

high_var <- unique(agg[which(agg$vrank >= quantile(agg$vrank,0.9) & agg$post_2020 == T),"STATE_NAME"])

agg[,"high_var"] <- F
agg[which(agg$STATE_NAME %in% unlist(high_var)),"high_var"] <- T

agg %>% 
  ggplot(aes(x=date,y=rank,col=STATE_NAME)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  theme(legend.position = "none") +
  ylab("Ordem de classificação do total de voos") +
  xlab("Data") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  ggtitle("Ordens de classificação total do voo")

agg %>% 
  filter(high_var == T) %>% 
  ggplot(aes(x=date,y=rank,col=STATE_NAME)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  ylab("Ordem de classificação do Total de Voos") +
  xlab("Ano") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(col = "País") +
  ggtitle("Classificação do Total de voos após 2020 com Variância de 10%")

ggsave(paste0("28_european-finssinhhh", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 8, height = 6)

