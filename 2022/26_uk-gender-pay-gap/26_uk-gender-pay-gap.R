# Week 26
# Theme: UK Gender pay gap
# Date: Jun 28 2022
# Author: Lucas Stefano

# Load packages
library(tidyverse)
library(showtext)
library(scales)
library(ggtext)
library(tidytuesdayR)
library(extrafont)
library(ib)
#theme_set(theme_light())
#font_add_google("IBM Plex Sans", "IBM Plex Sans")
extrafont::loadfonts(device = "win", quiet = TRUE) ## to load the font



# Get the data
tt <- tt_load("2022-06-28")
paygap <- tuesdata$paygap

tt %>% 
  map(glimpse)

paygap <- tt$paygap %>% 
  mutate(
    employer_name = str_to_lower(employer_name)
  )


paygap %>% 
  count(employer_id, sort = TRUE)
double_name <- paygap %>% 
  distinct(employer_id, employer_name) %>% 
  count(employer_id) %>% 
  filter(n > 1) %>% 
  pull(employer_id)
paygap %>% 
  distinct(employer_id, employer_name) %>% 
  filter(employer_id %in% double_name) %>% 
  arrange(employer_id)

paygap %>% 
  count(employer_size, sort = TRUE)

theme_set(theme_gray())
theme <- theme_update(text = element_text(family = "American Typewriter", size = 17),
                      title = element_text("American Typewriter", size = 20, color = "gray20"),
                      plot.title = element_text("American Typewriter", size = 30, color = "gray20"),
                      axis.text = element_text(size = 16))
p <- paygap %>% 
  filter(!is.na(male_lower_quartile)) %>% 
  add_count(employer_size) %>% 
  pivot_longer(male_lower_quartile:female_top_quartile) %>% 
  mutate(
    name = str_remove(name, "_quartile"),
    value = value / 100
  ) %>% 
  separate(name, c("gender", "class1", "class2"), fill = "right") %>% 
  mutate(class = case_when(
    class1 == "lower" & is.na(class2) ~ "Q1",
    class1 == "lower" & class2 == "middle" ~ "Q2",
    class1 == "upper" ~ "Q3",
    class1 == "top" ~ "Q4"
  )) %>% 
  filter(gender == "female", employer_size != "Not Provided") %>% 
  mutate(
    employer_size = paste0(employer_size, " (", n, ")"),
    employer_size = employer_size %>% 
      fct_reorder(parse_number(employer_size)) %>% 
      fct_relevel("Less than 250 (1867)")
  ) %>% 
  ggplot(aes(class, value)) +
  geom_hline(yintercept = 0.5) +
  geom_boxplot() +
  scale_y_continuous(labels = percent) +
  facet_wrap(vars(employer_size)) +
  labs(x = "Quartil de Pagamento", y = "Participação Feminina",
       title = "A participação feminina tende a diminuir, à medida que a remuneração \naumenta, em cada categoria de número de funcionários.",
       subtitle = "( ) denota # de amostras e Q4 é o quartil superior",
       caption = "Fonte: gender-pay-gap.service.gov.uk")+
#theme(text=element_text("IBM Plex Sans"))+
#theme_ib(md = TRUE)+
# Save plot
  ggsave("week26-2022.png", device = "png", type = "cairo", width = 15.5, height = 14, dpi = 300)

