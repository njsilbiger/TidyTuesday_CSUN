### Tidy Tuesday Animal Crossing ###
## Code by Nyssa Silbiger ###

## load libraries
library(tidyverse)
library(ggimage)
library(ggridges)
library(viridis)
library(hrbrthemes)

# ust the items data
items <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/items.csv')

## Let's look at sell value versus buy value for different items.  Are they the same?
## let's just look at bells for now (there is also miles)

# what is the difference between buyers and sellers by items
item2<-items %>%
  filter(sell_currency == 'bells',
         buy_currency=='bells')%>%
  mutate(difference =  buy_value - sell_value) %>%
  select(category, difference, image_url)

# get the max difference per category for images (not some items don't have a url)
maxitem<-item2 %>% 
  group_by(category)%>%
  summarise(difference = max(difference, na.rm = TRUE))%>%
  left_join(item2)%>%
  distinct()
  
# density plot of the difference between buying and selling
  ggplot(item2,aes(x = difference, y = reorder(category, difference, FUN = max), fill = category))+
    geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
    geom_image(data = maxitem ,aes(image=image_url), size=0.05)+
    scale_x_log10()+ # log transform it cuz there is a really wide range
    theme_modern_rc() +
    theme(
      legend.position="none",
      panel.spacing = unit(0.1, "lines"),
      strip.text.x = element_text(size = 8)
    )+
    xlab("Difference between buy value and sell value in bells")+
    ylab("Category")+
    labs(title = "It's a sellers market!",
         subtitle = "Literally everything is more expensive to buy.",
         caption = "Plot by Nyssa Silbiger \n@nsilbiger")+
    ggsave(filename = "AnimalCrossing_050520/Animalcrossingplot.png", width = 6, height = 6)
    
  