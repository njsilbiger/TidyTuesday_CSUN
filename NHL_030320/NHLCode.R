## Tidy Tuesday for NHL
## 3/3/2020

library(tidyverse)
library(ggtext)
library(patchwork)
library(magick)
library(ggimage)

game_goals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-03/game_goals.csv')

### Look at Wayne Gretsky stats

## what percent of total goals  at home versus away over time 
Goals<-game_goals %>%
  filter(str_detect(player, "Wayne")) %>%
  group_by(season, location)%>%
  summarise(n = sum(goals)) %>%
  mutate(percentage = n / sum(n))%>%
  ggplot(aes(x = season, y = percentage*100, fill = location))+
  geom_area(alpha=0.6 , size=1, colour="black")+
  scale_fill_manual(values = c("orange","blue"))+
  ylab('Percentage of Goals')+
  xlab('Season')+
  theme_classic()+
  theme(legend.position = "none", text = element_text(size=20))

# image of hockey puck
img<-"https://i.ebayimg.com/00/s/MTYwMFgxNjAw/z/ZNMAAOSw94pdOgXR/$_57.JPG"

# Assists
Assists<-game_goals %>%
  filter(str_detect(player, "Wayne")) %>%
  group_by(season, location)%>%
  summarise(n = sum(assists)) %>%
  mutate(percentage = n / sum(n))%>%
  ggplot(aes(x = season, y = percentage*100, fill = location))+
  geom_area(alpha=0.6 , size=1, colour="black")+
  scale_fill_manual(values = c("orange","blue"))+
  ylab('Percentage of Assists')+
  xlab('Season')+
  theme_classic()+
  theme(legend.position = "none", text = element_text(size=20))+
  geom_image(aes(x = 1997, y = 90, image=img), size=.15) # add the hockey puck picture to the corner


### Bring all the plots together
Goals + Assists + plot_annotation(title = "Wayne Gretzky stats at <span style = 'color:orange;'>home</span> versus <span style = 'color:blue;'>away</span> games",
                                   caption = "Plot by N. Silbiger \nTwitter: @NSilbiger",
                                   theme = theme(
                                   plot.title = element_markdown(lineheight = 1.1, size = 20, hjust = 0.5)
                                     ))+
  ggsave('NHL_030320/NHLplot.png', width = 10, height = 5.2)

