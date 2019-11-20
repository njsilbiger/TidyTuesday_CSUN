## Make a clock showing the best and worst ranked birds depending on the time of day they were ranked
# By Nyssa Silbiger
# 11/19/19

###################
library(tidyverse)
library(lubridate)
library(grid)
library(ggimage)
library(cowplot)

## load the data ##############3
nz_bird <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-19/nz_bird.csv")


# clean the data ################3
best_birds<-nz_bird %>%
  drop_na %>%
  separate(vote_rank, into = c("Name", "Rank"), sep = "_") %>% # split the vote
  group_by(hour, bird_breed,Rank) %>% # count how many votes were for each bird each hour
  tally() %>%
  filter(Rank %in% c(1,5)) %>% # pull out only the best (1) and the worst (5) birds
  pivot_wider(names_from = Rank, values_from = n)%>% # make 1 and 5 their own columns
  group_by(hour)

# pull out the worst birds (ones with the most 5s)
worst_birds<-  best_birds %>%
  filter(`5` == max(`5`, na.rm=TRUE))

# pull out the best birds
best_birds<-best_birds %>%
  filter(`1` == max(`1`, na.rm=TRUE))

# get image urls of the birds ###################
YEP<-'http://savetheantarctica.weebly.com/uploads/1/0/5/6/10564199/7022688.jpg'
KP <-"https://cdn.weasyl.com/static/media/51/7c/fb/517cfb9d19bf4e092826bba88f983aed83cdf788d5fae83ce899769adaf477e5.png"
KA<-"https://www.birdpx.com/uploads/images/species/new-zealand-kaka.png"
Kiwi<-"http://www.pngmart.com/files/8/Kiwi-Bird-Transparent-Background.png"
Mo<-"https://ih1.redbubble.net/image.346596392.0568/flat,1000x1000,075,f.u2.jpg"

## add a column for pics
best_birds$pic[which(best_birds$bird_breed=="Yellow-eyed penguin")]<-YEP
best_birds$pic[which(best_birds$bird_breed==unique(best_birds$bird_breed)[2])]<-KP


worst_birds$pic[which(worst_birds$bird_breed=="Yellow-eyed penguin")]<-YEP
worst_birds$pic[which(worst_birds$bird_breed==unique(worst_birds$bird_breed)[4])]<-KP
worst_birds$pic[which(worst_birds$bird_breed==unique(worst_birds$bird_breed)[2])]<-KA
worst_birds$pic[which(worst_birds$bird_breed==unique(worst_birds$bird_breed)[3])]<-Kiwi
worst_birds$pic[which(worst_birds$bird_breed==unique(worst_birds$bird_breed)[5])]<-Mo

#### make a clock of the birds.  Clock code modified from https://masalmon.eu/2017/08/12/wineoclock/ ##################
clock <- ggplot(best_birds, aes(xmin = hour,
                               xmax = hour + 0.03, 
                               ymin = 0,
                               ymax = 0))+
  geom_rect()+
  theme_bw()+
  coord_polar()+
  scale_x_continuous(limits = c(0,24), 
                     breaks = 0:23)+
  scale_y_continuous(limits=c(0,1.1)) +
  coord_polar()+
  geom_segment(x = 7, y = 0, xend = 7, yend = 0.5, colour = "black", size = 0.75, alpha = 0.3)+ # add hands
  geom_segment(x = 0, y = 0, xend = 0, yend = 1, colour = "black", size = 0.75, alpha = 0.3)+ # add hands
  geom_point(aes(0,0), size = 5)

# Best birds clock
best_clock<-clock+  geom_image(aes(image=pic, x = hour,
                 y = 1), size=.10) +
  theme(legend.position="none",
                       axis.text.y = element_blank(),
                       text = element_text(size=24),
                       axis.ticks.y = element_blank(),
                       axis.title.y = element_blank(),
                       axis.title.x = element_blank(),
                       panel.background = element_blank(),
                       panel.border = element_blank(),
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       plot.background = element_blank(),
                       plot.title = element_text(hjust = 0.5),
                       plot.subtitle = element_text(hjust = 0.5))+
  labs(title = "Best birds by hour", subtitle = "Birds with the most 1's selected")
  
# worst birds clock
worst_clock<-clock+ geom_image(data = worst_birds, aes(image=pic, x = hour,
                                   y = 1), size=.10) +
  theme(legend.position="none",
        axis.text.y = element_blank(),
        text = element_text(size=24),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))+
    labs(title = "Worst birds by hour", subtitle = "Birds with the most 5's selected")

## make a plot grid

all_clocks<-plot_grid(best_clock, worst_clock)
ggsave(filename = 'Birds_111919/birdclocks.png', plot = all_clocks, width = 5, height = 5)
