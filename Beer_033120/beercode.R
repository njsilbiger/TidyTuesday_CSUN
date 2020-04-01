## Beer Data ####
### By Nyssa Silbiger ####
### 3/31/2020 #####

#### Load the Data #############
beer_states <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/beer_states.csv')

### Load library #####
library(tidyverse)
library(viridis)
library(magick)
library(cowplot)
library(ggrepel)
library(patchwork)

## image of beer mug
imgurl<-"https://www.dlf.pt/png/list/14/141262_beer-mug-icon-png.png"

#what are the top 3 and bottom 2 states for total number of barrels across all years
top<-beer_states %>%
  group_by(state)%>%
  summarise(total = sum(barrels, na.rm = TRUE))%>%
  mutate(logtotal = log(total)) %>%
  mutate(number = c(rep(1:10,5),1,2))%>%
  filter(state !='total')%>%
  arrange(desc(total))%>%
  slice(1:3,50:51)

# make a bubble plot of the log barrels by state
dataplot<-beer_states %>%
  group_by(state)%>%
  summarise(total = sum(barrels, na.rm = TRUE))%>%
  mutate(logtotal = log(total))%>%
  mutate(number = c(rep(1:10,5),1,2))%>%
  filter(state !='total')%>%
  droplevels()%>%
  ggplot()+
  geom_point(aes(x = number, y = logtotal, size = logtotal, color = state), alpha = 0.5)+
  scale_color_viridis(option="plasma", discrete  = TRUE)+
  xlim(0,11)+
  ylim(10, 22.0)+
  geom_label_repel(data = top, aes(x = number, y = logtotal, label = state))+
  theme_void()+
  theme(legend.position = "none")

# put it on a beer glass
plot1<-ggdraw() +
  draw_image(imgurl, width = .6, height = .6, x = .2) +
  draw_plot(dataplot, width = 0.35, height = .45, y = .50, x = 0.30)
  
# add some titles
fullplot<-plot1+
  plot_annotation(title = "Size of bubbles is proportional to the \nlog total number of barrels from 2008 - 2019", 
                  subtitle = "Colorodo makes the most beer and North Dakota makes the least beer",
                  caption = "Made by Nyssa Silbiger \nTwitter @NSilbiger") & 
  theme(plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.title = element_text(hjust = 0.5))

# save the plot
  ggsave(fullplot,filename = "Beer_033120/BeerPlot.png", width = 8, height = 6)
