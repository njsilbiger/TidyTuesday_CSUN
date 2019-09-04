# First day of tidy tuesday
# Nyssa Silbiger
# 8/27/19

## load library #########
library('tidyverse')
library(magick)
library(magrittr) # For piping the logo
library(cowplot)

### Read data #################
simpsons <- readr::read_delim("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-27/simpsons-guests.csv", delim = "|", quote = "")

## Tidy data ########

# count the number of guest starts by episode
SData<-simpsons %>%
  mutate(season = as.numeric(season))%>%
  group_by(season) %>%
  tally()%>%
  drop_na() # remove NA

# plot data ######
dataplot<-ggplot(SData, aes(x = season, y = n))+
  geom_point()+
  geom_line()+
  xlab("Season number")+
  ylab("Number of guest stars")+
  theme_bw()

# add a picture of bart simpson as the background)
Simpsonplot<-ggdraw() +
  draw_plot(dataplot)+
  draw_image("https://upload.wikimedia.org/wikipedia/en/a/aa/Bart_Simpson_200px.png") 
  
ggsave(filename = '8_27_19/ SimpsonData.png', plot = Simpsonplot)
  
