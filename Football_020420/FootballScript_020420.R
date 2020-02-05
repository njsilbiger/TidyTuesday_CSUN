### Plot the Miami Dolphins ratings over time

# Load the libraries
library(tidyverse)
library(emojifont)
library(ggthemes)
library(magick)
library(cowplot)

# Read in the data
standings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/standings.csv')


## which team has the worst at home attendance
footballplot<-standings %>%
  filter(team_name =="Dolphins")%>%
  mutate(label = emoji("football"))%>% # add a football emoji
  ggplot()+
    geom_line(aes(x = year, y = simple_rating))+
    geom_text(aes(x = year, y = simple_rating, label = label), family="EmojiOne", size=6, color = "brown")+
  theme_economist()+
  theme(text = element_text(size=20))+ 
  ggtitle("Miami Dolphins #1!")+
  xlab("Year")+
  ylab("Ratings")
 
# Miami Dolphins logo
logo<-"https://cdn.bleacherreport.net/images/team_logos/328x328/miami_dolphins.png"

# add the dolphins logo 
plot1<-ggdraw() +
  draw_plot(footballplot)+
  draw_image(logo, scale = .5, x = .3, y = .35) 

### add just kidding cuz they are not #1
football2<-footballplot+ 
  annotate("text", x = 2007, y = 4, label = "Just Kidding", color = "red", size = 20)+
  theme(text = element_text(size = 20))

plot2<-ggdraw() +
  draw_plot(football2)+
  draw_image(logo, scale = .5, x = .3, y = .35) 

## read in the images as a gif
newlogo <- image_scale(image_read("Football_020420/Rplot01.png"))
oldlogo <- image_scale(image_read("Football_020420/Rplot.png"))


## Make it a gif
image_resize(c(oldlogo, newlogo)) %>%
  image_background('white') %>%
  image_morph() %>%
  image_animate()%>%
  image_write("Football_020420/MiamiDolphins.gif")
