### Moore's Law data
## Nyssa Silbiger
## 9/3/2019

## load libraries
library(tidyverse)
library(scales)
library(extrafont)
font_import() # for changing fonts on the plot
loadfonts(device = "win")
library(magick)
library(magrittr) # For piping the logo
library(cowplot)
library(gganimate)
library(gifski)
library(transformr)
library("emojifont")


## Read in the Data ####

cpu <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-03/cpu.csv")
gpu <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-03/gpu.csv")
ram <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-03/ram.csv")
# Number of designers of cpu over time
No.designers.cpu<-cpu %>%
  drop_na()%>%
  select(designer, date_of_introduction)%>%
  group_by(date_of_introduction)%>%
  tally()

# Same for GPU
No.designers.gpu<-gpu %>%
  drop_na()%>%
  select(designer_s, date_of_introduction)%>%
  group_by(date_of_introduction)%>%
  tally()


## how does number of CPU designers change over time
dataplot<-ggplot(No.designers.cpu, aes(
  x = date_of_introduction,
  y = n))+
  geom_text(aes(label = emoji('heart'), family = "EmojiOne"))+ # won't show up :(
  geom_line()+
  ylab('No. of CPU Designers')+
  xlab('Year')+
  theme(text=element_text(size=14,  family="Courier New"))+
  transition_reveal(date_of_introduction) 
  
anim_save(animation = dataplot, filename = "MooresLab_9_3_19/output.gif")
