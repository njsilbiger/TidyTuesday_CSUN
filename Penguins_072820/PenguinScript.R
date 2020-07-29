### Tidy Tuesday Penguins ####
### By Nyssa Silbiger ####
### 7/28/2020 #####
########################


### load libraries #####
library(tidyverse)
library(janitor)
library(hrbrthemes)
library(ggimage)
library(ggtext)

### Load the data #########
penguins_raw.csv <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-28/penguins_raw.csv')

### let's do something with the stable isotope data ##### 

adelie<-"https://e7.pngegg.com/pngimages/110/920/png-clipart-emperor-penguin-bird-desktop-arctic-penguin-animals-club-penguin-thumbnail.png"
strap<-"https://img2.pngio.com/penguin-png-images-free-download-winter-penguin-clipart-free-chinstrap-penguin-png-445_445.png"  
gentoo<-""


penguins_raw.csv %>%
  drop_na(`Delta 15 N (o/oo)`) %>%
  clean_names() %>%
  ggplot(aes(x = delta_13_c_o_oo, y = delta_15_n_o_oo, group = species, color = species))+
  geom_point()+
  stat_ellipse()+
  #geom_image(aes(x = -27, y = 9, image = adelie), size=0.25 )+
  #geom_image(aes(x = -25.5, y = 9.5, image = strap size=0.05))+
  xlab(bquote(delta^13~"C"))+
  ylab(bquote(delta^15~"N"))+
  annotate(geom = "text", x = -27, y =8, label = "Gentoo", color = "#067076", size = 10)+
  annotate(geom = "text", x = -25.5, y =9.25, label = "Adelie", color = "#FF7A00", size = 10)+
  annotate(geom = "text", x = -24.5, y =9.7, label = "Chinstrap", color = "#C35DCA", size = 10)+
  scale_color_manual(values=c("#FF7A00","#C35DCA","#067076"), 
                     labels = c("Adelie Penguin", "Chinstrap penguin", "Gentoo penguin"),
                                                                   name="") +
  theme_ft_rc()
                    

unique(penguins_raw.csv$species)



