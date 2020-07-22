### Australian Animals####
### By Nyssa Silbiger ######
### 07/21/2020 ############



### load the data #####
library(tidyverse)
library(janitor)
library(CatterPlots)
library(ggimage)
library(calecopal)
library(patchwork)



#### load the data ######
#animal_outcomes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-21/animal_outcomes.csv')
animal_complaints <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-21/animal_complaints.csv')
#brisbane_complaints <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-21/brisbane_complaints.csv')


### let's do something fun with the complaints #####

catdata<-animal_complaints %>%
  clean_names() %>% # fix the header names
  separate(col = date_received, into = c("month","year"))%>%
  group_by(animal_type, complaint_type) %>%
  tally() %>%
  filter(animal_type == 'cat') %>%
  mutate(fraction = n/sum(n), # Compute percentages
         ymax = cumsum (fraction), # Compute the cumulative percentages (top of each rectangle)
         ymin =c(0, head(ymax, n=-1)), # Compute the bottom of each rectangle
         labelPosition  = (ymax + ymin)/2)  
  

dogdata<-animal_complaints %>%
  clean_names() %>% # fix the header names
  separate(col = date_received, into = c("month","year"))%>%
  group_by(animal_type, complaint_type) %>%
  tally() %>%
  filter(animal_type == 'dog') %>%
  mutate(complaint_type = 
           case_when(complaint_type == "Aggressive Animal" ~ "Aggressive \nAnimal",
                     complaint_type == "Private Impound" ~ "Private \nImpound",
                      TRUE ~ complaint_type)) %>%
  mutate(fraction = n/sum(n), # Compute percentages
         ymax = cumsum (fraction), # Compute the cumulative percentages (top of each rectangle)
         ymin =c(0, head(ymax, n=-1)), # Compute the bottom of each rectangle
         labelPosition  = (ymax + ymin)/2)  

catimage<-"https://pbs.twimg.com/media/EYf5xu1UwAYDLNM.jpg" # this is my crazy cat, Pua
# Make the cat plot

catplot<-ggplot(catdata, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=complaint_type)) +
  geom_rect() +
  geom_text( x=4.5, aes(y=labelPosition, label=complaint_type, color=complaint_type), size=4) + # x here controls label position (inner / outer)
  scale_fill_manual(values = cal_palette("sierra1"))+
  scale_color_manual(values = cal_palette("sierra1"))+
  coord_polar(theta="y") +
  xlim(c(-1, 4)) +
  labs(caption = "Cat complaints")+
  geom_image(aes(x = -0.8, y = 0, image = catimage), size=0.45)+ # this add the image
  theme_void() +
  theme(legend.position = "none",
        plot.caption =element_text(hjust = 0.5, size = 18))

dogimage<-"https://www.anythingfrenchbulldog.com/wp-content/uploads/2019/11/how-often-do-french-bulldogs-poop.jpg"

dogplot<-ggplot(dogdata, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=complaint_type)) +
  geom_rect() +
  geom_text(x=5.5, aes(y=labelPosition, label=complaint_type, color=complaint_type), size=4) + # x here controls label position (inner / outer)
  scale_fill_manual(values = cal_palette("chaparral1"))+
  scale_color_manual(values = cal_palette("chaparral1"))+
  coord_polar(theta="y") +
  xlim(c(-2, 5)) +
  labs(caption = "Dog complaints")+
  theme_void() +
  geom_image(aes(x = -1.5, y = 0, image = dogimage), size=0.45)+ # this add the image
  theme(legend.position = "none",
        plot.caption = element_text(hjust = 0.5, size = 18))

# bring it together in patchwork
jointplot<-catplot +dogplot +
  plot_annotation(title = "Animal Complaints in Australian Suburbs") & theme(plot.title = element_text(hjust = 0.5, size = 20))
                                                                            

## save it
ggsave(filename = "Australian_animals_072120/OzAnimalplot.png", plot = jointplot, width = 10, height = 6)
