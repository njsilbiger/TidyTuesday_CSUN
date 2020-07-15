### Tidy Tuesday Astronauts ###
### 7/14/2020 ###
### By Nyssa Silbiger 

#### load libraries #####
library(tidyverse)
library(viridis)
library(hrbrthemes)
library(gganimate)
library(ggimage)
library(ggpubr)
library(png)

### load data #####
astronauts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-14/astronauts.csv')

## Hours in space flight by year and nationality

## Image for outer space
img.file <- system.file(file.path("071420_astronauts", "EarthRender.png"),
                        package = "ggpubr")
img <- png::readPNG(file.path("071420_astronauts", "EarthRender.png"))

# space ship image
ship<-"https://img.pngio.com/spaceship-png-images-transparent-free-download-pngmartcom-spaceshippng-512_512.png"

## get the total number of hours in space
astrodata<-astronauts %>%
  group_by(year_of_mission, nationality) %>%
  summarise_at(.vars = c("hours_mission", "total_number_of_missions"), .funs  = ~sum(.))
  
## find the max hours in space to make a rocket ship
rockethours<-astrodata %>%
  group_by(year_of_mission)%>%
  summarise(max.hours = sum(hours_mission) + 5000) # add values for rocket ship placement
  
#Make a tibble the says blast off in the last year
labels<-rockethours %>%
  filter(year_of_mission == max(year_of_mission)) %>%
  mutate(max.hours = 80000,
         label = "Blast off")

# make a silly animation of the total number of hours in space  
anim<-ggplot()+
  background_image(img)+ # add space image
  geom_bar(data = astrodata,position="stack", stat="identity",
            aes(x = year_of_mission, y = hours_mission, fill = nationality))+
  geom_image(data = rockethours, aes(x = year_of_mission, y = max.hours, image = ship), size=.05)+ # this add the coffee bean image
  ylab("Total mission hours")+
  xlab("Year of Mission")+
  ggtitle("Hours in space with colors by nationality")+
  scale_fill_viridis(option="magma", discrete = TRUE)+
  geom_text(data = labels, aes(x =year_of_mission-10, y = max.hours, label =label), size  = 12)+
  theme_ft_rc()+
  theme(legend.position = "none")+
  transition_manual(year_of_mission) +
  shadow_trail()
  
# save it with a freeze at the end
anim2<- animate(anim, fps = 5, 
          renderer = gifski_renderer(loop = FALSE), 
          height = 600, width = 900)
anim_save("071420_astronauts/astro.gif", animation = anim2)

