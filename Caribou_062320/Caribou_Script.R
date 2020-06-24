### Tidy Tuesday Caribou #####
### By Nyssa Silbiger ###
### 6/23/20 ###


#### Load libraries ####
library(tidytuesdayR)
library(tidyverse)
library(ggmap)
library(raster)
library(mapproj)
library(calecopal)
library(magick)
library(cowplot)
library(ggpomological)

### Load data ####
tuesdata <- tidytuesdayR::tt_load('2020-06-23')

location <- tuesdata$locations

rm(tuesdata) # such a big file.  Remove the rest
#### Analysis ####
## Let's try to map the tracks of some animals. 
## How many are there?
unique(location$animal_id) #260

# use the raster package to get a map of the regions that I am interested in
provinces <- c("British Columbia")
canada <- getData("GADM",country="CAN",level=1)
ca.provinces <- canada[canada$NAME_1 %in% provinces,]

## make a color pallete
pal1<-cal_palette("sierra1", n = 260, type = "continuous")

basemap<-ggplot(ca.provinces,aes(x=long,y=lat,group=group))+
  geom_path()+
  geom_polygon(fill = 'beige')+
  coord_map()+
  geom_line(data = location, aes(x  =longitude, y = latitude, color = animal_id, group = animal_id)) +
  scale_color_manual(values = pal1) +
  xlab("")+
  ylab("")+
  labs(caption = "Plot by @Nsilbiger",
       title = "Tracks of 260 Caribou in British Columbia")+
  theme_pomological_plain()+
  theme(legend.position = "none")
  
## put a caribou on it!
img<-"https://cdn.dribbble.com/users/35633/screenshots/1962802/dribbble_mathieubeaulieu_endangeredspecies-southernmountaincaribou.png"

logo_raw <- image_read(img)

## paste together the image with the plot
ggdraw() +
  draw_plot(basemap)+
  draw_image(logo_raw, scale = .2, x = -0.25, y = -0.18) +
  ggsave(filename = "Caribou_062320/cariboumap.png", width = 6, height = 6)
