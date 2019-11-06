
### Data on walking and biking for Tidy Tuesday
## By Nyssa Silbiger


### Load libraries ######
library(tidyverse)
library(ggmap)
library(sf)
library(png)
library(grid)
library(ggrepel)

## read data #####
commute_mode <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-05/commute.csv")


## Help from Tidy Tuesday podcast on cleaning city names
# (?<= ) means "look for a space before this, but don't include it in what we
# extract."
# Square brackets mean "look for any of these", so [a-z() ] means "look for
# lowercase letters, parentheses, or spaces."
# The plus sign means "find at least one of these."
# The dollar sign means "this has to occur at the end of the text."
target <- "(?<= )[a-z() ]+$"

commute_mode<-commute_mode %>% 
  dplyr::mutate(
    type = dplyr::if_else(
      stringr::str_detect(city, target),
      stringr::str_extract(city, target),
      "city"
    ),
    city = stringr::str_trim(stringr::str_replace(city, type, ""))
  )


## help from J Byrnes on how to get city names and lat/longs

#get my apis flowin'
register_google(key = '') ### use your own API
## Info on how here (https://www.r-bloggers.com/geocoding-with-ggmap-and-the-google-api/)

# #download the data and add geocoding
# #note: this takes a while, so, do this once, 

 commute_mode <- commute_mode %>%
   cbind(geocode(paste(commute_mode$city, commute_mode$state, sep = ","))) # pastes the city name and the state, separated by a comma and geocodes it

 
 #write.csv(x = commute_mode, file = "Biking_Walking_110519/commute.csv") # save the csv
 
 # make walk and bike its own columns so that I can look at relative values for each city
 commute_mode_wider <- commute_mode%>% 
   select(city, state, n,mode, lat, lon)%>%
   group_by(city)%>%
   pivot_wider(               # transform function
     names_from = mode,     # vector to transform from column to row
     values_from = n        # values to transform with the above vector from down the column to across the rows
   ) %>%
   replace(is.na(.),0) # replace all NA values with 0. Takes care of any inconsistencies in data input
 
 ## create a columns for log Bike/Walk where positive values are more bikers and negative are less
 commute_mode_wider$log.b.w<-log((commute_mode_wider$Bike+1)/(commute_mode_wider$Walk+1))
   
   
 commute_mode_sf <- commute_mode_wider %>%
   st_as_sf(coords = c("lon", "lat"), remove=FALSE)
 #saveRDS(commute_mode_sf, "Biking_Walking_110519/acs_bike_commute_tidy_sf.RDS") # save the sf object
  #commute_mode_sf <- readRDS("Biking_Walking_110519/acs_bike_commute_tidy_sf.RDS")
 
 ## Make a map
 
 # MAKE A BASEMAP FROM A .SHP FILE:
 # how to load a .shp file into R- this requires {sf}
 states<- st_read(dsn = "Biking_Walking_110519/cb_2017_us_state_20m/cb_2017_us_state_20m/cb_2017_us_state_20m.shp") # load a simple state shape file
 # the above code just tells st_read where the file is stored, currently stored in folder "cb_2017_us_state_20m" and the file name is " cb_2017_us_state_20m.shp"
 
 states<- st_transform(states, crs= 4326) # transform the data from NAD 83 to WGS 84 you probably won't have to worry about this, but its good to know
 st_crs(states) # check that coordinates were transformed- they are now in WGS84
 
 # set the crs
 commute_mode_sf<-commute_mode_sf %>%
   st_set_crs(4326)
 
 # PLOT BASEMAP AND POINTS IN GGPLOT
 p<-commute_mode_sf %>%  # our dataframe with biking and walking info
   ggplot()+
   theme(
     panel.border = element_rect(fill=NA, color="black", size=3), #thick outline of plot
     panel.background = element_rect(fill="cadetblue2"), # set background color
     panel.grid.major = element_line(color=NA))+ # remove gridlines
   geom_sf(data=states, fill="goldenrod", color="black", alpha=0.8, show.legend = FALSE)+
      # add points for our stations where we caught Blue Lanternfish
   geom_point(aes(x=lon, y=lat, color=log.b.w), size=1,alpha=0.8)+
   scale_colour_gradient2(breaks = c(-7, 0, 7),limits = c(-7.5,7),
                          labels = c("More Walkers", "Equal", "More Bikers"))+
   xlab("Longitude")+
   ylab("Latitude")+
   xlim(-128, -55)+ # zoomed region
   ylim(25, 50)+
   labs(colour = " ") +
   theme(legend.key.height = unit(1.9, "cm"),  # move legend and make it transparent
         legend.position = c(0.9, 0.52),
         legend.background = element_blank(),
         legend.box.background = element_blank(),
         legend.key = element_blank())+
   ggtitle("Relative bikers to walkers in major cities")
 
 # reading image
 img <- readPNG("Biking_Walking_110519/walking.png")
 # reading image
 img2 <- readPNG("Biking_Walking_110519/biking.png")
 

 # adding image of walker
 p1<-p + annotation_custom(rasterGrob(img2, width = 2, height = 2), xmin = -60, xmax = -55, ymin = 45, ymax = 50)+
   annotation_custom(rasterGrob(img, width = 2, height = 2), xmin = -60, xmax = -55, ymin = 27, ymax = 32)
   #
 
 # add labels for city with most bikers and most walkers
bike<- which(commute_mode_sf$log.b.w==max(commute_mode_sf$log.b.w))
walk<- which(commute_mode_sf$log.b.w==min(commute_mode_sf$log.b.w))

# create a dataframe with the cities with the most bikers and walkers
BW<-commute_mode_sf[c(bike,walk),] 
BW$most<-c("(Most Bikers)", "(Most Walkers)")
BW$most1<-paste(BW$city, BW$most, sep = " ") 

p1+geom_label_repel(data = BW, aes(x=lon, y=lat,label=most1), size=2, 
                   # nudge_x = -0.03,
                  #  nudge_y = -0.03,
                    segment.size = 2,
                   # direction = "x",
                    segment.color = "grey50",
                    min.segment.length = 2,
                    point.padding = NA,
                    arrow = arrow(length = unit(0.03, "npc"), type = "closed")
                    )+
  ggsave('Biking_Walking_110519/map.png', width = 10, height = 5)
 
 