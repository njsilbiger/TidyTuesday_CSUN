
## National park visits 
# By Nyssa Silbiger
# 9/17/2019


### load the libraries
library(tidyverse)
library(sf)
library(usmap)
library(gganimate)
library(transformr)
library(scales)

### Read in the data

park_visits <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-17/national_parks.csv")


## Make a map of park visits by state that changes over time
## first sum up all the all the visits by state and year
visitors.year<-park_visits %>%
  group_by(state, year)%>%
  summarise(total = sum(visitors))

# rename the state to abbr so that I can left join it with the map data
colnames(visitors.year)[1]<-'abbr'

# look at 1 year to start
test<-visitors.year%>%
  filter(year=='1930')

# USA data
state_map <- us_map(regions = "states")

#join the data
mapdata<-left_join(state_map, visitors.year)

# make year a numeric
mapdata$year<-as.integer(mapdata$year)

# remove NA in years
mapdata_clean<-mapdata %>%
  filter(!is.na(year))

# make an animation plot that shows the total visits per year 
p<-ggplot(data = mapdata_clean,
       mapping = aes(x = long, y = lat,
                     fill = total, 
                     group = group))+
  geom_polygon(color = "gray90", size = 0.05) +
  coord_equal()+
  scale_fill_gradient(low = 'lightblue', high = 'mediumpurple4',
                      trans = 'log10',  # make the colors log base 10
                      breaks = trans_breaks('log10', function(x) 10^x),
                      labels = trans_format('log10', math_format(10^.x)))+
  labs(fill = "National Park Visits")+
  theme_void()+
  transition_time(year) +
  labs(title = "Year: {frame_time}") 

animate(p, fps = 5, width = 800, height = 400)
anim_save("NationalParks/parks.gif", p)

