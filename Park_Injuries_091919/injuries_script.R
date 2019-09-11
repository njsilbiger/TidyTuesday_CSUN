##Amusement park injuries

## load libraries
library(tidyverse)
library(scales)
library(magick)
library(magrittr) # For piping the logo
library(cowplot)
library(usmap)
library(lubridate)
library("ggimage")

## download the data
safer_parks <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-10/saferparks.csv")

##### look at safer parks data and see where the most injuries are
# sum by state and year
states<-safer_parks %>%
  group_by(acc_state,year ) %>%
  summarise(total = sum(num_injured)) # get the total number per year and state

# get the density
state.density<-states %>% 
 group_by(acc_state) %>% # now get the average by state per year
  summarise(density = mean(total))%>% 
  arrange(desc(density))

colnames(state.density)[1]<-"abbr" # rename to work with usmaps

## Make a map colored by most injuries across the states then have a subplot showing 
## the most dangerous device types 
data("statepop") # bring in the data from the US maps code

states.all<-left_join(state.density, statepop)

map1<-plot_usmap(data = states.all, values = "density", lines = "black") + 
  scale_fill_continuous(
    low = "#F7F4F9", high = "#67001F", name = "Mean # of injuries per year", label = scales::comma
  ) + theme(legend.position = "right")


## now lets look at just california and see what the top 5 most dangerous rides are

device.sums<-safer_parks %>%
  filter(acc_state=='CA')%>%
  group_by(device_type) %>%
  summarise(total = sum(num_injured))%>%
  filter(total>100)%>%
  arrange(desc(total))%>%
  mutate(device_type=factor(device_type))
  

## add images to the dataframe
device.sums$pics<-c("https://www.tripsavvy.com/thmb/TOWTFw8YDSd124pDFL91WnGfTF8=/950x0/filters:no_upscale():max_bytes(150000):strip_icc():format(webp)/Six-Flags-Great-America-Skyline-56a952653df78cf772a5d3eb.jpg",
  "https://ca-times.brightspotcdn.com/dims4/default/207e765/2147483647/strip/true/crop/1280x720+0+0/resize/840x473!/quality/90/?url=https%3A%2F%2Fca-times.brightspotcdn.com%2F0c%2F74%2Fb153f60954ffe85b625fe9732e3a%2Fla-1548372579-ou4r6j2yxz-snap-image",
  "https://www.whitewaterwest.com/drive/uploads/2017/08/IMG_4400-1040x690.jpg",
  "https://media-cdn.tripadvisor.com/media/photo-s/05/ef/5a/d8/gilroy-gardens-family.jpg",
  "https://upload.wikimedia.org/wikipedia/commons/thumb/0/0b/Flume_Ride_-_tom_gondol.JPG/1600px-Flume_Ride_-_tom_gondol.JPG",
  "https://www.mensjournal.com/wp-content/uploads/gettyimages-454781877-5770e49b-00a7-42dd-9764-33bac3d6c9bc.jpg?crop=0px%2C254px%2C4928px%2C2772px&resize=2400%2C1350"
  )

## make a lollipop chart showing rides with >100 injuries
danger<-ggplot(device.sums,aes(x=reorder(device_type,total),y = total)) +
  geom_point() +
  geom_segment( aes(x=device_type, xend=device_type, y=0, yend=total )) +
  coord_flip() +
  ylab('Total number of injuries')+
  xlab('')+
  ggtitle('Most Dangerous Rides in CA')+
  theme(
    legend.position="none"
  ) +
geom_image(aes(image=pics), size=.15)

# plot together
grid<-plot_grid(map1, danger,labels=c('A', 'B'), ncol = 1)
#Save it
ggsave(filename = 'Park_Injuries_091919/output.png',plot = grid ,
       width = 8, height = 10)
