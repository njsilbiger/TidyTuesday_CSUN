
### Data on walking and biking for Tidy Tuesday
## By Nyssa Silbiger


### Load libraries ######
library(tidyverse)
library(ggmap)
library(sf)
#library(tmap)

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

 # make walk and bike its own columns so that I can look at relative values for each city
 
 # 
 commute_mode_sf <- commute_mode %>%
   st_as_sf(coords = c("lon", "lat"), remove=FALSE)
# 
 #saveRDS(commute_mode_sf, "Biking_Walking_110519/acs_bike_commute_tidy_sf.RDS") # save the sf object
 #write.csv(x = commute_mode, file = "Biking_Walking_110519/commute.csv") # save the csv
 
commute_mode_sf <- readRDS("Biking_Walking_110519/acs_bike_commute_tidy_sf.RDS")
 
### need to think about how to do this better. maybe select on columns I need.
 commute_mode_wider <- commute_mode %>% 
   group_by(city)%>%
   pivot_wider(               # transform function
     names_from = mode,     # vector to transform from column to row
     values_from = n        # values to transform with the above vector from down the column to across the rows
   ) %>%
   replace(is.na(.),0) # replace all NA values with 0. Takes care of any inconsistencies in data input
 