

### Horror movies by budget

###
library(tidyverse)
library(lubridate)
library("quantmod") # currency conversions
library(stringr)
library(magick)
library(magrittr) # For piping the logo
library(cowplot)
library(png)

# read the data
horror_movies <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-22/horror_movies.csv")


### horrow movies in the us
horror_clean<-horror_movies %>%
  filter(release_country=='USA')%>% # filter movies released in the US
  drop_na(movie_run_time) %>% # remove NAs that don't have data on run time
  separate(movie_run_time, into = c("time", "min"), sep = " ") %>% # remove the minutes
  mutate(time = as.numeric(time)) %>% # make time numeric
 # mutate(release_date2 = dmy(release_date)) %>%
  separate(title, into = c('title', 'year'), sep="([\\(\\)])") %>% # pull out the year since the dates are terrible
  mutate(year = as.numeric(year))%>%
  drop_na(year)

## get averages by year
horror_clean %>%
  group_by(year)%>%
  summarise(mean_time = mean(time)) %>%
  ggplot( aes(x = year, y = mean_time))+
  geom_bar(stat = "identity")


## highest rated movie by release country
Best_horrow_movies<-horror_movies %>%
  group_by(release_country)%>%
  filter(review_rating == max(review_rating, na.rm=TRUE)) %>% # put out the movies with the highest ratings by country
  drop_na(budget) %>%
  mutate(amount =parse_number(budget))%>% # pull out the numbers
  separate(budget, into = c('currency'), sep="(?=[:digit:])") # pull out the currencies
  

# change the euro and pound symbols
Best_horrow_movies$currency[which(Best_horrow_movies$currency =="€")]<-'EUR'
Best_horrow_movies$currency[which(Best_horrow_movies$currency =="$")]<-'USD'
Best_horrow_movies$currency[which(Best_horrow_movies$currency =="£")]<-'GBP'

## get currency conversions
currDF  <- data.frame(
  date = seq.Date(from = as.Date("2019-10-22"), to = as.Date("2019-10-22"), length = 10),
  currency = c('EUR', 'INR', 'DKK', 'SGD', 'THB', 'NGN', 'HUF', 'NZD', 'KRW', 'GBP'),
  amount = 1:10,stringsAsFactors=FALSE)
currDF

currCombinations<-paste(setdiff(unique(currDF$currency),"USD"),"USD",sep="/")

#Set start and end dates
startDt = as.Date("2019-10-22")
endDt = as.Date("2019-10-22")

fxData <- do.call(merge.xts,lapply(currCombinations,function(x) 
  getFX(x,from=startDt,to=endDt,auto.assign=FALSE)))
## convert currencies to $$

# traspose the dataset and make the rownames a column for currency
fxData<-as.data.frame(t(fxData)) # transform the data
fxData$currency<-row.names(fxData) # pull out the row names
colnames(fxData)[1]<-'Rate'  # rename the column
rownames(fxData) <- c() # remove rowname

# remove the .usd
fxData<-fxData %>%
  separate(col = currency, into = "currency", sep = "\\.")%>%
  add_row(Rate = 1, currency = "USD") # add USD

Best_horrow_movies$currency<-str_trim(Best_horrow_movies$currency) # trim the white space in the word

Best_horror_movies<-left_join(Best_horrow_movies, fxData)

## now convert everything to $$
Best_horror_movies$dollars<-Best_horror_movies$amount*Best_horror_movies$Rate
Best_horror_movies$tempvar <- "Cost of highest rated horror movie by country" # helps with the background

dataplot<-Best_horror_movies%>%
 ggplot(aes(x = reorder(release_country,dollars), y = dollars))+
  geom_bar(stat = "identity", color = 'yellow')+
  coord_flip()+
  theme_classic()+
  ylab('Budget in USD')+
  xlab("")+
 # ggtitle("Cost of highest rated horror movie by country")+
  theme(panel.background = element_rect(fill = "transparent"),
        plot.title = element_text(color="yellow", size=15, face="bold"),
        axis.text.x = element_text(color = "white",size=15, face="bold"),
        axis.text.y = element_text(size=15, face="bold"),
        axis.title.x = element_text(color="white", size=15, face="bold"),
          strip.background = element_rect(fill="black"),
        strip.text = element_text(colour = 'yellow', size = 15, face = "bold"))+
  facet_grid(~tempvar)
  


# add a picture of bart simpson as the background)
img<-"https://pixel.nymag.com/imgs/daily/vulture/2019/07/10/10-momo.w330.h330.2x.jpg"

horrorimg<-ggbackground(dataplot, img)

ggsave(filename ="Horror_movies/horrorimg.png",plot = horrorimg)
