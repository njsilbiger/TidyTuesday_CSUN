### Data from Tour de France
### Tidy Tuesday
### By Nyssa Silbiger

##################

# load libraries
library(tidyverse)
library(hrbrthemes)
library(lubridate)
library(RColorBrewer)
library(ggsci)
library(wesanderson)
library(magick)
library(patchwork)
library(cowplot)
library(ggwordcloud)
library(ggtext)
library(RCurl)

### load data
tdf_winners <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-07/tdf_winners.csv')

## First make a plot of winner speed over year
p1<-tdf_winners %>%
  mutate(Year = year(start_date))%>%
  ggplot(aes(x =Year, y = time_overall ))+
  geom_point()+
  geom_smooth(method="glm",
              method.args=list(family=gaussian(link="log")))+
  ylab("Time to finish (hours)")+
  ggtitle("Time to finish wins versus year")+
  theme(plot.title = element_text(hjust = 0.5, size = 14))

### plot stage wins by time total
p2<-tdf_winners %>%
  mutate(Year = year(start_date))%>%
  ggplot(aes(y =stage_wins, x = Year ))+
  geom_point()+
  xlab("Year")+
  ylab("Number of stages won")+
  ggtitle("Stages wins versus year")+
  theme(plot.title = element_text(hjust = 0.5, size = 14))

## make donut plot of winers by nationality
countrydata<-tdf_winners %>%
  count(nationality) %>%
  mutate(nationality = str_trim(nationality, side = c("both")))%>% #remove white space
  mutate(fraction = n/sum(n),
         ymax=cumsum(fraction),   #top
         ymin = c(0,head(ymax, n=-1)), #bottom
         labelPosition = (ymax+ymin)/2) #label position
        # label = paste0(nationality, "\n value: ",n))  # pretty label
## only pull out a few for the labels  
labels <- countrydata %>%
  filter(nationality %in% c("France", "Belgium", "Italy", "United States", "Spain")) %>%
  mutate(labels = nationality) 

countrydata<-left_join(countrydata, labels)
  
# Make the plot
p3<-  ggplot(countrydata, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=nationality)) +
    geom_rect() +
    geom_text(x=2,aes( y=labelPosition, label=labels, color=nationality), size=6) + # x here controls label position (inner / outer)
    scale_fill_manual(values=c(wes_palette("Darjeeling1"),wes_palette("Darjeeling2"),wes_palette("FantasticFox1"))) +
    scale_color_manual(values=c(wes_palette("Darjeeling1"),wes_palette("Darjeeling2"),wes_palette("FantasticFox1"))) +
    coord_polar(theta="y") +
    xlim(c(-1, 4)) +
    ggtitle("France has the most winners, then Belgium")+
    theme_void() +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5, size = 14))
       
# make a word cloud in the shape of a bike
p4<-tdf_winners %>%
  mutate(nickname = as.character(nickname),
         nickname = trimws(nickname,which="both")) %>%
  group_by(nickname) %>%
  summarize(N = n()) %>%
  mutate(angle = 45 * sample(-2:2, n(), replace = TRUE, prob = c(1, 1, 4, 1, 1)))%>%
  ungroup()%>%
  ggplot(aes(label = nickname, size = N, angle = angle, color = nickname)) +
  geom_text_wordcloud_area() +
  scale_size_area(max_size = 25) +
  ggtitle("Le Boss, Big Tex wins a lot")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, size = 14))
  
## bring the plots togehter
(p3+p4) /(p1+p2) + plot_annotation("They're getting faster!")+
  ggsave("TourdeFrance_040720/tourfranceplot.png", width = 12, height = 10)
  
  