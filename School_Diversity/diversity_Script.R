### Looking at trends in School diversity from the 90s to present ####

## Script by Nyssa Silbiger
# Date 9/24/19

### Load libraries ######
library(tidyverse)
library(ggthemes)
library(ggrepel)


### read the data

school_diversity <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-24/school_diversity.csv")


### make a segment plot for the states to see if they have improved or gotten worse over time 

## rank the factors for school year and also diversity
school_diversity$diverse<- factor(school_diversity$diverse, ordered = TRUE, 
                                  levels = c( "Extremely undiverse", "Undiverse", "Diverse"))


### Make a segment plot where if diversity increased the color is blue, stayed the same its black, and decreased its red
school_diversity$diverse_num<-as.numeric(school_diversity$diverse)

# Only look at large city schools in CA
sub_diversity<-school_diversity %>%
  filter(ST =='CA', d_Locale_Txt == 'city-large')%>% # large city schools in CA
  group_by(diverse, diverse_num, SCHOOL_YEAR)

## add some random values to jitter the 
sub_diversity$diverse_num[sub_diversity$SCHOOL_YEAR=='1994-1995']<-sub_diversity$diverse_num[sub_diversity$SCHOOL_YEAR=='1994-1995']+runif(n = length(sub_diversity), min = -0.4, max = 0.4)

# make another dataframe with just the 1994 data for the labels
sub_diversity2<-sub_diversity[sub_diversity$SCHOOL_YEAR=='1994-1995',]

#point vector
d2<-data.frame(x = c(2,2,2), y = c(1,2,3))

## Also look at difference between 1995 and 2016
sub_diversity%>%
  group_by(LEA_NAME, SCHOOL_YEAR)%>% # group by school and year
  summarise(mean = mean(diverse_num, na.rm = TRUE))%>% # make it so there is one per column
  spread(SCHOOL_YEAR, mean)%>% # spread the columns by year
  mutate(diff = as.factor(round(`2016-2017` - `1994-1995` ))) %>%# subtract the 2 and round so that I can use it for colors
  na.omit()%>%
  tail(n = 27)%>%
  ggplot(aes(y = `1994-1995`, yend =`2016-2017`, x = 1, xend = 2))+
  geom_segment(aes(color = diff))+
  geom_point(aes(color = diff))+
  geom_text_repel(aes(x = 1, y = `1994-1995`, label =LEA_NAME, color = diff ), nudge_x = -0.25, force = 3,nudge_y = jitter(x = 0,factor = 0.2 ))+
  scale_color_manual(values=c("red", "grey", "lightblue", "darkblue"), name = 'Change in diversity',
                     label = c("Lower Diversity", "Stayed the same", "Increased a little", "Increased a lot"))+
  scale_x_continuous(breaks=c(1,2),
                   labels=c("1994 - 1996", "2016 - 2017"), name = "", limits = c(-0.75, 2) )+
  scale_y_continuous(breaks = c(1,2,3),labels=c("Extremely undiverse","Undiverse", "Diverse"), name = "", limits = c(1,3.5))+
  ggtitle("Change in diversity of large city schools in CA")+
  theme(axis.text=element_text(size=16,face="bold"))+
  theme_solarized() +
  ggsave('School_Diversity/Diversity.png')
  

  
#position = position_nudge (y = jitter(x = 0,factor = 0.2)
