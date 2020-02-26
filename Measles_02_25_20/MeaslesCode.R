# By Nyssa Silbiger
# 2/25/2020

#load libraries
library(tidyverse)
library(geofacet)
library(ggtext)
library(hrbrthemes)
library(extrafont)

# Download the data
measles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-25/measles.csv')


## get percentage of students that did not get the mmr shot for religious, medical, or personal reasons accross all states
measles %>%
  mutate(xrel = as.numeric(xrel))%>% # it is showing up as a logical instead of a value
  replace_na(list(xmed = 0, xrel = 0, xper = 0, enroll = 0, overall = 0, mmr = 0))%>% # replace all NA with 0
  select(-lat, -lng, -index, -enroll, -overall)%>% #remove lat and long
  pivot_longer(cols = xrel:xper, names_to = "Reason", values_to = "percentage") %>% # have all the reasons together for the barplot below
  group_by(state, Reason) %>%
  summarise_if(is.numeric, list(~mean(.), ~sd(.)/sqrt(n()))) %>% # get means and SE
  rename(SE = `percentage_/`) %>% # rename the SE column
  ggplot(aes(x = Reason, y = percentage_mean, color = Reason, fill = Reason))+
  geom_bar(stat = 'identity')+
  geom_errorbar(aes(ymin = percentage_mean - SE, ymax = percentage_mean+SE), color = "white", width = 0)+
  theme_ft_rc() + # black background theme
  labs(caption = "Plot by N. Silbiger \n@nsilbiger \nData by the Wallstreet Journal")+
  theme(axis.title.x=element_blank(),  # remove xlabels
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_markdown(lineheight = 1.1),
        legend.position = "none")+
  xlab("")+ylab("")+
  labs(title = "<b>Mean prcentage of students that refused vaccines due to <span style = 'color:#ff6666;'> medical </span>,<span style = 'color:#00ff00;'> personal </span>, or <span style = 'color:#6699ff;'> religious </span> reasons.</b><br>")+
  facet_geo(~ state)+ # facet wrap it by state
  ggsave('Measles_02_25_20/measlesplot.png', width = 18, height = 10)

