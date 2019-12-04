### Philly parking ticket data
### 12/3/2019


### load library ####

library(tidyverse)
library(lubridate)
library(cowplot)
library(treemapify)
# read the data
tickets <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-03/tickets.csv")


## let's look at the % of parking tickets by hour of data and by month in 2017
# by month
ticketmonth<-tickets %>%
  mutate(month = as.factor(month(issue_datetime, label = TRUE)),
         hour = as.factor(hour(issue_datetime))) %>%
  group_by(month)%>%
  tally()%>%
  mutate(label = paste0(month,"\n", n)) # add the ticket number to the label

# by hour
tickethour<-tickets %>%
  mutate(month = as.factor(month(issue_datetime, label = TRUE)),
         hour = as.factor(hour(issue_datetime))) %>%
  group_by(hour)%>%
  tally()%>%
  mutate(label = paste0(hour,":00 \n", n)) # add the ticket number to the label

# tree map by month
monthplot<-ggplot(ticketmonth, aes(area = n, fill = month, label = label, alpha = 0.5)) +
  geom_treemap()+
  geom_treemap_text(place = "center")+
  guides(fill = FALSE, alpha = FALSE)+
  ggtitle("By month")
  
# treemap by hour
hourplot<-ggplot(tickethour, aes(area = n, fill = hour, label = label, alpha = 0.5)) +
  geom_treemap()+
  geom_treemap_text(place = "center")+
  guides(fill = FALSE, alpha = FALSE)+
  ggtitle("By time of day")

# add a super title to the plot
title <- ggdraw() + draw_label("Number of parking tickets in Philly (2017)", fontface='bold')

# use plot_grid to bring the plots together
BothPlots<-plot_grid(monthplot, hourplot)
AllPlots<-plot_grid(title, BothPlots, align = "v", nrow = 2, rel_heights=c(0.1, 1))
ggsave("Philly_parking_tickets/ticketplots.png", width = 14, height = 8) # save it
