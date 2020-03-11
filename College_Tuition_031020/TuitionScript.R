### What are america's most expensive schools?
# By Nyssa Silbiger

## load library
library(tidyverse)
#extrafont::loadfonts(device = "win")
library(RColorBrewer)
library(ggsci)
library(wesanderson)

## Inspiration from https://cedricscherer.netlify.com/2019/05/17/the-evolution-of-a-ggplot-ep.-1/

tuition_cost <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/tuition_cost.csv')

# Most expesive tuition (top 5)
top5<-tuition_cost %>%
  arrange(desc(in_state_tuition))%>%
  top_n(5) 
  
### Plot most expensive instate tuition by state
US_avg <-tuition_cost %>% # get average tuition
      summarise(mean = median(in_state_tuition, na.rm=TRUE)) %>%
       pull(mean)

State_mean<-tuition_cost %>% # get  median tuition
  drop_na()%>%
  droplevels()%>%
  group_by(state)%>%
  summarise(state_mean = median(in_state_tuition, na.rm=TRUE)) 

# get a color pallet.... this doesn't look great....  
colourCount = length(unique(tuition_cost$state))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))

## Make a plot showing the median in state tuition for each state
set.seed(123)
  tuition_cost %>%
    left_join(State_mean)%>%
    mutate(state = as.factor(state))%>%
    drop_na()%>%
    droplevels()%>%
    ggplot(aes(x = reorder(state, in_state_tuition, FUN = median),
               y = in_state_tuition, color = state)) +
    coord_flip() +
    scale_y_continuous(expand = c(0.005, 0.005)) +
  #  scale_color_uchicago() +
    scale_color_manual(values = getPalette(colourCount))+
    #scale_color_discrete(type = "viridis")+
   # scale_color_brewer(brewer.pal(50, "BrBG"))+
    labs(x = NULL, y = "In state tuition") +
    theme(legend.position = "none",
          axis.title = element_text(size = 12),
          axis.text.x = element_text(family = "Roboto Mono", size = 10),
          panel.grid = element_blank())+
    geom_segment(aes(x = reorder(state, in_state_tuition, FUN = median), xend = reorder(state, in_state_tuition, FUN = median),
                     y = US_avg, yend = state_mean),
                 size = 0.8) +
    geom_hline(aes(yintercept = US_avg), color = "gray70", size = 0.6) +
    geom_point(size = 3, alpha = 0.15)+ 
    geom_jitter(size = 2, alpha = 0.25, width = 0.2)+
    stat_summary(fun = "median", geom = "point", size = 5)+
    ggrepel::geom_label_repel(data = top5, aes(x = state, y = in_state_tuition, label = name), color = 'grey2')  +
    ggsave(filename = 'College_Tuition_031020/Tuitionplot.png', width = 8, height = 7)
  
