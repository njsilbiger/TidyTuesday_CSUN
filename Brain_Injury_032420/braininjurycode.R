
library(tidyverse)
library(packcircles)
library(ggthemes)
library(patchwork)
library(ggsci)

# load the data 
tbi_age <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-24/tbi_age.csv')

# Make a plot with # of injuries
total<-tbi_age %>%
  filter(age_group != "0-17")%>%
  mutate(age_group = factor(age_group, 
                            levels =c("0-4", "5-14","15-24","25-34","35-44","45-54","55-64","65-74","75+", "Total")))%>% 
  ggplot(aes(x = age_group, y = number_est, fill = injury_mechanism))+
  geom_bar(stat = "identity")+
  theme_solarized()+
  scale_fill_jama()+
  xlab(" ")+
  ylab(" ")+
  ggtitle("Total number of injuries")+
  theme(legend.title = element_blank())

  
# plot with # of deaths
deaths<-tbi_age %>%
  filter(age_group != "0-17")%>%
  filter(type == "Deaths")%>%
  mutate(age_group = factor(age_group, 
                            levels =c("0-4", "5-14","15-24","25-34","35-44","45-54","55-64","65-74","75+", "Total")))%>% 
  ggplot(aes(x = age_group, y = number_est, fill = injury_mechanism))+
  geom_bar(stat = "identity")+
  theme_solarized()+
  scale_fill_jama()+
  xlab(" ")+
  ylab(" ")+
  ggtitle("Total number of deaths")+
  theme(legend.title = element_blank())

p1<-total+deaths + plot_annotation(tag_levels = 'A')+
  plot_layout(guides = "collect")&theme(legend.position = "bottom")

  ggsave("Brain_Injury_032420/Injuryplot.png",p1, width = 12, height = 5)
