### African-american accomplishments in science

# load library 
library(tidyverse)
library(ggchicklet)
library(hrbrthemes)
library(stringr)
library(patchwork)

# load data

firsts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-09/firsts.csv')

## let's make a timeline of accomplishments


# highlight a person from each category
people<-firsts %>%
  group_by(category)%>%
  do(sample_n(.,1))


# plot a timeline
p1<-firsts %>%
  ggplot(aes(x = year, y= category, color = category)) +
  geom_point(alpha = 0.5)+
  geom_point(data = people, aes(x = year, y = category, color = category), 
             size = 3, color = "blue")+
  theme_ipsum_rc(grid="X") +
  ggtitle("Timeline of events")+
  scale_color_manual(values = c("#000000", "#FBEE1F","#C20008", "#252421",
                                "#424042", "#535052","#FBEE1F","#000000"))+
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5))

# highlight people
p2<-people %>% # try to clean up the names
  mutate(names  = word(person, 1,2, sep=" "), # pull the first two words so it only gets name
         names  = str_replace_all(names, "[^[:alnum:]]"," "), 
         names  = str_replace_all(names, "[:digit:]", " "),
         names = trimws(names)) %>%
  ggplot( aes(x = -2, y = 0, label = stringr::str_wrap(accomplishment, 12)))+
    geom_text()+ # add the accomplishment info
    ggtitle("Featured leaders")+
    facet_grid(~category*names)+
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=5)
  )


final_plot<-p1/p2 + plot_annotation(caption = "Plot by N. Silbiger \n@NSilbiger")

ggsave(final_plot,
       filename = "AfricanAmericanAccomplishments/AccomPlot.png", width = 14, height = 7)

