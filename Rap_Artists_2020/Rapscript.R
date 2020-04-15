### Tidy Tuesday Best Rap Artists ####
### 4/14/2020 ####

library(tidyverse)
library(ggrepel)
library(hrbrthemes)
library(PNWColors)
library(patchwork)
library(ggtext)

#read in data
polls <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/polls.csv')
rankings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/rankings.csv')

# get a cool color pallet
pal<-pnw_palette("Starfish",3)

# rankings of rap artists by gender
p1<-rankings %>%
  group_by(gender)%>%
  count()%>%
  ungroup()%>%
  mutate(percent = 100*n/sum(n),
         fraction = n/sum(n),
         ymax=cumsum(fraction),   #top
         ymin = c(0,head(ymax, n=-1)), #bottom
         labelPosition = (ymax+ymin)/2)%>% #label position)%>%
 ggplot(aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=gender)) +
  geom_rect() +
  geom_text(x=2,aes( y=labelPosition, label=paste0(round(percent),"%"), color=gender), size=6) + # x here controls label position (inner / outer)
  scale_fill_manual(values=pal) +
  scale_color_manual(values=pal) +
  coord_polar(theta="y") +
  xlim(c(-1, 4)) +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 14))


# pull out top artist for each
topartists<-rankings %>%
  group_by(gender) %>%
  top_n(n = 1, wt = points)
  
### average points per gender
p2<-rankings %>%
  group_by(gender) %>%
  ggplot(aes(x = gender, y = points, fill = gender))+
  geom_boxplot()+
  coord_trans(y = "log")+
  geom_curve(aes(x = 1.9, y = topartists$points[1]+20 , xend = 2, yend = topartists$points[1], 
                 colour = "curve"), arrow = arrow(length = unit(0.03, "npc"))) +
  geom_curve(aes(x = 0.9, y = topartists$points[2]+20 , xend = 1, yend = topartists$points[2], 
                 colour = "curve"), arrow = arrow(length = unit(0.03, "npc"))) +
  geom_curve(aes(x = 2.9, y = topartists$points[3]+20 , xend = 3, yend = topartists$points[3], 
                 colour = "curve"), arrow = arrow(length = unit(0.03, "npc"))) +
  annotate(geom="text", x=c(1.9,0.9,2.9), y=topartists$points+25, label=topartists$artist,
           color="red")+
  scale_fill_manual(values=pal) +
  theme_bw()+
  xlab("Gender of artist")+
  ylab("Total Points Awarded")+
  theme(legend.position = "none")

# Bring the plots together

plots<-p1+p2 +
  plot_annotation(title = "Points distribution and representation by <span style = 'color:#b97999;'> female</span>,<span style = 'color:#89689d;'> male</span>, and <span style = 'color:#e69b99;'>mixed </span>rap artists.",
                  subtitle = "Female artists are <b> SUPER </b> underrepresented in the rankings, but are the best artists.",
                  caption = "Plot by N. Silbiger \n@nsilbiger \nData by the BBC Music")&
  theme(plot.title = element_markdown(lineheight = 1.1),
        plot.subtitle = element_markdown())

  ggsave(plot = plots,filename =  "Rap_Artists_2020/rapplot.png", width = 8, height = 5)
