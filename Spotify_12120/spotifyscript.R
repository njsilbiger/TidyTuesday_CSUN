### Spotify coding list
## By Nyssa Silbiger
## 1/21/2020


### load libraries #####
library(tidyverse)
#devtools::install_github("dill/beyonce")
library(beyonce)
library(magick)

## Load the Data ####
spotify_songs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv')

# what is beyonce's most danceable song?  ####
top_song<-spotify_songs %>%
  filter(track_artist =="Beyoncé")%>%
  arrange(desc(danceability)) %>%
  filter(danceability==max(danceability))%>%
  select(track_name, danceability, track_album_name)


## What are Beyonce's most danceable albumn? ####

## get a beyonce color palette
pal <- beyonce_palette(69,type = "discrete")

Beyonce<-spotify_songs %>%
  filter(track_artist =="Beyoncé")%>%
  arrange(desc(danceability)) %>%
  ggplot()+
  geom_boxplot(aes(x = reorder(track_album_name, danceability, FUN = median), y = danceability,  fill =track_album_name )) +
  scale_fill_manual(values = beyonce_palette(69, type = "discrete"))+
  xlab(" ")+
  ylab("Danceability")+
  ggtitle('Beyoncé albums ranked by danceability')+
  geom_curve(aes(x = 2, y = top_song$danceability-.1 , xend = top_song$track_album_name, yend = top_song$danceability, 
                 colour = "curve"), arrow = arrow(length = unit(0.03, "npc"))) +
  annotate(geom="text", x=2, y=top_song$danceability-.08, label=top_song$track_name,
           color="red")+
  theme_classic()+
  theme(legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5),
        text = element_text(size=18),
        legend.position = "none",
        axis.text.x = element_text(angle = 90, hjust = 1))+
  ggsave("Spotify_12120/Beyonce_background.png")



## add a beyonce gif
## set the background
background <- image_scale(image_read("Spotify_12120/Beyonce_background.png"), "600x600")
# And bring in a gif (from Formation)
logo_raw <- image_scale(image_read("https://media.giphy.com/media/tqMySWDYuyoDu/giphy.gif"),"200x150" )

# make frames for the animations
frames <- lapply(logo_raw, function(frame) {
  image_composite(background, frame, offset = "+400+180")
})

# animate it
animation <- image_animate(image_join(frames), fps = 10)
#save the animation
image_write(animation, "Spotify_12120/BeyonceFinal.gif")
