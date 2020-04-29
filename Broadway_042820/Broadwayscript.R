### Broadway data for Tidy Tuesday
### By Nyssa Silbiger
#### 4/28/2020

# load library
library(tidyverse)
library(lubridate)
library(ggbump)
library(ggrepel)

grosses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/grosses.csv', guess_max = 40000)

### let's add up the total weekly_gross_overall for each show per year then rank them
highestgrossing<-grosses %>%
  mutate(years = year(week_ending))%>%
  group_by(show, years)%>%
  summarise(total_yearly_gross = sum(weekly_gross_overall)/1000000) %>% # in millions 
  mutate(cum_gross = cumsum(total_yearly_gross)) %>%
  ungroup()%>%
  arrange(years,-cum_gross) %>%
  group_by(years) %>% 
  mutate(rank = 1:n()) %>%
  filter(rank <= 5) 
 
### pull out the first time a show makes it in for the labels
first_time<-highestgrossing %>%
  group_by(show)%>%
  filter(years == max(years))

# Make a ggbump of the highest cumulative grossing shows
# inspiration https://github.com/davidsjoberg/ggbump/wiki/My-year-on-Spotify
myplot<-ggplot(highestgrossing, aes(years, rank, color = show)) +
  geom_bump(size = 1)+
  geom_text_repel(data = first_time, aes(years, rank,label = show), force = 5, nudge_y = 0.25)+
  geom_point(data = first_time, aes(years, rank), size = 5)+
  theme_minimal() +
  scale_y_reverse() +
  scale_x_continuous(breaks = seq(1985,2020,5))+
  theme(legend.position = "none",
        panel.grid = element_blank(),
        plot.title = element_text(hjust = .5, color = "white"),
        plot.caption = element_text(hjust = 1, color = "white", size = 8),
        plot.subtitle = element_text(hjust = .5, color = "white", size = 10),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(face = 2, color = "white"),
        panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black")) +
  labs(x = NULL,
       title ="Highest cumulative grossing Broadway shows ",
       subtitle ="Top 5 grossing shows per year",
       caption = "Plot by Nyssa Silbiger")+
  geom_point(data = tibble(x = 1982, y = 1:5), aes(x = x, y = y), 
             inherit.aes = F,
             color = "white",
             size = 10,
             pch = 21) +
  geom_text(data = tibble(x = 1982, y = 1:5), aes(x = x, y = y, label = y), 
            inherit.aes = F,
            color = "white")
# save it
ggsave(myplot, filename = "Broadway_042820/broadwayplot.png", width = 10, height = 6)
