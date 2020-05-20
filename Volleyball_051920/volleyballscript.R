### Nyssa Silbiger #####
#### Tidy Tuesday Script on Beack Volleyball ####
#### 5/19/2020 #######

#### load library ####
library(tidyverse)
library(ggeffects)
library(hrbrthemes)
library(patchwork)
library(ggtext)

### Load data ####
vb_matches <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-19/vb_matches.csv', guess_max = 76000)

##who makes more mistakes men or women and by winning?
data<-vb_matches %>%
  select(year, gender, country, w_p1_tot_errors, w_p2_tot_errors, l_p1_tot_errors,l_p2_tot_errors) %>% # pull out columns I want to work with
  pivot_longer(cols =w_p1_tot_errors:l_p2_tot_errors, names_to = "grouping", values_to = "num.errors" ) %>%
  separate(col = grouping, into = c("win_lose","player","total","e"),sep = "_") %>% # pull out the infp from the grouping column
  select(-c("total","e")) %>% # remove what I don't need
  replace_na(replace = list(num.errors = 0)) %>% # replace the NAs with 0
  mutate(error_yn = ifelse(num.errors>0,1,0)) %>% # make a column of 1's and 0's for errors
  mutate(gender = ifelse(gender =="M", "Man", "Woman"),
         win_lose = ifelse(win_lose =="w", "Win", "Lose"),
         win_lose= factor(win_lose, levels=c('Win',"Lose"))) # reorder so wins comes first
### run a logisitic regression to see if there are any differences in probability of errors by gender
fit<-glm(error_yn ~ gender*win_lose, data = data, family = "binomial")

p1<-ggpredict(fit,terms = c("win_lose","gender")) %>% # this automatically takes the effect sizes and error bars from the model
  plot() +
  labs(y = "Probability of making an error",
       title = "",
       x = "")+
  scale_color_ipsum() +
  theme_ipsum_rc() +
  theme(legend.position = "none")

rm(vb_matches) # this  is really large so remove for space

## now select only the values that are positive and see if an error is made are men or women more likely to make more?
p2<-data %>%
  filter(error_yn>0) %>% # only keep values that have at least 1 error
  ggplot(aes(x = num.errors, fill = gender))+
  geom_density(adjust = 2, alpha = 0.8,position = "stack")+ # adjust smoothed out the density
  scale_fill_ipsum() +
  facet_wrap(~win_lose)+
  labs(x = "Number of errors",
       y = "Density")+
  theme_ipsum_rc(grid="XY") +
  theme(legend.title = element_blank())
  

# get the colors
ipsum_pal()(2)

# Use patchwork to bring it together
fullplot<-p1+p2 + 
  plot_annotation(title = "<span style = 'color:#d18975'>Women</span> have a higher probability of making a mistake, but when mistakes are made <span style = 'color:#8fd175'>men</span> make more than <span style = 'color:#d18975'>women</span>",
                  caption = "Plot by Nyssa Silbiger \n@NSilbiger",
                  subtitle = "Results from beach volleyball tournaments")&
  theme(plot.title = element_markdown(lineheight = 1.1)) 
#save the plot
  ggsave(plot = fullplot,filename = "Volleyball_051920/volleyballplot.png", height = 5, width = 12)
        
      

