### Hotel data for Tidy Tuesday
### 02/11/20

#### Load Libraries #####
library(tidyverse)
library(brms)
library(tidybayes)
library(ggthemes)
library(modelr)
library(ggpubr)

## read in data ####
hotels <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-11/hotels.csv')


## Let's run a logisitic regression to see if the probability of canceling a stay increases with the number of times the person canceled in the past
m1 <- brm(
  is_canceled ~ previous_cancellations,
  data = hotels,
  family = "bernoulli",
  chains = 2,
  seed = 123 # Adding a seed makes results reproducible.
) 

# plot marginal effects
list_of_data<-conditional_effects(m1)
test<-as.data.frame(list_of_data$previous_cancellations)

p1<-test %>%
  ggplot()+
  geom_line(aes(x = previous_cancellations, y = estimate__))+
  geom_ribbon(aes(x = previous_cancellations, ymin = lower__, ymax = upper__))+
  geom_point(data = hotels, aes(previous_cancellations, y = is_canceled), color = 'orange', size = 3) +
  theme_fivethirtyeight()+
  theme(axis.title = element_text())+
  font("subtitle", size = 14, color = "orange", face = "italic")+
  font("xlab", size = 18)+
  font("ylab", size = 18)+
  labs(x = "Number of prior cancellations at a hotel", y = "Probability of cancelling again",
       subtitle = "People be flaky", title = "The probability of cancelling a reservation increases 
       significantly with the # of prior cancellations")

# save the plot
ggsave(p1, filename = "Hotel_02112020/hotelplot.png")

