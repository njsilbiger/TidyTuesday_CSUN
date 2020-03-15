## Food consumption and CO2 production
## By Nyssa Silbiger

### Load libraries #####
library(tidyverse)
library(ggsci) # cool color pallets
library(ggrepel)
library(patchwork)
library(ggtext)

## Load Data ####
food_consumption <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-18/food_consumption.csv')

## add another category to the food
# what is already there?
unique(food_consumption$food_category)

food_consumption<-food_consumption %>%
  mutate(cat2 =  ## make smaller categories
           case_when(food_category %in% c("Pork", "Poultry","Beef","Lamb & Goat", "Fish") ~ 'Meat',
                     food_category == "Milk - inc. cheese" ~"Dairy",
                     food_category %in% c("Wheat and Wheat Products", "Rice","Nuts inc. Peanut Butter")~"Nuts and Grains",
                     food_category == "Soybeans" ~ "Soy",
                     food_category == "Eggs" ~ "Eggs"))

## select a few countries to label
labels_data<-food_consumption %>%
  group_by(country ) %>%
  summarise(y = sum(consumption), y2 = sum(co2_emmission)) %>%
  arrange(desc(y)) %>%
  filter(y == max(y)| country == 'USA'|y == min(y)| y2 == max(y2)|
           country == "Iran" | country =="Japan" | country == 'Spain')

# Make a circular stacked barplot of food consumption
  p1<-ggplot(food_consumption)+
  # Add the stacked bar
  geom_bar(aes(x=reorder(country, consumption), y=consumption, fill=cat2), stat="identity", alpha=0.5)+
    # Add text showing the value of each 100/75/50/25 lines
 geom_hline(yintercept = c(200, 400, 600), lty = 2, color = "grey")+
    annotate("text",x = c(10,10,10), y = c(225, 425, 625), 
             label = c("200","400",expression(paste("600 kg pp"^-1, "yr"^-1))), angle = 340)+
  scale_fill_uchicago()+
  geom_label_repel(data = labels_data, 
                   mapping = aes(x = country, y = y, label = country ), 
                   segment.size = .5, nudge_y = 70, nudge_x = 2)+
  labs(caption = "Food consumption")+
  theme_minimal() +
  theme(
  legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
  plot.caption = element_text(hjust = 0.5, size = 16, vjust = 10),
  legend.title = element_blank()
  ) +
  coord_polar()
  
## same but CO2_emission
  p2<-ggplot(food_consumption)+
    # Add the stacked bar
    geom_bar(aes(x=reorder(country, co2_emmission), y=co2_emmission, fill=cat2), stat="identity", alpha=0.5)+
    # Add text showing the value of each 100/75/50/25 lines
    geom_hline(yintercept = c(600, 1000, 2000), lty = 2, color = "grey")+
    geom_label_repel(data = labels_data, 
                     mapping = aes(x = country, y = y2, label = country ), 
                     segment.size = .5, nudge_y = 300, nudge_x = -5)+
    annotate("text",x = c(10,10, 10), y = c(700, 1100, 2100), 
             label = c("600","1000",expression(paste("2000 kg pp"^-1, "yr"^-1))), angle = 340)+
    scale_fill_uchicago()+
        theme_minimal() +
    labs(caption=expression(paste("CO"[2]," consumption"))) + 
    theme(
      legend.position = "none",
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      plot.caption = element_text(hjust = 0.5, size = 16, vjust = 10),
      legend.title = element_blank()
    ) +
    coord_polar()
  
# bring the plots together
p1+p2 +
  plot_annotation(title = "Consumption rates by <span style = 'color:#800000FF;'>dairy</span>, <span style = 'color:#767676FF;'>eggs</span>, <span style = 'color:#FFA319FF;'>meat</span>, <span style = 'color:#8A9045FF;'>nuts & grains</span>, and <span style = 'color:#155F83FF;'>soy</span> ",
                    caption = "Plot by N. Silbiger \nTwitter: @NSilbiger",
                    theme = theme(
                      plot.title = element_markdown(lineheight = 1.1, size = 20, hjust = 0.5)
                    ))+
  ggsave(filename = 'FoodConsumption_031420/foodplot.png', width = 10, height = 6)
