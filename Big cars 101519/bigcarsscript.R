## Look at the big cars data 


## Load the libraries
library(tidyverse)
library(ggridges)
library(viridis)
library(hrbrthemes)

## load the data
big_epa_cars <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-15/big_epa_cars.csv")

# make a ridge diagram of CO2 emissions by vehicle size class
#big_epa_cars$VClass<-as.factor(big_epa_cars$VClass)


### remove the 1 to -1 

## seems like for missing data for fuel type 1 they put a 0, 1, or -1.  So lets replace thos with NA

big_epa_cars$co2<-ifelse(big_epa_cars$co2<1, NA, big_epa_cars$co2)

# Separate by manual and automatic transmission
big_epa_cars<-big_epa_cars %>% 
  separate(trany, c("Auto_manual", "Type"), sep = " ")%>%
  drop_na(co2, Auto_manual) 


# Plot
  ggplot(big_epa_cars, aes(x = co2, y = reorder(VClass, -pv2), fill = ..x..), group = Auto_manual) + #ordered axis by decreasing car size
    geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
    scale_fill_viridis(name = "Vehicle size class", option = "C") +
    ylab(expression("Car type by decreasing car size"))+
    xlab(expression(paste(CO[2]," emissions in grams/mile")))+
    labs(title = expression(paste(CO[2],' emissions by vehicle size class and transmission type'))) +
    theme_ipsum() +
    theme(
      legend.position="none",
      panel.spacing = unit(0.1, "lines"),
     # strip.text.x = element_text(size = 8),
     axis.title.x.bottom  = element_text( size = 16, face = "bold" , hjust = 0.5),
     axis.title.y.left = element_text( size = 16, face = "bold" , hjust = 0.5),
      strip.text = element_text(size=18)
    )+
    facet_wrap(~Auto_manual, scales ='free_y')+
    ggsave(filename = 'Big cars 101519/CO2emissionsbycar.png')
