#### Cocktails Tidy Tuesday ######
### 5/26/2020 #####


# load libraries #####
library(tidyverse)
library(vegan) # for the nmds
library(fishualize) #color pal of fish
library(patchwork)
## read in the data ####
cocktails <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/cocktails.csv')

# Let's do an nmds of ingredients by categories. Ingredients are like species and drinks are like sites
## Then color by glass type
ingreds<-cocktails %>%
  filter(alcoholic =="Alcoholic") %>%
  select(c(category,drink, ingredient, glass, ingredient_number))%>%
  group_by(category,drink, ingredient, glass) %>%
  tally() %>%
 # mutate(present = 1) %>% # add a column of 1's so that it is 1 or NA for pivoting
  pivot_wider(names_from = ingredient, values_from = n) %>%
  replace(is.na(.), 0)  %>%
 # select(!ingredient_number) %>%
  filter(`Bitters` == 1)  # only pull out drinks with bitters
  
# pull out only the ingredients for the nmds code
ingreds.only<-ingreds %>%
  ungroup() %>%
  select(4:309) %>%
  select_if(colSums(.) != 0) # only pull out columns without all zeros

# run an nMDS 
ingreds.mds <- metaMDS(ingreds.only, distance = "bray")

#Each dot represents a drink. Dots closer together means ingredients within that drink are more similar
# 
spp.ingred<- data.frame(ingreds.mds$species) #save "species" intrinsic values into dataframe
spp.ingred <- cbind(spp.ingred, Species = rownames(spp.ingred)) #add species names to dataframe

sites.ingred<- data.frame(ingreds.mds$points) #save "site" intrinsic values into dataframe
sites.ingred<-sites.ingred %>%
  bind_cols(ingreds) %>%
  select(drink, glass, category, MDS1, MDS2)

# make the base nmds plot
nmds.plot <- ggplot(# run an nMDS 
  sites.ingred, aes(x=MDS1, y=MDS2))+ #sets up the plot
  coord_fixed()+
  theme_void()+ 
  xlab("")+
  ylab("")+
  scale_color_fish_d(option = "Naso_lituratus")+ # fish color palette!
  theme(panel.background = element_rect(fill = "seashell2", colour = "black", size = 1, linetype = "solid"),
        legend.position = "right", 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 12), 
        axis.text = element_text(size = 10),
        axis.text.x = element_blank(),
        axis.text.y = element_blank()) # add legend at right of plot

# vectors for "species" (ingredient)
species<-nmds.plot + 
  ggrepel::geom_text_repel(data = sites.ingred, aes(x=MDS1, y=MDS2, label = drink), cex = 3, direction = "both", segment.size = 0.25)+
  geom_point(aes(MDS1, MDS2, color = glass), size = 3)+ #adds site points to plot
  labs(colour = "Glass Type") # add legend labels 
  
# points for "sites" (drink name)  
sites<-nmds.plot +
  geom_segment(data = spp.ingred, aes(x = 0, xend=MDS1, y=0, yend=MDS2), 
               arrow = arrow(length = unit(0.25, "cm")), colour = "grey10", lwd=0.3) + #add vector arrows of ingredients
  ggrepel::geom_text_repel(data = spp.ingred, aes(x=MDS1, y=MDS2, label = Species), cex = 3, direction = "both", segment.size = 0.25)

# put the plots together in patchwork
finalplot<-sites+species+
  plot_annotation(title = "Drinks with bitters as an ingredient") &
  theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom")&
  plot_layout(guides="collect") 

ggsave("Cocktails_052920/nmdsplot.png")
    