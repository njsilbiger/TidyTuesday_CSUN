## Let's make a tree of trees! 
## Code by Nyssa Silbiger
## 1/28/2020
######################################

#load libraries
library(tidyverse)
library(ggdendro)
library(dendextend)

# load the data
sf_trees <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-28/sf_trees.csv')

dendplot<-sf_trees %>%
  separate(col = species,into = c("species_name","common_name"), sep = "::")%>% # separate out the common names
  filter(species_name !="Tree(s) ") %>% # remove things with no species name
  group_by(legal_status, common_name)%>%
  tally()%>%
  filter(common_name !="") %>% # remove things with no species name
  pivot_wider(names_from =common_name, values_from = n) %>% # count how many of each tree there are per legal status.  
  replace(., is.na(.), 0) %>% # replace all NA with a 0
  filter(legal_status !=0)%>%
  column_to_rownames(var = "legal_status")%>%
  scale%>% ## make the dendogram
  dist %>%
  hclust %>%
  as.dendrogram %>%
  set("nodes_cex", 2) %>%  # node point size
  set("branches_k_color", k=5) %>% set("branches_lwd", 1.5) %>%
  set("labels_colors") %>% set("labels_cex", 1.5) %>% 
  set("leaves_pch", 19) 

# make it a plot (ggplot kept cutting off my labels even when changing the margins)
png(filename = 'SFTrees_012820/treeoftrees.png', height = 500, width = 1000)
par(mar=c(3,4,1,18))
plot(dendplot, horiz = TRUE, axes = FALSE)
dev.off()
