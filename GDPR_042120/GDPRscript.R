### Tidy Tuesday on GDPR violations ####
### By Nyssa Silbiger #####

# Load library ######
library(tidyverse)
library(viridis)
library(patchwork)
library(hrbrthemes)
library(circlize)
library(networkD3)
library(RColorBrewer)
library(htmlwidgets)
library(htmltools)

## load data ###
gdpr_violations <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-21/gdpr_violations.tsv')
gdpr_text <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-21/gdpr_text.tsv')

## let's make a sankey plot linking countries to articles of violates. 
## Because there are several times when multiple articles had been violated we need to separate the article column

violdata<-gdpr_violations %>%
  separate(col = article_violated,into = c("first", "second", "third",
                                           "fourth", "fifth"), sep = "[|]" )%>%
  pivot_longer(cols = first:fifth, names_to = "Which.Articles", values_to = "Articles")%>% # make the dataset long
  drop_na(Articles)%>% # remove all the NAs
  mutate(Articles = str_extract(string = Articles, pattern = "[0-9]+"))%>%  # extract the article number and remove the extras for a cleaner plot
  drop_na(Articles) %>%# remove all the NAs again
# We need to count the number of unique groupings from country to article # 
  group_by(name, Articles)%>%
  count()%>%
  filter(Articles != 2018) ## pretty sure "article 2018" is a typo

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(name=c(as.character(violdata$name), as.character(violdata$Articles)) %>% unique())

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
violdata$IDsource=match(violdata$name, nodes$name)-1 
violdata$IDtarget=match(violdata$Articles, nodes$name)-1

# prepare  a color scale -- I chose Plasma from Virdis
cols<-viridis_pal(alpha = 1, begin = 0, end = 1, direction = 1,option = "C")
pal<-cols(25)

ColourScal ='d3.scaleOrdinal().range(["#0D0887FF", "#270592FF", "#3B049AFF", "#4C02A1FF", "#5D01A6FF", "#6E00A8FF",
 "#7E03A8FF", "#8E0BA5FF", "#9C179EFF", "#A92395FF", "#B52F8CFF", "#C13B82FF",
 "#CC4678FF", "#D5536FFF", "#DE5F65FF", "#E56B5DFF", "#ED7953FF", "#F3864AFF",
 "#F89441FF", "#FCA338FF", "#FDB32FFF", "#FDC328FF", "#FBD424FF", "#F6E726FF",
 "#F0F921FF"])'


# Make the Network
mynetwork<-sankeyNetwork(Links = violdata, Nodes = nodes,
              Source = "IDsource", Target = "IDtarget",
              Value = "n", NodeID = "name", 
              sinksRight=FALSE, colourScale=ColourScal, fontSize = 15,
              width= 1500, height=900)

# add some titles
mynetwork<- htmlwidgets::prependContent(mynetwork, htmltools::tags$h2("GDPR violations by country and article number"))
#mynetwork<- htmlwidgets::appendContent(mynetwork, htmltools::tags$p("Spain has the most violations and article 5 was violated the most"))


# save it
setwd("GDPR_042120/")
saveNetwork(mynetwork, file = "networkdiagram.html", selfcontained = TRUE)


## info about the results
# 32 is "Failure to implement sufficient measures to ensure information security"
# 5 Failure to comply with data processing principles
# 6 Non-compliance with lawful basis for data processing