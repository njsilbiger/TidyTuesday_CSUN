# Tidy Tuesday on CRAN data
# 11/12/19


library(collapsibleTree) 
library(htmlwidgets)
library(tidyverse)

# load the data
cran_code <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-12/loc_cran_packages.csv")

#dendogram to look at languages by package name?

# lets find the top 5 most popular languages

top5<-cran_code %>%
  group_by(language)%>%
  tally()%>%
  arrange(desc(n))%>%
  top_n(5)

# filter out the data from just the top 5 languages
top5_data<-cran_code %>%
  filter(language %in% top5$language) %>%
  group_by(language)%>%
  top_n(n = 5, wt = code) # now pull out the packages with that have the most code per each of the 5 languages


## add a column that says how many lines of code
top5_data$code_lines<-paste(top5_data$code, "lines of code")

# Below is all thanks to Kathryn Scafidi!
#Now let's get this graph cookin!
#Root is the name you want the first main node to be
#Hierarchy is the order from your data sheet of what you want each node to be nested in
#Node size is what you can use to visually show the differences by the node size, so even though I will
#have the info pop up when you hover over the last node, the visual differnce of the node sizes is more eyecatching
h<-collapsibleTree(
  top5_data,
  root = "CRAN",
  hierarchy = c("language", "pkg_name", "code_lines"),
  nodeSize = "code",
  fill = c(
    # The root
    "e69b99",
    # Unique languages
    rep("ba7999", length(unique(top5_data$language))), # startfish colors from PNWcolors packages
    # Unique packages per language
    rep("89689d", length(unique(paste(top5_data$language, top5_data$pkg_name)))),
    # Unique lines of code per region
    rep("59629b", length(unique(paste(top5_data$code_lines, top5_data$language))))
  ),
  width = 800,
  fontSize = 14,
  zoomable = FALSE
)

#save the widget
saveWidget(h, file="dendrogram.html")
