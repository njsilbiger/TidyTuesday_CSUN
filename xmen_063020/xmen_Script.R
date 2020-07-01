### X-men tidy tuesday####
#### By Nyssa Silbiger #####

### load library ####
library(tidyverse)
library(ggraph)
library(igraph)
library(patchwork)

# read in data ###### 
tuesdata <- tidytuesdayR::tt_load('2020-06-30')

#network of hugging hugging_with_which_character
characters<-tuesdata$characters

char_sub<-characters %>%
  drop_na(hugging_with_which_character) %>% # remove all the NAs
  select(character, hugging_with_which_character, issue) %>%
  separate(character, into = c("Hero", "Human"), sep = "=") %>% # separate the names
  separate(hugging_with_which_character, into = c("first", "second"), ",") # some people were hugged twice


## some don't have hero names, so need to clean up those columns
notahero<-which(is.na(char_sub$Human))
char_sub$Human[notahero] <-char_sub$Hero[notahero]

## remove the crazy strings
char_sub$Human<-stringr::str_replace_all(string = char_sub$Human, pattern =  "[^[:alnum:]]", replacement = "") 
char_sub$first<-stringr::str_replace_all(string = char_sub$first, pattern =  "[^[:alnum:]]", replacement = "") 
char_sub$second<-stringr::str_replace_all(string = char_sub$second, pattern =  "[^[:alnum:]]", replacement = "") 

## pile on the second huggers

hug2<-char_sub %>%
  filter(!is.na(second)) %>%
  select(Human, second, issue) %>%
  rename(first = second) %>%
  bind_rows(char_sub)%>%
  rename(source = `Human`, target = `first`) %>%
  mutate(connection = paste0(source, target, "_")) %>%
  select(source, target, connection, issue) %>%
  mutate(value = runif(nrow(hug2))) # add a random value

# Transform to a igraph object -  who is hugging whom?
mygraph <- graph_from_data_frame(hug2)

# Make a cord diagram
p2 <-  ggraph(mygraph, layout="linear") + 
  geom_edge_arc(aes(edge_colour=issue), edge_alpha=0.3, edge_width=0.2) +
  geom_node_point( color="#69b3a2", size=5) +
  geom_node_text( aes(label=name), repel = FALSE, color="#69b3a2",  angle=65, hjust=1, nudge_y = -.5, size=2) +
  theme_void() +
  ggtitle("Who's hugging who?")+
 # coord_flip(clip = "off") +
  theme(
    legend.position="none",
    plot.margin = unit(c(1,1,2,1), "cm"))
  

## who gives the most hugs?
gives<-hug2 %>%
  group_by(source) %>%
  count() %>%
  ungroup()%>%
  arrange(desc(n)) %>%
  mutate(source = fct_reorder(source, desc(n)))%>%
  slice(1:10)  # get the top 10
    
# who gets the most?
gets<-hug2 %>%
  group_by(target)%>%
  count()%>%
  arrange(desc(n))  %>%
  ungroup()%>%
  mutate(target = fct_reorder(target, desc(n)))%>%
  slice(1:10)

## make a barplot
p3<-ggplot(gives, aes(x = source, y = n)) +
  geom_bar(stat = "identity", fill = "darkorchid")+
  xlab("")+
  ylab("Number of hugs")+
  ggtitle("Who gives the most hugs?")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 65, vjust = 0.9, hjust=1)) 

p4<-ggplot(gets, aes(x = target, y = n)) +
  geom_bar(stat = "identity", fill = "darkgreen")+
  xlab("")+
 # ylab("Number of hugs")+
  ggtitle("Who gets the most hugs?")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 65, vjust = 0.9, hjust=1)) 

# bring it together
finalplot<-p2/(p3+p4)

ggsave(filename = "xmen_063020/xmenplot.png",width = 8, height = 10)
