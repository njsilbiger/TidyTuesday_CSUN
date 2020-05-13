#### Volcano Tidy Tuesday Script ####
#### By Nyssa Silbiger #############
#### 5/12/2020 ####################

### Load libraries #######

library(tidyverse)
library(lubridate)
library(sf)        # for manipulation of simple features objects
library(rworldmap) # for getMap()
library(cowplot)



volcano <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/volcano.csv')
eruptions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/eruptions.csv')
events <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/events.csv')

# Get the number of eventss by volcano year and the year of the most recent eruption
counts<-events %>%
  group_by(volcano_name, volcano_number)%>%
  count() %>%
  select(num.eruptions = n) %>%
  left_join(volcano) %>%
  drop_na(latitude) %>%# drop volcanoes that we have no location for
  mutate(lasteruption = as.numeric(last_eruption_year)) # convert the years to numbers

## How to make a goode projection: https://wilkelab.org/practicalgg/articles/goode.html

world_sf <- st_as_sf(getMap(resolution = "low"))
# map projection
crs_goode <- "+proj=igh"

# projection outline in long-lat coordinates
lats <- c(
  90:-90, # right side down
  -90:0, 0:-90, # third cut bottom
  -90:0, 0:-90, # second cut bottom
  -90:0, 0:-90, # first cut bottom
  -90:90, # left side up
  90:0, 0:90, # cut top
  90 # close
)
longs <- c(
  rep(180, 181), # right side down
  rep(c(80.01, 79.99), each = 91), # third cut bottom
  rep(c(-19.99, -20.01), each = 91), # second cut bottom
  rep(c(-99.99, -100.01), each = 91), # first cut bottom
  rep(-180, 181), # left side up
  rep(c(-40.01, -39.99), each = 91), # cut top
  180 # close
)

goode_outline <- 
  list(cbind(longs, lats)) %>%
  st_polygon() %>%
  st_sfc(
    crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  )

goode_outline <- st_transform(goode_outline, crs = crs_goode)

# get the bounding box in transformed coordinates and expand by 10%
xlim <- st_bbox(goode_outline)[c("xmin", "xmax")]*1.1
ylim <- st_bbox(goode_outline)[c("ymin", "ymax")]*1.1

# turn into enclosing rectangle
goode_encl_rect <- 
  list(
    cbind(
      c(xlim[1], xlim[2], xlim[2], xlim[1], xlim[1]), 
      c(ylim[1], ylim[1], ylim[2], ylim[2], ylim[1])
    )
  ) %>%
  st_polygon() %>%
  st_sfc(crs = crs_goode)

# calculate the area outside the earth outline as the difference
# between the enclosing rectangle and the earth outline
goode_without <- st_difference(goode_encl_rect, goode_outline)

# give it the right projection
sites <- st_as_sf(counts %>%  ungroup, coords = c("longitude", "latitude"), 
                  crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

## pull out the volcanos that are currently erupting
current <- counts %>%
  filter(lasteruption == 2020) %>%
  select(volcano_name, longitude, latitude) %>%
  ungroup()

currentproj <- st_as_sf(current, coords = c("longitude", "latitude"), 
                  crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")


## Make a plot
ggplot(world_sf) + 
  geom_sf(fill = "#E69F00B0", color = "black", size = 0.5/.pt) +
  geom_sf(data = goode_without, fill = "white", color = "NA") +
  geom_sf(data = goode_outline, fill = NA, color = "gray30", size = 0.5/.pt) +
  geom_sf(data = sites, aes(size = num.eruptions, color = num.eruptions)) +
  ggrepel::geom_label_repel( # label the volcanos currently erupting
    data = currentproj,
    aes(label = volcano_name, geometry = geometry),
    stat = "sf_coordinates",
    min.segment.length = 0.5,
    colour = "magenta",
    segment.colour = "magenta", label.size = 0.2 
  )+
  coord_sf(crs = crs_goode, xlim = 0.95*xlim, ylim = 0.95*ylim, expand = FALSE) +
  theme_minimal_grid() +
  scale_size(trans="log10", name = "# of eruptions")+
  scale_colour_gradient(
    trans = "log10",
    low = "white",
    high = "red",
    space = "Lab",
    na.value = "grey50",
    guide = FALSE,
    aesthetics = "colour"
  )+
 # scale_color_continuous(trans="log10", name = "year of last eruption", breaks = c(-1000, 1, 2000))+
  xlab("")+
  ylab("")+
  ggtitle("Volcano eruptions around the globe")+
  labs(subtitle = "Labels indicate eruptions in 2020",
       caption = "Plot by Nyssa Silbiger \n@NSilbiger")+
 # geom_sf(data = sites) +
  theme(
    panel.background = element_rect(fill = "#56B4E950", color = "white", size = 1),
    panel.grid.major = element_line(color = "gray30", size = 0.25)
  ) +
  ggsave("Volcanoes_051220/volcano.png", width = 10, height = 10)

