# eda 2 for tidytuesday 2021-02-16

# https://towardsdatascience.com/eye-catching-animated-maps-in-r-a-simple-introduction-3559d8c33be1
# https://d4tagirl.com/2017/05/how-to-plot-animated-maps-with-gganimate
# https://medium.com/@mueller.johannes.j/use-r-and-gganimate-to-make-an-animated-map-of-european-students-and-their-year-abroad-517ad75dca06
# https://stackoverflow.com/questions/17884375/custom-choropleth-map-with-states-grouped-into-territories
# https://www.blog.cultureofinsight.com/2017/09/animated-choropleth-maps-in-r/

# load libraries

library(tidyverse)
library(sf)
library(ggmap)
library(gganimate) # devtools::install_github("dgrtwo/gganimate")
library(hrbrthemes)

# load data 


census <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-16/census.csv')

c <- census %>%
  select(region,year,total,white,black,black_free,black_slaves)%>%
  pivot_longer(cols = c(total,white,black,black_free,black_slaves), 
               names_to = "Group", 
               values_to = "Total")

us_state_map = map_data("state");


sf_fifty <- sf::st_as_sf(us_state_map, coords = c("long", "lat")) %>% 
  group_by(group, region) %>% 
  summarize(do_union = FALSE) %>%
  st_cast("POLYGON") %>% 
  ungroup()

northeast <- sf_fifty %>%
  filter(
    region %in% c(
      "connecticut", "maine", "massachusetts", "new hampshire", 
      "rhode island", "vermont", "new jersey","new york", "delaware",
      "district of columbia","maryland","pennsylvania"
    )
  ) %>%
  summarise(id = "Northeast") %>%
  merge(.,c,by.x=('id'),by.y=c('region')) %>%
  filter(Group == 'black_slaves')

south <- sf_fifty %>%
  filter(
    region %in% c(
      "virginia","west virginia", "alabama","florida","georgia",
      "kentucky","mississippi","north carolina","south carolina",
      "tennessee", "arkansas","louisiana","texas"
    )
  ) %>%
  summarise(id = "South")

midwest <- sf_fifty %>%
  filter(
    region %in% c(
      "illinois","indiana","michigan","minnesota","ohio","wisconsin",
      "iowa","kansas","missouri","nebraska","south dakota","north dakota"
    )
  ) %>%
  summarise(id = "Midwest")

west <- sf_fifty %>%
  filter(
    region %in% c(
      "california","nevada", "colorado","montana",
      "utah","idaho","oregon","washington","wyoming"
    )
  ) %>%
  summarise(id = "West")

ggplot() +
  theme_minimal() +
  geom_sf(data = south, col = "green", alpha = 0, size = 2) +
  geom_sf(data = northeast, col = "hotpink", alpha = 0, size = 2) +
  geom_sf(data = midwest, col = "yellow", alpha = 0, size = 2) +
  geom_sf(data = west, col = "orange", alpha = 0, size = 2) 
