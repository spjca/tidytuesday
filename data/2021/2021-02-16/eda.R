# eda for tidytuesday 2021-02-16

# https://towardsdatascience.com/eye-catching-animated-maps-in-r-a-simple-introduction-3559d8c33be1
# https://d4tagirl.com/2017/05/how-to-plot-animated-maps-with-gganimate
# https://medium.com/@mueller.johannes.j/use-r-and-gganimate-to-make-an-animated-map-of-european-students-and-their-year-abroad-517ad75dca06
# https://stackoverflow.com/questions/17884375/custom-choropleth-map-with-states-grouped-into-territories
# https://www.blog.cultureofinsight.com/2017/09/animated-choropleth-maps-in-r/

# load libraries

library(tidyverse)
library(maps)
library(gganimate)

# load data 

georgia_pop <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-16/georgia_pop.csv')
census <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-16/census.csv')
furniture <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-16/furniture.csv')
city_rural <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-16/city_rural.csv')
income <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-16/income.csv')
freed_slaves <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-16/freed_slaves.csv')
occupation <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-16/occupation.csv')
conjugal <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-16/conjugal.csv')



# census

c <- census %>%
  select(region,year,total,white,black,black_free,black_slaves)%>%
  pivot_longer(cols = c(total,white,black,black_free,black_slaves), 
               names_to = "Group", 
               values_to = "Total")


#load us state map data
us_state_map = map_data("state");

#map each state to a division
us_state_map$division[us_state_map$region %in% c("connecticut", "maine", "massachusetts", "new hampshire", "rhode island", "vermont", "new jersey","new york", "delaware","district of columbia","maryland","pennsylvania")] <- "Northeast"
us_state_map$division[us_state_map$region %in% c("virginia","west virginia", "alabama","florida","georgia","kentucky","mississippi","north carolina","south carolina","tennessee", "arkansas","louisiana","texas")] <- "South"
us_state_map$division[us_state_map$region %in% c("illinois","indiana","michigan","minnesota","ohio","wisconsin","iowa","kansas","missouri","nebraska")] <- "Midwest"
us_state_map$division[us_state_map$region %in% c("new mexico","oklahoma","arizona","puerto rico","virgin islands","alaska","hawaii")] <- "Other"
us_state_map$division[us_state_map$region %in% c("california","nevada", "colorado","montana","north dakota","south dakota","utah","wyoming","idaho","oregon","washington")] <- "West"
# us_state_map$division[us_state_map$region %in% c("california","nevada", "colorado","montana","north dakota","south dakota",
#                                                  "utah","wyoming","idaho","oregon","washington","connecticut", "maine",
#                                                  "massachusetts", "new hampshire", "rhode island", "vermont", "new jersey",
#                                                  "new york", "delaware","district of columbia","maryland","pennsylvania",
#                                                  "virginia","west virginia", "alabama","florida","georgia","kentucky",
#                                                  "mississippi","north carolina","south carolina","tennessee", "arkansas",
#                                                  "louisiana","texas","illinois","indiana","michigan","minnesota","ohio",
#                                                  "wisconsin","iowa","kansas","missouri","nebraska")]<-"USA Total"


c2 <- c %>%
  filter(region != "USA Total") %>%
  merge(x = ., y = us_state_map[ , c("division", "long","lat","group","order")], by.x = c("region"), by.y=c("division"), all.x=TRUE, all.y=TRUE)

x <- c2 %>%
  ggplot(.,aes(x=long,y=lat, group=group)) +
  geom_polygon(aes(fill = Total),color="black") +
  scale_fill_gradient(low = "#fcfbfd", high = "#2d004b") +
    transition_states(year, 3, 20)


# # different way
# map("usa")
# map("state",regions=c("connecticut", "maine", "massachusetts", "new hampshire", 
#                       "rhode island", "vermont", "new jersey","new york", "delaware",
#                       "district of columbia","maryland","pennsylvania"),interior=F,boundary=T,add=T)
# map("state", regions=c("virginia","west virginia", "alabama","florida","georgia",
#                        "kentucky","mississippi","north carolina","south carolina",
#                        "tennessee", "arkansas","louisiana","texas"),interior=F,boundary=T,add=T)
# map("state", regions=c("illinois","indiana","michigan","minnesota","ohio","wisconsin","iowa","kansas",
#                        "missouri","nebraska"),interior=F,boundary=T,add=T)
# map("state", regions=c("california","nevada", "colorado","montana","north dakota",
#                        "south dakota","utah","wyoming","idaho","oregon","washington"),interior=F,boundary=T,add=T)

plot(us_state_map$division=='Northeast')
