library(tidyverse)
library(readxl)
library(glue)
library(viridis)
library(hrbrthemes)

# to save time, I am using the loading script provided

# HBCU data
hbcu_all <- read_excel("tabn313.20.xls", sheet = 1)

hbcu_perc <- hbcu_all %>%
  select(Year,Males,Females) %>%
  pivot_longer(cols = c(Males, Females), 
               names_to = "Gender", 
               values_to = "Total")
  

ggplot(hbcu_perc, aes(x = Year,y= Total,fill = Gender)) +
  geom_area()

hbcu_perc_area <- hbcu_perc %>%
  group_by(Year, Gender) %>%
  summarise(n = sum(Total)) %>%
  mutate(Percentage = n / sum(n))

ggplot(hbcu_perc_area, aes(x = Year, y = Percentage, fill = Gender)) +
  geom_area()

# now fancy it up
ggplot(hbcu_perc_area, aes(x = Year, y = Percentage, fill = Gender)) +
  geom_area(alpha=0.6 , size=.5, colour="black") +
  scale_fill_viridis(discrete = T) +
  theme_ipsum() +
  theme(legend.position="bottom") +
  labs(x="Year", y="Percent",
       subtitle="1976 to 2015",
       caption="SeanPJ.com") +
  ggtitle("HBCU Education by Gender")


# https://www.r-graph-gallery.com/136-stacked-area-chart.html
# https://thatdatatho.com/2020/03/28/tidyrs-pivot_longer-and-pivot_wider-examples-tidytuesday-challenge/
# http://www.sthda.com/english/wiki/ggplot2-legend-easy-steps-to-change-the-position-and-the-appearance-of-a-graph-legend-in-r-software#change-the-legend-position
# 


hbcu_colleges_perc <- hbcu_all %>%
  select(Year,`2-year - Public`,`2-year - Private`,`4-year - Public`,`4-year - Private`) %>%
  pivot_longer(cols = c(`2-year - Public`,`2-year - Private`,`4-year - Public`,`4-year - Private`), 
               names_to = "College Type", 
               values_to = "Total")

ggplot(hbcu_colleges_perc, aes(x = Year,y= Total,fill = `College Type`)) +
  geom_area()

hbcu_colleges_perc_area <- hbcu_colleges_perc %>%
  group_by(Year,`College Type`) %>%
  summarise(n = sum(Total)) %>%
  mutate(Percentage = n / sum(n))


# now fancy it up
p <- ggplot(hbcu_colleges_perc_area, aes(x = Year, y = Percentage, fill = `College Type`)) +
  geom_area(alpha=0.6 , size=.5, colour="black") +
  scale_fill_viridis(discrete = T) +
  theme_ipsum() +
  theme(legend.position="bottom") +
  labs(x="Year", y="Percent",
       subtitle="1976 to 2015",
       caption="SeanPJ.com") +
  ggtitle("HBCU by Type") 

# and make it animated
library(gganimate)

q <- p + transition_reveal(Year)

animate(q, end_pause = 10)
anim_save('area.gif', q)


t <- ggplot(hbcu_colleges_perc, aes(x = Year, y = Total, fill = `College Type`)) +
  geom_area(alpha=0.6 , size=.5, colour="black") +
  scale_fill_viridis(discrete = T) +
  scale_y_continuous(breaks = c(0,50000,100000,150000,200000,250000,300000,350000) )+
  theme_ipsum() +
  theme(legend.position="bottom") +
  labs(x="Year", y="Total",
       subtitle="1976 to 2015",
       caption="SeanPJ.com") +
  ggtitle("HBCU by Type") +
  transition_reveal(Year)

anim_save('output.gif',t)
