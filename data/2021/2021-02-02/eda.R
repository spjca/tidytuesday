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
               names_to = "Sex", 
               values_to = "Total")
  

ggplot(hbcu_perc, aes(x = Year,y= Total,fill = Sex)) +
  geom_area()

hbcu_perc <- hbcu_perc %>%
  group_by(Year, Sex) %>%
  summarise(n = sum(Total)) %>%
  mutate(Percentage = n / sum(n))

ggplot(hbcu_perc, aes(x = Year, y = Percentage, fill = Sex)) +
  geom_area()

# now fancy it up
ggplot(hbcu_perc, aes(x = Year, y = Percentage, fill = Sex)) +
  geom_area(alpha=0.6 , size=.5, colour="white") +
  scale_fill_viridis(discrete = T) +
  theme_ipsum() +
  theme(legend.position="bottom") +
  labs(x="Year", y="Percent",
       subtitle="1976 to 2015",
       caption="SeanPJ.com") +
  ggtitle("HBCU Gender Percentages")


# https://www.r-graph-gallery.com/136-stacked-area-chart.html
# https://thatdatatho.com/2020/03/28/tidyrs-pivot_longer-and-pivot_wider-examples-tidytuesday-challenge/
# http://www.sthda.com/english/wiki/ggplot2-legend-easy-steps-to-change-the-position-and-the-appearance-of-a-graph-legend-in-r-software#change-the-legend-position
# 