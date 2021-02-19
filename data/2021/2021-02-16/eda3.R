# eda 3
# make an area chart instead

library(tidyverse)
library(ggplot2)
library(viridis)
library(grid)
library(gridExtra)
library(scales)

census <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-16/census.csv')


c <- census %>%
  filter(region == "USA Total") %>%
  select(region,year,white,black,black_free,black_slaves)%>%
  pivot_longer(cols = c(white,black,black_free,black_slaves), 
               names_to = "Group", 
               values_to = "Total") %>%
  filter(Group != 'black')

p1 <- ggplot(c, aes(x=year,y=Total,fill=Group))+
  geom_area(alpha=0.6 , size=.5, colour="black") +
  scale_fill_viridis(discrete = T) +
  theme_ipsum() +
  theme(legend.position="bottom") +
  labs(x="Year", y="Total",
       subtitle = "Total Population by Group",
       caption="") +
  scale_y_continuous(labels = comma) 

cp <- c %>%
  group_by(year, Group) %>%
  summarise(n = sum(Total)) %>%
  mutate(Percentage = n / sum(n)) %>%
  select(!n)

p2 <- ggplot(cp, aes(x=year,y=Percentage,fill=Group)) +
  geom_area(alpha=0.6 , size=.5, colour="black") +
  scale_fill_viridis(discrete = T) +
  theme_ipsum() +
  theme(legend.position="bottom") +
  labs(x="Year", y="Total",
       subtitle = "Percentage Population by Group",
       caption="SeanPJ.com") +
  scale_y_percent()


grid.arrange(p1,p2, nrow=1,
             top = textGrob("Tidy Tuesday Week 8: W.E.B Du Bois Challenge",
                            gp=gpar(fontsize=20,font=3)))
