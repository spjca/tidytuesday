# libraries
library(ggplot2)
library(viridis)
library(hrbrthemes)
library(scales)
library(dplyr)

# load data
employed <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-23/employed.csv')
earn <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-23/earn.csv')

earn$yrQtr <- apply( earn[ , c("year","quarter") ] , 1 , paste , collapse = "-" )


x <- earn %>%
  filter(
    #race == 'All Races' & 
      age == '16 years and over' & 
      ethnic_origin == 'All Origins'
           ) %>%
  select(sex,
         race,
         #ethnic_origin,
         yrQtr,median_weekly_earn) #%>%
  #filter(sex !="Both Sexes") %>%
  #group_by(sex, race,ethnic_origin, yrQtr) %>%
  #summarise(n = sum(median_weekly_earn)) %>%
  #mutate(Percentage = n / sum(n)) %>%
  #select(!n) %>%
ggplot(x) +
  geom_line(aes(x=yrQtr,y=median_weekly_earn, group = sex, color = race)) +
  facet_wrap(~sex)+
  scale_fill_viridis(discrete = T) +
  theme_ipsum() +
  theme(legend.position="bottom") +
  labs(x="Year", y="Total",
       subtitle = "Median Weekly Earnings ",
       caption="SeanPJ.com") +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      vjust = 0.5
      )
  )



