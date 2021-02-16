# eda

# libraries
library(tidytuesdayR)
library(tidyverse)
library(viridis)
library(hrbrthemes)
library(gganimate)

# load data using tidytuesdayR

lifetime_earn <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/lifetime_earn.csv')
student_debt <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/student_debt.csv')
retirement <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/retirement.csv')
home_owner <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/home_owner.csv')
race_wealth <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/race_wealth.csv')
income_time <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/income_time.csv')
income_limits <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/income_limits.csv')
income_aggregate <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/income_aggregate.csv')
income_distribution <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/income_distribution.csv')
income_mean <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/income_mean.csv')


### lifetime earnings viz
# grouped bar plot
# https://www.r-graph-gallery.com/48-grouped-barplot-with-ggplot2.html
ggplot(lifetime_earn, aes(fill=gender, y=lifetime_earn, x=race)) + 
  geom_bar(position="dodge", stat="identity") +
  theme_ipsum() +
  theme(legend.position="bottom") +
  scale_y_continuous(breaks = c(0,500000,1000000,1500000,2000000,2500000,3000000,3500000) )+
  labs(x="Ethnicity", y="Total",
       subtitle="By Gender and Ethnicity",
       caption="SeanPJ.com") +
  ggtitle("Lifetime Income") 




### student debt
# grouped bar plot
ggplot(student_debt, aes(fill=race, y=loan_debt, x=year)) + 
  geom_bar(position="dodge", stat="identity") +
  theme_ipsum() +
  theme(legend.position="bottom") +
  #scale_y_continuous(breaks = c(0,500000,1000000,1500000,2000000,2500000,3000000,3500000) )+
  labs(x="Year", y="Total",
       subtitle="By Ethnicity",
       caption="SeanPJ.com") +
  ggtitle("Student Debt 1989-2016") 

# with facet wrap
ggplot(student_debt, aes(fill=race, y=loan_debt, x=year)) + 
  geom_bar(position="dodge", stat="identity") +
  facet_wrap(~race) +
  theme_ipsum() +
  theme(legend.position="bottom") +
  #scale_y_continuous(breaks = c(0,500000,1000000,1500000,2000000,2500000,3000000,3500000) )+
  labs(x="Year", y="Total",
       subtitle="By Ethnicity",
       caption="SeanPJ.com") +
  ggtitle("Student Debt 1989-2016") 


# percentages instead of actual numbers
# with facet wrap
ggplot(student_debt, aes(fill=race, y=loan_debt_pct, x=year)) + 
  geom_bar(position="dodge", stat="identity") +
  facet_wrap(~race) +
  theme_ipsum() +
  theme(legend.position="bottom") +
  #scale_y_continuous(breaks = c(0,500000,1000000,1500000,2000000,2500000,3000000,3500000) )+
  labs(x="Year", y="Total",
       subtitle="By Ethnicity",
       caption="SeanPJ.com") +
  ggtitle("Student Debt 1989-2016") 




### retirement
# with facet wrap
ggplot(retirement, aes(fill=race, y=retirement, x=year)) + 
  geom_bar(position="dodge", stat="identity") +
  facet_wrap(~race) +
  theme_ipsum() +
  theme(legend.position="bottom") +
  #scale_y_continuous(breaks = c(0,500000,1000000,1500000,2000000,2500000,3000000,3500000) )+
  labs(x="Year", y="Total",
       subtitle="By Ethnicity",
       caption="SeanPJ.com") +
  ggtitle("Retirement 1989-2016") 



### home owner percentage
# facet wrapped
ggplot(home_owner, aes(fill=race, y=home_owner_pct, x=year)) + 
  geom_bar(position="dodge", stat="identity") +
  facet_wrap(~race) +
  theme_ipsum() +
  theme(legend.position="bottom") +
  labs(x="Year", y="Total",
       subtitle="By Ethnicity",
       caption="SeanPJ.com") +
  ggtitle("Home Ownership Percentage 1989-2016") 

# no wrap
ggplot(home_owner, aes(fill=race, y=home_owner_pct, x=year)) + 
  geom_bar(position="dodge", stat="identity") +
  theme_ipsum() +
  theme(legend.position="bottom") +
  labs(x="Year", y="Total",
       subtitle="By Ethnicity",
       caption="SeanPJ.com") +
  ggtitle("Home Ownership Percentage 1989-2016") 

# no wrap geom_area
ggplot(home_owner, aes(fill=race, y=home_owner_pct, x=year)) + 
  geom_area(alpha=0.6 , size=.5, colour="black") +
  theme_ipsum() +
  theme(legend.position="bottom") +
  labs(x="Year", y="Total",
       subtitle="By Ethnicity",
       caption="SeanPJ.com") +
  ggtitle("Home Ownership Percentage 1989-2016") 



### race wealth
ggplot(race_wealth, aes(fill=race, y=wealth_family, x=year)) +
  facet_wrap(~type)+
  geom_bar(position="dodge", stat="identity") +
  theme_ipsum() +
  theme(legend.position="bottom") +
  labs(x="Year", y="Total",
       subtitle="By Ethnicity",
       caption="SeanPJ.com") +
  ggtitle("Family Wealth 1989-2016") 


race_wealth %>%
  ggplot(., aes(fill=race, y=wealth_family, x=year)) +
  facet_grid(type~race)+
  geom_bar(position="dodge", stat="identity") +
  theme_ipsum() +
  theme(legend.position="bottom") +
  labs(x="Year", y="Total",
       subtitle="By Ethnicity",
       caption="SeanPJ.com") +
  ggtitle("Family Wealth 1989-2016") 



### income time
ggplot(income_time, aes(fill=percentile, y=income_family, x=year)) + 
  geom_bar(position="dodge", stat="identity") +
  theme_ipsum() +
  theme(legend.position="bottom") +
  labs(x="Year", y="Total",
       #subtitle="By Gender and Ethnicity",
       caption="SeanPJ.com") +
  ggtitle("Income Percentiles 1989-2016") 



### income limits
income_limits %>%
  filter(dollar_type == 'Current Dollars') %>%
  ggplot(., aes(fill=income_quintile, y=income_dollars, x=year)) +
  facet_wrap(~ race) +
  geom_bar(position="dodge", stat="identity") +
  theme_ipsum() +
  theme(legend.position="bottom") +
  labs(x="Year", y="Total",
       #subtitle="By Gender and Ethnicity",
       caption="SeanPJ.com") +
  ggtitle("Income Percentiles 1989-2016") 


income_limits %>%
  filter(dollar_type == 'Current Dollars' ) %>%
  ggplot(., aes(fill=race, y=income_dollars, x=year)) +
  facet_wrap(~ income_quintile) +
  geom_bar(position="dodge", stat="identity") +
  theme_ipsum() +
  theme(legend.position="bottom") +
  labs(x="Year", y="Total",
       #subtitle="By Gender and Ethnicity",
       caption="SeanPJ.com") +
  ggtitle("Income Percentiles 1989-2016") 


income_limits %>%
  filter(dollar_type == 'Current Dollars' && income_quintile != '(all)') %>%
  ggplot(., aes(fill=race, y=income_dollars, x=year)) +
  #facet_wrap(~ income_quintile + race) +
  facet_grid(income_quintile ~ race,
             margins = TRUE) +
  geom_bar(position="dodge", stat="identity") +
  theme_ipsum() +
  theme(legend.position="bottom") +
  labs(x="Year", y="Total",
       #subtitle="By Gender and Ethnicity",
       caption="SeanPJ.com") +
  ggtitle("Income Percentiles 1989-2016") 



### income aggregates
# reorder income quintiles
#income_aggregate$income_quintile_f <- factor(income_aggregate$income_quintile,
#                                             levels = c("Top 5%", "Highest", "Second", "Third", "Fourth","Lowest"))
# facet grid
income_aggregate %>%
  filter(race != '(all)') %>%
  ggplot(., aes(fill=race, y=income_share, x=year)) +
  #facet_wrap(~ income_quintile + race) +
  facet_grid(income_quintile_f ~ race,
             margins = TRUE) +
  geom_bar(position="dodge", stat="identity") +
  theme_ipsum() +
  theme(legend.position="bottom") +
  labs(x="Year", y="Total",
       #subtitle="By Gender and Ethnicity",
       caption="SeanPJ.com") +
  ggtitle("Percentiles Share of Income 1989-2016") 




### income mean
# reorder income quintiles
income_mean$income_quintile_f <- factor(income_mean$income_quintile,
                                             levels = c("Top 5%", "Highest", "Second", "Middle", "Fourth","Lowest"))
# facet grid
income_mean %>%
  #filter(race == 'All Races') %>%
  ggplot(., aes(fill=race, y=income_dollars, x=year)) +
  #facet_wrap(~ income_quintile + race) +
  facet_grid(income_quintile_f ~ race) +
  geom_bar(position="dodge", stat="identity") +
  theme_ipsum() +
  theme(legend.position="none") +
  labs(x="Year", y="Total",
       #subtitle="By Gender and Ethnicity",
       caption="SeanPJ.com") +
  ggtitle("Percentiles Mean Income 1989-2016") 




income_mean %>%
  #filter(race == c('White Alone','Black Alone','Asian Alone')) %>%
  ggplot(., aes(x=year,y=income_dollars, colour = race)) +
  geom_line() +
  geom_smooth() +
  #scale_y_log10(breaks = c(0,1000,5000,10000,25000,50000,100000,150000,200000,350000,500000,1000000)) +#,2500000,300000,350000) )+
  #scale_y_continuous(breaks = c(0,1000,5000,10000,25000,50000,100000,150000,200000,350000,500000,1000000)) +
  facet_grid(income_quintile_f ~ race) +
  theme_ipsum() +
  theme(legend.position="bottom") +
  labs(x="Year", y="Total",
       caption="SeanPJ.com") +
  ggtitle("Percentiles Mean Income 1989-2016") 



income_mean %>%
  #filter(race == c('White Alone','Black Alone','Hispanic')) %>%
  ggplot(., aes(x=year,y=income_dollars, colour = race)) +
  #geom_line() +
  geom_smooth() +
  #scale_y_log10(breaks = c(0,1000,5000,10000,25000,50000,100000,150000,200000,350000,500000,1000000)) +#,2500000,300000,350000) )+
  #scale_y_continuous(breaks = c(0,1000,5000,10000,25000,50000,100000,150000,200000,350000,500000,1000000)) +
  facet_grid(income_quintile_f ~ race) +
  theme_ipsum() +
  theme(legend.position="bottom") +
  labs(x="Year", y="Total",
       caption="SeanPJ.com") +
  ggtitle("Percentiles Mean Income 1989-2016") 

# smoothed line alone
income_mean %>%
  filter(year > 2000) %>%
  ggplot(., aes(x=year,y=income_dollars, colour = race)) +
  geom_smooth() +
  facet_grid(income_quintile_f ~ race) +
  theme_ipsum() +
  theme(legend.position="bottom") +
  labs(x="Year", y="Total",
       caption="SeanPJ.com") +
  ggtitle("Percentiles Mean Income 1989-2016") 


# back to lines but shorten the time period and freescale in facet_grid
income_mean %>%
  filter(year > 1996 & 
           race == c('White Alone','Black Alone','Hispanic','Asian Alone', 'All Races') &
           dollar_type == 'Current Dollars') %>%
  ggplot(., aes(x=year,y=income_dollars, colour = race)) +
  geom_line() +
  scale_y_continuous() +
  facet_grid(income_quintile_f ~ race, scales = "free") +
  theme_ipsum() +
  theme(legend.position="none") +
  labs(x="Year", y="Total",
       subtitle = 'Facet Grid Line Plot by Income Percentile and Race',
       caption="SeanPJ.com") +
  ggtitle("Mean Income 1996-2016") 


income_mean %>%
  filter(year > 1996 & 
           race == c('White Alone','Black Alone','Hispanic','Asian Alone', 'All Races') &
           dollar_type == 'Current Dollars') %>%
  ggplot(., aes(x=year,y=income_dollars, colour = race)) +
  geom_line() +
  scale_y_continuous() +
  facet_grid(income_quintile_f ~ race, scales = "free") +
  theme_ipsum() +
  theme(legend.position="none") +
  labs(x="Year", y="Total",
       title = "Mean Income 1996-2016",
       subtitle = 'Facet Grid Line Plot by Income Percentile and Race',
       caption="SeanPJ.com")  


x <- income_mean %>%
  filter(year > 1996 & 
           race == c('White Alone','Black Alone','Hispanic','Asian Alone', 'All Races') &
           dollar_type == 'Current Dollars') %>%
  ggplot(., aes(x=year,y=income_dollars, colour = race)) +
  geom_line() +
  scale_y_continuous() +
  facet_grid(income_quintile_f ~ race, scales = "free") +
  theme_ipsum() +
  theme(legend.position="none") +
  labs(x="Year", y="Total",
       title = "Mean Income 1996-2016",
       subtitle = 'Facet Grid Line Plot by Income Percentile and Race',
       caption="SeanPJ.com")

##### need to finish below

# ggsave(filename = "plot.png", 
#        income_mean %>%
#          filter(year > 1996 & 
#                   race == c('White Alone','Black Alone','Hispanic','Asian Alone', 'All Races') &
#                   dollar_type == 'Current Dollars') %>%
#          ggplot(., aes(x=year,y=income_dollars, colour = race)) +
#          geom_line() +
#          scale_y_continuous() +
#          facet_grid(income_quintile_f ~ race, scales = "free") +
#          theme_ipsum() +
#          theme(legend.position="none") +
#          labs(x="Year", y="Total",
#               subtitle = 'Facet Grid Line Plot by Income Percentile and Race',
#               caption="SeanPJ.com") +
#          ggtitle("Mean Income 1996-2016") )


# anim <- income_mean %>%
#   filter(year > 1996 & 
#            race == c('White Alone','Black Alone','Hispanic','Asian Alone', 'All Races') &
#            dollar_type == 'Current Dollars') %>%
#   ggplot(., aes(x=year,
#                 y=income_dollars,
#                 colour = race,
#                 frame=year)) +
#   geom_line() +
#   scale_y_continuous() +
#   facet_grid(income_quintile_f ~ race, scales = "free") +
#   theme_ipsum() +
#   theme(legend.position="none") +
#   labs(x="Year", y="Total",
#        subtitle = 'Facet Grid Line Plot by Income Percentile and Race',
#        caption="SeanPJ.com") +
#   ggtitle("Mean Income 1996-2016") 
# 
# animate(anim,
#         height = 1380, width =780)
# 
# anim_save(filename = 'mean_income.gif',
#           anim,
#           fps = .3,
#           end_pause = 5)

