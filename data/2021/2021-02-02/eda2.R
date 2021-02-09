library(tidyverse)
library(readxl)
library(glue)
library(viridis)
library(hrbrthemes)
library(gganimate)
library(ggtext)

hs_students <- read_excel("104.10.xlsx", sheet = 1)
bach_students <- read_excel("104.10.xlsx", 
                            sheet = 2
                            ,col_names = c("Year",
                                           "Total",
                                           "SE_Total",
                                           "White",
                                           "SE_White",
                                           "Black",
                                           "SE_Black",
                                           "Hispanic",
                                           "SE_Hispanic",
                                           "Total_API",
                                           "SE_Total_API",
                                           "Asian",
                                           "SE_Asian",
                                           "PacIslander",
                                           "SE_PacIslander",
                                           "AmericanIndian",
                                           "SE_AI",
                                           "Multiethnic",
                                           "SE_Multiethnic"))

dictionary <- t(bach_students[1,])
bach_students <- bach_students[-1,]

bach_clean <- bach_students %>%
  select(Year,Total,White,Black,Hispanic,Asian,PacIslander,AmericanIndian,Multiethnic) %>%
  pivot_longer(cols = c(Total,White,Black,Hispanic,Asian,PacIslander,AmericanIndian,Multiethnic),
               names_to="Ethnicity",
               values_to="Percentages") %>% 
  mutate((across(c(Year,Percentages),as.numeric))) 

# no data in first 21 rows (starts in 1940)
bach_clean <- bach_clean[-c(1:20),]

ggplot(bach_clean, aes(x = Year, y = Percentages, color = Ethnicity)) +
  geom_line() +
  geom_point() +
  theme_ipsum() +
  theme(legend.position="bottom") +
  labs(x="Year", y="Percent All Persons Age 25 and Older",
       title = "Population College Attendance",
       subtitle="1930 to 2016",
       caption="SeanPJ.com") 
  

anim <- ggplot(bach_clean, aes(x = Year, y = Percentages, color = Ethnicity)) +
  geom_line() +
  geom_label(aes(label=round(Percentages,2))) +
  theme_ipsum() +
  theme(legend.position="bottom") +
  labs(x="Year", y="Percent All Persons Age 25 and Older",
       title = "Population College Attendance",
       subtitle="1930 to 2016 ",
       caption="SeanPJ.com") +
  transition_reveal(Year)

animate(anim,fps = .3,end_pause = 5)

anim_save(filename = 'populations.gif',
          anim,
          fps = .3,
          end_pause = 5)
