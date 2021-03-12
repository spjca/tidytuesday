library(tidyverse)
library(ggridges)
library(ggplot2)
library(dplyr) # easier data wrangling 
library(viridis) # colour blind friendly palette, works in B&W also
library(lubridate) # for easy date manipulation
library(ggExtra) # because remembering ggplot theme options is beyond me
library(tidyr) 


raw_bechdel <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-09/raw_bechdel.csv')
movies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-09/movies.csv')


cleaned <- movies %>%
  inner_join(raw_bechdel[,3:5], by='imdb_id') %>%
  mutate(domgross_2013 = as.double(domgross_2013)) %>%
  mutate(intgross_2013 = as.double(intgross_2013)) %>%
  mutate(totalNet_2013 = domgross_2013 + intgross_2013 - budget_2013) %>%
  mutate(costNetRatio_2013 = totalNet_2013/budget_2013) %>%
  mutate(runtime = as.integer(gsub(" min", "",runtime))) 


cleaned %>%
  group_by(year, rating) %>%
  mutate(avgAnnualRatingNetBudgetRatio_2013 = mean(costNetRatio_2013)) %>%
  ggplot() +
  geom_line(aes(year,avgAnnualRatingNetBudgetRatio_2013)) +
  facet_wrap(~rating)


cleaned %>% 
  group_by(year) %>%
  mutate(avgRating = mean(rating)) %>%
  ungroup() %>%
  ggplot() +
  geom_line(aes(year, avgRating))


movies %>%
  group_by(year) %>%
  mutate(avgRating = mean(metascore, na.rm = TRUE)) %>%
  ggplot() +
  geom_line(aes(year, avgRating)) 

cleaned %>% 
  group_by(year) %>%
  summarise(avgRating = mean(rating)) %>%
  ggplot() +
  geom_line(aes(year, avgRating))


# mean runtime per year by movietype
cleaned %>% 
  group_by(year,binary) %>%
  mutate(avgRuntime = mean(runtime, na.rm = TRUE)) %>%
  ggplot() +
  geom_line(aes(year, avgRuntime)) +
  facet_wrap(~binary)



# turn the string csv genre column into several boolean columns
lst <- strsplit(as.character(cleaned$genre),", ")
lvl <- unique(unlist(lst))      
res <- data.frame(rating = cleaned$rating,
                  rated = cleaned$rated,
                  year = cleaned$year,
                  imdb_id=cleaned$imdb_id,
                  do.call(rbind,lapply(lst, function(x) table(factor(x, levels=lvl)))), 
                  stringsAsFactors=FALSE)

genreCounts <- res %>%
  gather(genreDecp,count,"Biography":"Documentary") %>%
  group_by(year,genreDecp,rating) %>%
  summarise(total=sum(count))

genreCounts %>%
  ggplot(.,aes(y = total, x = year, fill = rating)) +
  geom_bar(stat = 'identity')+ # group = genreDecp)) +
  facet_wrap(~genreDecp)


genreCounts %>%
  mutate(year = as.factor(year)) %>%
  #mutate(year = as.factor()) %>%
  ggplot(.,aes( y = genreDecp, x = total))+#, fill = rating))+
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")



# heatmap

p <- genreCounts %>%
  filter(rating!=0 
         & genreDecp!='Documentary' 
         & genreDecp!='Sport' 
         & genreDecp!='War' 
         & genreDecp!='Western' 
         & genreDecp!='Music' 
         & genreDecp!='Musical'
         & genreDecp!='Biography'
         & genreDecp!='History') %>%
  ggplot(.,aes(rating,year,fill=total))+
  geom_tile(color= "white",size=0.1) + 
  scale_fill_viridis(name="Number of Movies",option ="C")
p <-p + facet_grid(~genreDecp)
p <-p + scale_y_continuous(breaks = unique(genreCounts$year))
#p <-p + scale_x_continuous(breaks =c(1,10,20,31))
p <-p + theme_minimal(base_size = 8)
p <-p + labs(title= paste("Bechdel Test Movie Analysis 1970-2013"), 
             subtitle = "Total number of Movies by Year, Genre, and Test Rating",
             caption = "Tidy Tuesday Week 11 2021\n SeanPJ.Com",
             x="Bechdel Rating", 
             y="")
p <-p + theme(legend.position = "bottom")+
  theme(plot.title=element_text(size = 14))+
  theme(axis.text.y=element_text(size=6)) +
  theme(strip.background = element_rect(colour="white"))+
  theme(plot.title=element_text(hjust=0))+
  theme(axis.ticks=element_blank())+
  theme(axis.text=element_text(size=7))+
  theme(legend.title=element_text(size=8))+
  theme(legend.text=element_text(size=6))+
  removeGrid()#ggExtra


ggsave(p, file="output.png", width=20, height=10)
