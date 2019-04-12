rm(list=ls())

library(ggplot2)
library(dplyr)
library(stringr)
library(lubridate)
library(tidyr)
library(readr)
library(Kendall)
library(RColorBrewer)

### Load and pepare data ###
if(Sys.info()[6]!="abramfleishman"){
  setwd("/Users/owenryerson/Google Drive/Birds!/R data Processing/CSV/LaCruz")
  
  getwd()
  
  files<-dir()
  files
  
  sb <- read.csv(files[18], stringsAsFactors = FALSE)  ##"LaCruz_ShoreBirds.csv"
}else{
  sb<-read_csv("Data/LaCruz_ShoreBirds.csv")
}


# Explore the data a little -------------------------------------------------


sb <- sb %>%
  filter( season != "09-10") %>%  ## fall 11 is already gone
  mutate(DateTime=mdy(DateTime), # Make DateTime a date
         # add a day of season
         day_of_season = as.numeric(DateTime-ymd(paste(year(DateTime),"10","01",sep = "-"))),
         day_of_season = ifelse(day_of_season <0,day_of_season +365,day_of_season ))

sum(sb$Count, na.rm = TRUE)



# Mean birds per season ---------------------------------------------------

mean_by_season <- sb %>%
  # drop jul, aug, sep, # months 5 and 6 have few birds so doesn't make sense to include
  filter(month(DateTime)%in%c(1:4,10:12)) %>%
  # group by species season and date, this way we can get a total number of each
  # species seen grouping all the sites.
  group_by( Species, season, Date) %>%
  summarise(total_count=sum(Count,na.rm=T)) %>%
  # then take the average per survey each season
  group_by( Species, season) %>%
  summarise(mean_count=mean(total_count,na.rm=T))


ggplot(data = mean_by_season , aes(x=season, y= mean_count)) +
  facet_wrap(Species ~.,scales="free_y") +
  geom_point() +
  labs(title="Laguna La Cruz Waterbird Counts 2010-2018",  x= "Season", y="Mean Count") +
  theme_classic() +
  geom_smooth(method="lm", se=FALSE)

ggplot(data = mean_by_season %>%
         group_by(season) %>%
         summarise(mean_count = sum(mean_count,na.rm=T)),
       aes(x=season, y= mean_count)) +
  geom_point() +
  labs(title="Laguna La Cruz Waterbird Counts 2010-2018",  x= "Season", y="Mean Count") +
  theme_classic() +
  geom_smooth(method="lm", se=FALSE)


# Total birds per survey --------------------------------------------------


total_by_date <- sb %>%
  filter(month(DateTime)%in%c(1:5,10:12)) %>%
  group_by( Species, season, day_of_season) %>%
  summarise(total_count=sum(Count,na.rm=T))


ggplot(data = total_by_date ,
       aes(x=day_of_season, y= total_count,col=season)) +
  facet_wrap(~Species,scales="free_y")+
  geom_point() +
  labs(title="Laguna La Cruz Waterbird Counts 2010-2018",  x= "Season", y="Total Count") +
  theme_classic() +
  geom_smooth(se=FALSE)

ggplot(data = total_by_date %>%
         group_by(season,day_of_season) %>%
         summarise(total_count = mean(total_count,na.rm=T)),
       aes(x=day_of_season, y= total_count,col=season)) +
  # facet_wrap(~Species,scales="free_y")+
  geom_point() +
  labs(title="Laguna La Cruz Waterbird Counts by Date 2010-2018",  x= "Day of Season (Since 01-Oct))", y="Mean Count") +
  theme_classic() +
  geom_smooth(se=F) +
  scale_color_viridis_d()



# Prep data for MannKendall -----------------------------------------------

### create data frame with monthly averages, including NA
full <- data.frame(Date = seq(from = ymd("2010-10-01"), ymd("2018-06-01"),by='1 month'))


### monthly averages
sbmavg <- sb

sbavg <- sbmavg %>%
  # add a month column
  mutate(month =  month(sbmavg$DateTime)) %>%
  # group by date
  group_by( yearmonth) %>%
  # get total and the number of days surveyed
  summarise(count = sum(Count, na.rm = TRUE),
            dayspermonth = n_distinct(DateTime,na.rm=TRUE)) %>%
  # manually calculate mean
  mutate(avg=count/dayspermonth,
         yearmonth = as.Date(paste(yearmonth,"-01",sep=""))) %>%
  select(yearmonth, avg) %>%
  # join with the full range of dates
  right_join(full,by=c("yearmonth"="Date")) %>%
  # get rig of jul-sep and surveys before sep-2012
  filter(!month(yearmonth)%in%c(7,8,9),
         yearmonth>"2012-09-01")

# create time series
ts <- ts(sbavg$avg, start=c(2012, 10),  frequency=9) # changed frequancy to reflect the 3 months i removed
ts

# run  SeasonalMannKendall
sm <- SeasonalMannKendall(ts)
summary(sm)

ggplot(sbavg,aes(x=yearmonth,y=avg))+
  geom_point()+
  geom_smooth(method="lm",se=F)+
  facet_wrap(~month(yearmonth))

lm(avg~year(yearmonth)+month(yearmonth),data=sbavg) %>% summary


