rm(list=ls())

library(MASS)
library(ggplot2)
library(dplyr)
library(stringr)
library(lubridate)
library(tidyr)
library(readr)
library(Kendall)
library(RColorBrewer)
library(MuMIn)
library(patchwork)

options(na.action="na.fail")

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

# starting with shorebirds in La Cruz
sb <- sb %>%
  filter( !season %in%c("09-10","10-11","11-12")) %>%  ## drop old data since it was covering a bigger area
  mutate(date=mdy(DateTime), # Make date
         # add a numeric season year
         year_season=ifelse(monthnum<9,year(date),year(date)+1),
         # add a day of season
         day_of_season = as.numeric(date-ymd(paste(year(date),"10","01",sep = "-"))),
         day_of_season = ifelse(day_of_season <0,day_of_season +365,day_of_season ),
         #a dd a quadradic term
         day_of_season2 = day_of_season^2,
         # add month of season
         month_of_season = ifelse(monthnum<9, monthnum+3,monthnum-9),
         # add hour
         hour=hour(hm(TimeStart)),
         # fix hour
         hour=as.numeric(ifelse(hour>500,str_extract(hour,"^[0-9]{2}"),hour)),
         hour=ifelse(is.na(hour),floor(mean(hour,na.rm=T)),hour),
         hour=ifelse(hour==2,14,hour),
         # add missing tides and winds
         Tide=ifelse(is.na(Tide),"mid-rising",Tide),
         Wind=ifelse(is.na(Wind),floor(mean(Wind,na.rm=T)),Wind),
         # fix tide
         Tide=tolower(Tide),
         Tide=recode(Tide,
                     "mid-risng"="mid-rising"),
         # split tide into height and direction
         tide_height=str_extract(Tide,"mid|low|high"),
         tide_height=ifelse(is.na(tide_height),"unk",tide_height),
         tide_dir=str_extract(Tide,"slack|rising|falling"),
         tide_dir=ifelse(is.na(tide_dir),"slack",tide_dir),
         # fix point names
         PointLoc=gsub("la|santa|site| ","",tolower(PointLoc)),
         PointLoc=recode(PointLoc,
                         "crabcoop"="crabcamp",
                         "oystercamp"="oysterfarm")) %>%
  filter(PointLoc!="restaurant")

# warning about strings failed to parse in HM is ok

# GLM? --------------------------------------------------------------------
names(sb)
unique(sb$TimeStart)
table(sb$PointLoc)

# summarize by survey (group all the species)
sum_by_season_site <- sb %>%
  # drop jul, aug, sep for few obs # months 5 and 6 have few birds so might not make sense to include
  filter(month(date)%in%c(1:6,10:12)) %>%
  # group by species season and date, this way we can get a total number of each
  # species seen grouping all the sites.
  group_by(  year_season, month_of_season,day_of_season,day_of_season2,tide_dir,tide_height,Wind,PointLoc,hour) %>%
  summarise(total_count=sum(Count,na.rm=T)) %>%
  ungroup  %>% as.data.frame()

# quick checks for missing values and what unique values we have
table(sum_by_season_site$tide_height,useNA = "ifany")
table(sum_by_season_site$tide_dir,useNA = "ifany")
table(sum_by_season_site$hour,useNA = "ifany")

table(sum_by_season_site$PointLoc)
table(sum_by_season_site$PointLoc,sum_by_season_site$year_season)
par(mfrow=c(1,1))
hist(sum_by_season_site$total_count,breaks = 100)

# plot ?  # need more plots to see what we would predict interms of patterns
ggplot(sum_by_season_site,aes(x=factor(year_season),y = total_count))+
  geom_boxplot()+
  facet_wrap(~PointLoc,scales="free")



# GLM (Gaussian) -----

mod_norm<-glm(total_count~year_season+Wind+hour+day_of_season+PointLoc+tide_height+tide_dir,
          data=sum_by_season_site)

# calculate risiduals for plotting
resid_norm<-data.frame(
  pearsons_residuals=resid(mod_norm,type="pearson"),
  deviance_residuals=resid(mod_norm,type="deviance"),
  response_residuals=sum_by_season_site$total_count-predict(mod_norm,type = "response"),
  predicted=predict(mod_norm,type = "response"),
  observed=sum_by_season_site$total_count)

# model results
summary(mod_norm)

# plots for understanding model results
par(mfrow=c(2,2))
plot(mod_norm)
par(mfrow=c(1,1))

# plots for eval of model fit (should be no patters in residuals!)
ggplot(resid_norm,aes(x=predicted,y=pearsons_residuals))+
  geom_point()+
  ggplot(resid_norm,aes(x=predicted,y=deviance_residuals))+
  geom_point()+
  ggplot(resid_norm,aes(x=predicted,y=response_residuals))+
  geom_point()+
  ggplot(resid_norm,aes(x=predicted,y=observed))+
  geom_point()

# GLM (Poisson)
mod_pois<-glm(total_count~year_season+Wind+hour+day_of_season+PointLoc+tide_height+tide_dir,
          data=sum_by_season_site,family = "poisson")

resid_pois<-data.frame(
  pearsons_residuals=resid(mod_pois,type="pearson"),
  deviance_residuals=resid(mod_pois,type="deviance"),
  response_residuals=sum_by_season_site$total_count-predict(mod_pois,type = "response"),
  predicted=predict(mod_pois,type = "response"),
  observed=sum_by_season_site$total_count)

summary(mod_pois)

# check for overdispersion
# if not ~=1 than it is over or under dispersed.  if >1 over, if<1 under
# in this case it is much larger than 1 so we can't fit a poisson model!
summary(mod_pois)$deviance/summary(mod_pois)$df.residual

par(mfrow=c(2,2))
plot(mod_pois)

# still patterns
ggplot(resid_pois,aes(x=predicted,y=pearsons_residuals))+
  geom_point()+
  ggplot(resid_pois,aes(x=predicted,y=deviance_residuals))+
  geom_point()+
  ggplot(resid_pois,aes(x=predicted,y=response_residuals))+
  geom_point()+
  ggplot(resid_pois,aes(x=predicted,y=observed))+
  geom_point()

# GLM (quasioisson)
# accounts for over dispersion but can't calculate AIC and trouble for model selection
# but residuals seem a bit better.  But probably should use negitive binomial distribution
mod_qpois<-glm(total_count~year_season+hour+day_of_season+PointLoc+Wind+tide_height+tide_dir,
              data=sum_by_season_site,family = "quasipoisson")

resid_qpois<-data.frame(
  pearsons_residuals=resid(mod_qpois,type="pearson"),
  deviance_residuals=resid(mod_qpois,type="deviance"),
  response_residuals=sum_by_season_site$total_count-predict(mod_qpois,type = "response"),
  predicted=predict(mod_qpois,type = "response"),
  observed=sum_by_season_site$total_count)

summary(mod_qpois)
par(mfrow=c(2,2))
plot(mod_qpois)

ggplot(resid_qpois,aes(x=predicted,y=pearsons_residuals))+
  geom_point()+
  ggplot(resid_qpois,aes(x=predicted,y=deviance_residuals))+
  geom_point()+
  ggplot(resid_qpois,aes(x=predicted,y=response_residuals))+
  geom_point()+
  ggplot(resid_qpois,aes(x=predicted,y=observed))+
  geom_point()

# GLM (Negitive Binomial)
#  remove wind and hour and month
# there was an arc in the residuals for day_of_season so I added a quadratic (day_of_season2) to account for the fact that in the beginning and end of the season there are low numbers with high numbers in the middle of the season
mod_nb<-glm.nb(total_count~year_season+day_of_season+day_of_season2+PointLoc+tide_height+tide_dir,
               data=sum_by_season_site,link = "log")

## full model
# mod_nb<-glm.nb(total_count~year_season+month_of_season+day_of_season+day_of_season2+hour+Wind+PointLoc+tide_height+tide_dir,
#              data=sum_by_season_site,link = "log")

resid_nb<-data.frame(
  pearsons_residuals=resid(mod_nb,type="pearson"),
  deviance_residuals=resid(mod_nb,type="deviance"),
  response_residuals=sum_by_season_site$total_count-predict(mod_nb,type = "response"),
  predicted=predict(mod_nb,type = "response"),
  observed=sum_by_season_site$total_count)

summary(mod_nb)
# should look at pearson's or deviance residuals for eval
# deviance look ok
ggplot(resid_nb,aes(x=predicted,y=pearsons_residuals))+
  geom_point()+
  ggplot(resid_nb,aes(x=predicted,y=deviance_residuals))+
  geom_point()+
  ggplot(resid_nb,aes(x=predicted,y=response_residuals))+
  geom_point()+
  ggplot(resid_nb,aes(x=predicted,y=observed))+
  geom_point()

# normal plots don't look too bad?
par(mfrow=c(2,2))
plot(mod_nb)

# component residual plots show the effect of each variable accounting for all
# the other variables in the model.
# the blue line is least squares regression line and the pink is a smooth line
car::crPlots(mod_nb)
par(mfrow=c(1,1))

# both show a decine here
car::crPlots(mod_nb,terms = "year_season")


sum_by_season_site$pearsons_residuals=resid(mod_nb,type="pearson")
sum_by_season_site$deviance_residuals=resid(mod_nb,type="deviance")
sum_by_season_site$response_residuals=sum_by_season_site$total_count-predict(mod_nb,type = "response")
sum_by_season_site$predicted=predict(mod_nb,type = "response")
#
# Plot all the residuals by each variable to confirm there are not any patterns
#
# 2016 is a little low
ggplot(sum_by_season_site,aes(x=factor(year_season),y=deviance_residuals))+geom_boxplot()
# hour 6 and 15 are a bit different
ggplot(sum_by_season_site,aes(x=factor(hour),y=deviance_residuals))+geom_boxplot()
# no pattern now there was before i added the quadratic
ggplot(sum_by_season_site,aes(x=(day_of_season),y=deviance_residuals))+geom_point()
# no pattern
ggplot(sum_by_season_site,aes(x=(day_of_season2),y=deviance_residuals))+geom_point()
# no pattern
ggplot(sum_by_season_site,aes(x=factor(PointLoc),y=deviance_residuals))+geom_boxplot()
# hmmI think wind var is still a little wonky
ggplot(sum_by_season_site,aes(x=factor(Wind),y=deviance_residuals))+geom_boxplot()
# # no pattern
ggplot(sum_by_season_site,aes(x=factor(tide_height),y=deviance_residuals))+geom_boxplot()
# no pattern
ggplot(sum_by_season_site,aes(x=factor(tide_dir),y=deviance_residuals))+geom_boxplot()
# june ==9 and it is a bit differnt  might drop june?
ggplot(sum_by_season_site,aes(x=factor(month_of_season),y=deviance_residuals))+geom_boxplot()

# model selection
mod_nb_dredge<-dredge(mod_nb,extra = "R^2")
mod_nb_dredge %>% head(10)
# says we could drop month and wind for sure and maybe hour.
# Instead of dropping, we can model average and it will essentially do the same thing as removing them
# but model average would be the best model set, so removal could occur
#
# If we do drop month and wind for sure and  hour. there is one model that is WAY better than all the rest. no model averaging needed

# model averaging
model.avg(mod_nb_dredge) %>% summary

# year results?  30% decline per year?!
1-exp(-0.37)

