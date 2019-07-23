
# GLM analysis to identify trends in abundance
#
# This is an example script to identify terends in abundance for shorebirds in
# Estero La Cruz, Sonora, MX.  We go through several models and pick a GLM with
# a negative binomial distribution in the end.
#
# Author: Abram Fleishman
# Date created: 19 Apr 2019
# Date last modified: 19 Jul 2019
#
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
  # set your paths here Owen
  cruz<-read_csv("/Users/abramfleishman/google_drive/R data Processing/CSV/LaCruz/LaCruz_Guilds.csv")
  tast<-read_csv("/Users/abramfleishman/google_drive/R data Processing/CSV/Tastiota/Tastiota_guilds.csv")
  card<-read_csv("/Users/abramfleishman/google_drive/R data Processing/CSV/Cardonal/Cardonal_Guilds.csv")
}else{
  # paths for abram
  cruz<-read_csv("/Users/abramfleishman/google_drive/R data Processing/CSV/LaCruz/LaCruz_Guilds.csv")
  tast<-read_csv("/Users/abramfleishman/google_drive/R data Processing/CSV/Tastiota/Tastiota_guilds.csv")
  card<-read_csv("/Users/abramfleishman/google_drive/R data Processing/CSV/Cardonal/Cardonal_Guilds.csv")
  out_dir<-'/Users/abramfleishman/google_drive/Birds!/R data Processing/'
}

asdf<-as.data.frame


# GLM? --------------------------------------------------------------------
esteros<-readRDS(paste0(out_dir,"compiled_esteros_all_species_all_site_20190722.rds")) # %>% filter(estuary=="La Cruz")
esteros$guild %>% table
head(esteros) %>% asdf
esteros %>% select(day_of_season,year_season,point) %>% distinct %>% group_by(point,year_season) %>% count %>% asdf
# summarize by survey (group all the species)
sum_by_season_site <- esteros %>%
  # drop jul, aug, sep for few obs # months 5 and 6 have few birds so might not make sense to include
  filter(month(date)%in%c(1:6,10:12),
         estuary=="La Cruz",
         point%in%c("levee","oysterfarm","montecristo","puntacruz","crabcamp")) %>%
  # group by species season and date, this way we can get a total number of each
  # species seen grouping all the sites.
  group_by(  estuary,point,guild,
             year_season, month_of_season,day_of_season,day_of_season2,hour,
             tide_dir,tide_height,wind,cloud,tempf) %>%
  summarise(total_count=sum(density,na.rm=T)) %>%
  ungroup %>%
  filter(guild=="gulls, terns, and skimmers") %>%
  as.data.frame()

# quick checks for missing values and what unique values we have
table(sum_by_season_site$tide_height,useNA = "ifany")
table(sum_by_season_site$tide_dir,useNA = "ifany")
table(sum_by_season_site$hour,useNA = "ifany")
table(sum_by_season_site$cloud,useNA = "ifany")
table(sum_by_season_site$wind,useNA = "ifany")
# table(sum_by_season_site$tempf,useNA = "ifany")

table(sum_by_season_site$point)
table(sum_by_season_site$point,sum_by_season_site$year_season)
par(mfrow=c(1,1))
hist(sum_by_season_site$total_count,breaks = 100)

# plot ?  # need more plots to see what we would predict interms of patterns
ggplot(sum_by_season_site,aes(x=factor(guild),y = total_count*10000,fill=factor(year_season)))+
  geom_boxplot()+
  facet_wrap(~estuary+point,scales="free")+labs(y="Birds/Hectare")



# GLM (Gaussian) -----
# https://stats.stackexchange.com/questions/187824/how-to-model-non-negative-zero-inflated-continuous-data
sum_by_season_site$total_count <-sum_by_season_site$total_count *10000

mod_norm<-glm(total_count~year_season+wind+hour+day_of_season+point+tide_height+tide_dir,
              data=sum_by_season_site,family = Gamma(link = log))


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
mod_pois<-glm(total_count~year_season+wind+hour+day_of_season+point+tide_height+tide_dir,
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
mod_qpois<-glm(total_count~year_season+hour+day_of_season+point+wind+tide_height+tide_dir,
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


# zero inflated model -----------------------------------------------------


library(pscl)
sum_by_season_site$total_count<-round(sum_by_season_site$total_count)
mod_qpois<-zeroinfl(total_count~year_season+hour+day_of_season+point+wind+tide_height+tide_dir,
               data=sum_by_season_site,dist = "negbin")

resid_qpois<-data.frame(
  pearsons_residuals=resid(mod_qpois,type="pearson"),
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

# GLM (Negitive Binomial) -----
#  remove wind and hour and month?
# there was an arc in the residuals for day_of_season so I added a quadratic (day_of_season2) to account for the fact that in the beginning and end of the season there are low numbers with high numbers in the middle of the season

mod_nb<-glm.nb(total_count~year_season+day_of_season+point+
                 wind+cloud+tempf+
                 tide_height+tide_dir,
               data=sum_by_season_site,link = "log")

## full model
# mod_nb<-glm.nb(total_count~year_season+month_of_season+day_of_season+day_of_season2+hour+wind+point+tide_height+tide_dir,
             # data=sum_by_season_site,link = "identity")

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
ggplot(sum_by_season_site,aes(x=factor(point),y=deviance_residuals))+geom_boxplot()
# hmmI think wind var is still a little wonky
ggplot(sum_by_season_site,aes(x=factor(wind),y=deviance_residuals))+geom_boxplot()
# # no pattern
ggplot(sum_by_season_site,aes(x=factor(tide_height),y=deviance_residuals))+geom_boxplot()
# no pattern
ggplot(sum_by_season_site,aes(x=factor(tide_dir),y=deviance_residuals))+geom_boxplot()
# june ==9 and it is a bit differnt  might drop june?
ggplot(sum_by_season_site,aes(x=factor(month_of_season),y=deviance_residuals))+geom_boxplot()

# model selection
mod_nb_dredge<-dredge(mod_nb,extra = "R^2")
mod_nb_dredge %>% head(20)
# says we could drop month and wind for sure and maybe hour.
# Instead of dropping, we can model average and it will essentially do the same thing as removing them
# but model average would be the best model set, so removal could occur
#
# If we do drop month and wind for sure and  hour. there is one model that is WAY better than all the rest. no model averaging needed

# model averaging
model.avg(mod_nb_dredge) %>% summary

# year results?  30% decline per year?!
1-exp(-0.39)
