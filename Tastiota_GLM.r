rm(list=ls())


library(dplyr)

### Load and pepare data ###

setwd("/Users/owenryerson/Google Drive/Birds!/R data Processing/CSV/Tastiota") #"Tastiota_Guilds.csv"

getwd()

files<-dir()
files

tg <- read.csv(files[8], stringsAsFactors = FALSE)

#Prep Data --------------------------------------------------------------------------------------------------

tg$DateTime <- as.POSIXct(tg$Date , format = "%m/%d/%y" ) 

tg$monthname <- months(tg$DateTime)

tg$season_num <- as.numeric(substr(tg$season,0,2)) #make a numeric season column 


#Create Julian Days --------------------------------------------------------------------------------------------------

## create julian days for each leap year group to avoid incorrect values

s2 <- c("12-13", "13-14", "14-15", "15-16")
szn2<- filter(tg, season %in% s2)
szn2$jday <- as.integer(julian(szn2$DateTime,origin= '2012-09-01'))%%365L


s3 <- c("16-17", "17-18")
szn3<- filter(tg, season %in% s3)
szn3$jday <- as.integer(julian(szn3$DateTime,origin= '2016-09-01'))%%365L

tg <- rbind(szn2, szn3)

# create daily averages --------------------------------------------------------------------------------------

dailyavg <- group_by(tg, DateTime, season_num, jday, PointLoc, Tide, Wind, Observer1, guild, TempF) %>%
  summarise(sum(Count, na.rm = T)) %>%
  rename(count = 'sum(Count, na.rm = T)')
dailyavg$Wind <- as.numeric(dailyavg$Wind)
dailyavg$TempF <- as.numeric(dailyavg$TempF)

dailyavg$PointLoc <-as.factor(dailyavg$PointLoc) 
dailyavg$PointLoc2 <-as.numeric(dailyavg$PointLoc) ## make point loc have a numeric value

dailyavg$Tide <-as.factor(dailyavg$Tide)
dailyavg$Tide2 <-as.numeric(dailyavg$Tide)

guilds <- unique(tg$guild)

for (i in 1:length(guilds)) {
  x <- filter(dailyavg, guild == guilds[i] )
  glm <-glm(count~season+monthname+PointLoc+Tide+Wind+Observer1, data=x)
  summary(glm) ## doesn't work yet
}

x <- dailyavg %>%
  filter( guild == "shorebird" )

glmpois <- glm(count~season_num+jday+PointLoc2+Tide+Wind+TempF, data = x, family =poisson()) # Poisson distributed GLM
summary(glmpois)

with(glmpois, cbind(res.deviance = deviance, df = df.residual,
               p = pchisq(deviance, df.residual, lower.tail=FALSE))) # tests to see if model fits data, if p is > 0.05 it fits

glmnotemp <- glm(count~season_num+jday+PointLoc2+Tide+Wind, data = x, family =poisson()) # Poisson distributed GLM with no temp
summary(glmnotemp)


glmnoj <- glm(count~season_num+PointLoc2+Tide+Wind+TempF, data = x, family =poisson()) # Poisson distributed GLM with no j-day
summary(glmnoj)

#Correlation test 
l <- cor.test(x$jday, x$TempF, method = "kendall")
l # there is no correlation between j-day and temp


# Poisson distributed GLM with log of response variable
glmlog <- glm(log(count)~season_num+jday+PointLoc2+Tide+Wind+TempF, data = x, family =poisson()) 
summary(glmlog)
plot(glmlog, which = 1)

with(glmlog, cbind(res.deviance = deviance, df = df.residual,
                    p = pchisq(deviance, df.residual, lower.tail=FALSE))) 

AIC <- AICc(glmpois, glmnotemp, glmnoj, glmlog)
AIC





glm <- glm(count~season+monthname+PointLoc+Tide+Wind+Observer1, data = x) # normal GLM

summary(glm)
