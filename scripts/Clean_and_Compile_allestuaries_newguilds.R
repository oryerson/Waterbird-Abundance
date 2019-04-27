# GLM analysis to identify trends in abundance
#
# This is an example script to identify terends in abundance for shorebirds in
# Estero La Cruz, Sonora, MX.  We go through several models and pick a GLM with
# a negative binomial distribution in the end.
#
# Author: Abram Fleishman
# Date created: 19 Apr 2019
# Date last modified: 19 Apr 2019
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
  cruz<-read_csv("/Users/owenryerson/Google Drive/Birds!/R data Processing/CSV/LaCruz/LaCruz_New_Guilds.csv")
  tast<-read_csv("/Users/owenryerson/Google Drive/Birds!/R data Processing/CSV/Tastiota/Tastiota_New_guilds.csv")
  card<-read_csv("/Users/owenryerson/Google Drive/Birds!/R data Processing/CSV/Cardonal/Cardonal_New_Guilds.csv")
  out_dir<-'/Users/owenryerson/Google Drive/Birds!/R data Processing/CSV'
}else{
  # paths for abram
  cruz<-read_csv("/Users/abramfleishman/google_drive/R data Processing/CSV/LaCruz/LaCruz_Guilds.csv")
  tast<-read_csv("/Users/abramfleishman/google_drive/R data Processing/CSV/Tastiota/Tastiota_guilds.csv")
  card<-read_csv("/Users/abramfleishman/google_drive/R data Processing/CSV/Cardonal/Cardonal_Guilds.csv")
  out_dir<-'/Users/abramfleishman/google_drive/R data Processing/'
}

asdf<-as.data.frame

# Clean cruz --------------------------------------------------------------
head(cruz) %>% asdf
table(is.na(cruz$DateTime))
cruz %>% names
unique(cruz$hour)
cruz$TimeStart %>% sort%>% as.character() %>% unique()

cruz_clean<-cruz %>%
  mutate(date=ymd(DateTime), # Make date
         # add a numeric season year
         year_season=ifelse(monthnum<8,year(date),year(date)+1),
         # add a day of season
         day_of_season = as.numeric(date-ymd(paste(year(date),"09","01",sep = "-"))),
         day_of_season = ifelse(day_of_season <0,day_of_season +365,day_of_season ),
         #a dd a quadradic term
         day_of_season2 = day_of_season^2,
         # add month of season
         month_of_season = ifelse(monthnum<8, monthnum+3,monthnum-8),
         # add hour
         hour=hour(hms(TimeStart)),
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
         PointLoc=gsub("la|santasite| ","",tolower(PointLoc)),
         PointLoc=recode(PointLoc,
                         "crabcoop"="crabcamp",
                         "oystercamp"="oysterfarm"),
         Estuary="La Cruz",
         Cloud=as.numeric(gsub("%","",Cloud)),
         Cloud=ifelse(is.na(Cloud),round(mean(Cloud,na.rm=T)),Cloud),
         TempF=ifelse(TempF==0,round(mean(TempF,na.rm=T)),TempF),
         TempF=ifelse(is.na(TempF),round(mean(TempF,na.rm=T)),TempF)) %>%
  filter(PointLoc!="restaurant") %>%
  select(Estuary,Point=PointLoc,date,TimeStart,year_season,month=monthnum,month_of_season,day_of_season,day_of_season2,hour,tide_height,tide_dir,Wind,Cloud,TempF,guild,Species,Count)

names(cruz_clean)<-tolower(names(cruz_clean))

head(cruz_clean) %>% asdf

table(cruz_clean$cloud)
table(cruz_clean$hour)
table(cruz_clean$tempf)
table(cruz_clean$wind)
table(cruz_clean$point)
table(cruz_clean$guild)

# oops!  looks like we do not have zeros!  aka when a bird was not seen there is
# no row for it on that survey!  This is a problem for modeling species but
# maybe not for modeling guilds.  need to look into it and understand better
table(cruz_clean$species)


# Clean Tastiota ----------------------------------------------------------


head(tast) %>% asdf
table(tast$Estuary)
table(tast$Tide %>% tolower)
table(tast$Cloudcover)
table(tast$TempF )
table(tast$Precip)
table(tast$Wind )
table(tast$PointLoc )  # I am almost possitive that Camp is the same as town, they are not ever on the same day. 
                       # I think Mudflat and Mudflat East are the same, but not 100% the sure

tast <- filter(tast, PointLoc != 'Mudflat East') # Mudflat East is not used in every year

tast_clean<-tast %>%
  mutate(date=ymd(DateTime), # Make date
         # add a numeric season year
         monthnum=month(date),
         year_season=ifelse(monthnum<8,year(date),year(date)+1),
         # add a day of season
         day_of_season = as.numeric(date-ymd(paste(year(date),"09","01",sep = "-"))),
         day_of_season = ifelse(day_of_season <0,day_of_season +365,day_of_season ),
         #a dd a quadradic term
         day_of_season2 = day_of_season^2,
         # add month of season
         month_of_season = ifelse(monthnum<8, monthnum+3,monthnum-8),
         # add hour
         hour=hour(hms(TimeStart)),
         # fix hour
         hour=as.numeric(ifelse(hour>500,str_extract(hour,"^[0-9]{2}"),hour)),
         hour=ifelse(is.na(hour),floor(mean(hour,na.rm=T)),hour),
         hour=ifelse(hour==2,14,hour),
         hour=ifelse(hour==1,13,hour),
         hour=ifelse(hour==5,17,hour),
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
         
         Estuary=recode(Estuary,
                        "Tastota"="Tastiota"),
         Cloudcover=as.numeric(gsub("%","",Cloudcover)),
         Cloudcover=ifelse(is.na(Cloudcover),round(mean(Cloudcover,na.rm=T)),Cloudcover),
         TempF=ifelse(is.na(TempF),round(mean(TempF,na.rm=T)),TempF)) %>%
  filter(PointLoc!="restaurant") %>%
  select(Estuary,Point=PointLoc,date,TimeStart,year_season,month=monthnum,month_of_season,day_of_season,day_of_season2,hour,tide_height,tide_dir,Wind,Cloud=Cloudcover,TempF,guild,Species,Count)

names(tast_clean)<-tolower(names(tast_clean))

head(tast_clean) %>% asdf
tast$TimeStart %>% sort%>% as.character() %>% unique()
tast_clean$hour %>% table
tast_clean$tide_height %>% table
tast_clean$tide_dir %>% table



# Clean Cardonal ----------------------------------------------------------


head(card) %>% asdf
unique(card$DateTime)
card$TimeStart %>% sort%>% as.character() %>% unique()
card$TimeEnd %>% sort%>% as.character() %>% unique()

#Clean Up Sites

table(card$PointLoc)

site <- c("Canal","Canal new", "lake", "Lake", "Panga") # these are the sites that are consistant across all years

card <- filter(card, PointLoc %in% site)

# remove tastiota and word estero
table(card$Estuary)

# need to clean up
table(card$Tide %>% tolower)
# neeed to remove "%"
table(card$Cloudcover)
table(card$TempF )

# what to do with the precip?  remove?
table(card$Precip)
# yikes wind need help!
table(card$Wind )

# need to paredown to the point counts and fix names
table(card$PointLoc %>% tolower())


card_clean<-card %>%
  mutate(date=ymd(DateTime), # Make date
         # add a numeric season year
         monthnum=month(date),
         year_season=ifelse(monthnum<8,year(date),year(date)+1),
         # add a day of season
         day_of_season = as.numeric(date-ymd(paste(year(date),"09","01",sep = "-"))),
         day_of_season = ifelse(day_of_season <0,day_of_season +365,day_of_season ),
         #a dd a quadradic term
         day_of_season2 = day_of_season^2,
         # add month of season
         month_of_season = ifelse(monthnum<8, monthnum+3,monthnum-8),
         # add hour
         hour=hour(hm(TimeStart)),
         # fix hour
         hour=as.numeric(ifelse(hour>500&hour<960,str_extract(hour,"^[0-9]{1}"),hour)),
         hour=as.numeric(ifelse(hour>999,str_extract(hour,"^[0-9]{2}"),hour)),
         hour=ifelse(is.na(hour),floor(mean(hour,na.rm=T)),hour),
         hour=ifelse(hour==2,14,hour),
         hour=ifelse(hour==1,13,hour),
         hour=ifelse(hour==5,17,hour),
         # add missing tides and winds
         Tide=ifelse(is.na(Tide),"mid-rising",Tide),
         # wind needs some real fixing
         Wind=case_when(
           season%in%c("09-10","10-11","11-12","12-13")&Wind%in%c("light","NW 2") ~ 1,
           season%in%c("09-10","10-11","11-12","12-13")&Wind%in%c("3","4","5","6","NW 5", "SW 5") ~ 2,
           season%in%c("09-10","10-11","11-12","12-13")&Wind%in%c("10",'8','7','SW 8') ~ 3,
           season%in%c("09-10","10-11","11-12","12-13")&Wind%in%c("12","15","15-20") ~ 4,
           season%in%c("09-10","10-11","11-12","12-13")&Wind%in%c("18") ~ 5,
           Wind%in%c("1 to 2") ~ 1.5,
           Wind%in%c("2 to 3") ~ 2.5,
           is.na(Wind) ~3,
           TRUE ~0),
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
         PointLoc=gsub(" ","_",tolower(PointLoc)),
         PointLoc=ifelse(is.na(PointLoc),"travelling",PointLoc),
         Estuary=gsub("Estero ","",Estuary),
         Cloudcover=as.numeric(gsub("%","",Cloudcover)),
         Cloudcover=ifelse(is.na(Cloudcover),round(mean(Cloudcover,na.rm=T)),Cloudcover),
         TempF=ifelse(is.na(TempF),round(mean(TempF,na.rm=T)),TempF)) %>%
  filter(Estuary!="Tastiota") %>%
  select(Estuary,Point=PointLoc,date,TimeStart,year_season,month=monthnum,month_of_season,day_of_season,day_of_season2,hour,tide_height,tide_dir,Wind,Cloud=Cloudcover,TempF,guild,Species,Count)

names(card_clean)<-tolower(names(card_clean))


unique(card_clean$wind)
head(card_clean) %>% asdf
card_clean$wind %>%  table(.,useNA = "ifany")

# card$Wind %>%  table(.,useNA = "ifany")

card_clean$hour %>% table(.,useNA = "ifany")
card_clean$cloud %>% table(.,useNA = "ifany")
card_clean$point %>%  table(.,useNA = "ifany")
card_clean$point %>%  table(.,useNA = "ifany")
card_clean$tide_height %>%  table(.,useNA = "ifany")
card_clean$tide_dir %>%  table(.,useNA = "ifany")
ncol(card_clean)
ncol(cruz_clean)
ncol(tast_clean)
sapply(cruz_clean, function(x)sum(is.na(x)))
sapply(tast_clean, function(x)sum(is.na(x)))

esteros <- bind_rows(cruz_clean %>% select(-timestart),
                     card_clean%>% select(-timestart),
                     tast_clean%>% select(-timestart)) %>%
  mutate(guild=tolower(guild),
         point = tolower(point))


# Impute zeros ------------------------------------------------------------
bigs<-esteros %>% select(-guild,-species,-count) %>% distinct()
counts<-esteros %>% select(date,point,species,count)
guild_species<-esteros %>% select(guild,species) %>% distinct()
species<-unique(esteros$species)
poin<-unique(esteros$point)
point_species<-expand.grid(point=poin,species=species,stringsAsFactors = F) %>%
  left_join(guild_species)
esteros<-bigs %>% left_join(point_species) %>% left_join(counts) %>%
  mutate(count=ifelse(is.na(count),0,count))
head(esteros) %>% asdf


# checks ------------------------------------------------------------------

sapply(esteros, function(x)sum(is.na(x)))
table(esteros$estuary,is.na(esteros$cloud))
table(esteros$estuary,is.na(esteros$tempf))
# save --------------------------------------------------------------------



saveRDS(esteros,paste0(out_dir,"compiled_esteros_for_glm_27Apr19_newguilds.rds"))

© 2019 GitHub, Inc.
Terms
Privacy
Security
Status
Help
Contact GitHub
Pricing
API
Training
Blog
About

