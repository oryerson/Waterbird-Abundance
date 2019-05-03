# This is a script to get summary statisitics 
# for all three estuaries
#
# Author: Owen Ryerson
# Created: April 28, 2019
# Last editied: April 30, 2019


rm(list=ls())

library(ggplot2)
library(dplyr)

#Load data
esteros <- readRDS("/Users/owenryerson/Google Drive/Birds!/R data Processing/CSV/All Estuaries rds/CSVcompiled_esteros_for_glm_27Apr19_newguilds.rds")

# remove duplicate species
esteros <- esteros %>% 
  mutate(species = recode(species, "Phalarope" = "Phalarope sp.", 
                  "Marbled Godwit/Long Billed Curlew"="Marbled Godwit/Long-billed Curlew"))

#reate data frame for each estuary 
tast <- filter(esteros, estuary == "Tastiota")
card <-filter(esteros, estuary == "Cardonal")
cruz <- filter(esteros, estuary == "La Cruz")

#Check sites
table(tast$point)
table(card$point)
table(cruz$point)

#Summary Stats ----------------------------------------------------------------------------------------------------------

# stats for all the estuaries together

# relative species abundance
esteros_sp <- group_by(esteros, species, guild) %>%
  summarise(sum(count, na.rm=TRUE)) %>%
  rename(count = 'sum(count, na.rm = TRUE)')
esteros_sp$total <- as.numeric(sum(esteros_sp$count))
esteros_sp$avgabundance <- esteros_sp$count/esteros_sp$total *100

esteros_sp_real <- filter(esteros_sp, count != 0) # number of species
sum(esteros$count) # total count of all sightings

# Sightings per season

Fall <- c(9,10,11)
Winter <- c(12,1,2)
Spring <-c(3,4,5)
Summer <- c(6,7,8)

esteros$season <- ifelse(esteros$month %in% Fall, 'Fall', ifelse(esteros$month %in% Winter, 'Winter',
                                                               ifelse(esteros$month %in% Spring, 'Spring',
                                                                      ifelse(esteros$month %in% Summer, 'Summer', 'issue'))))

szn_sum <- group_by(esteros, season) %>%
  summarise(sum(count))

szn_estuary_sum <- group_by(esteros, season, estuary) %>% # sightings per season by estuary
  summarise(sum(count))


## number of sight visits ##

sightvisits <- group_by(esteros, date, point, estuary, season) %>%
  summarise(sum(count))%>%
  rename(count = "sum(count)")
  
cruz_sightvisit <- filter(sightvisits, estuary == "La Cruz")
card_sightvisit <- filter(sightvisits, estuary == "Cardonal")
tast_sightvisit <- filter(sightvisits, estuary == "Tastiota")

# Guild Stats ----------------------------------------------------------------------------------------------------------

# sum by guild
Guild_sum <- group_by(esteros, guild) %>%
  summarise(sum(count)) %>%
  rename(count = 'sum(count)')

## Shorebirds ##

shorebirds_sp <- group_by(esteros, species, guild) %>%
  summarise(sum(count, na.rm=TRUE)) %>%
  rename(count = 'sum(count, na.rm = TRUE)') %>%
  filter(guild == "shorebirds")
shorebirds_sp$total <- as.numeric(sum(shorebirds_sp$count))
shorebirds_sp$avgabundance <- shorebirds_sp$count/shorebirds_sp$total *100

shorebird_szn_estuary <- group_by(esteros, season, estuary, guild) %>% # sightings per season by estuary
  summarise(sum(count))%>%
  rename(count = 'sum(count)') %>%
  filter(guild == "shorebirds")

sum(shorebird_szn_estuary$count, shorebird_szn_estuary$season == 'winter') # total winter count

peeps <- filter(shorebirds_sp, species %in% c("Peep sp.", "Western Sandpiper", "Least Sandpiper", "Spotted Sandpiper"))
sum(peeps$avgabundance)

## Gulls, Terns, and Skimmers ##

gulls_t_s_sp <- group_by(esteros, species, guild) %>%
  summarise(sum(count, na.rm=TRUE)) %>%
  rename(count = 'sum(count, na.rm = TRUE)') %>%
  filter(guild == "gulls, terns, and skimmers")
gulls_t_s_sp$total <- as.numeric(sum(gulls_t_s_sp$count))
gulls_t_s_sp$avgabundance <- gulls_t_s_sp$count/gulls_t_s_sp$total *100

gulls_t_s_szn_estuary <- group_by(esteros, season, estuary, guild) %>% # sightings per season by estuary
  summarise(sum(count))%>%
  rename(count = 'sum(count)') %>%
  filter(guild == "gulls, terns, and skimmers")

## pelicans and allies ##

pelicans_sp <- group_by(esteros, species, guild) %>%
  summarise(sum(count, na.rm=TRUE)) %>%
  rename(count = 'sum(count, na.rm = TRUE)') %>%
  filter(guild == "pelicans and allies")
pelicans_sp$total <- as.numeric(sum(pelicans_sp$count))
pelicans_sp$avgabundance <- pelicans_sp$count/pelicans_sp$total *100

pelican_szn_estuary <- group_by(esteros, season, estuary, guild) %>% # sightings per season by estuary
  summarise(sum(count))%>%
  rename(count = 'sum(count)') %>%
  filter(guild == "pelicans and allies")

## Long-legged Waders ##

waders_sp <- group_by(esteros, species, guild) %>%
  summarise(sum(count, na.rm=TRUE)) %>%
  rename(count = 'sum(count, na.rm = TRUE)') %>%
  filter(guild == "long-legged waders")
waders_sp$total <- as.numeric(sum(waders_sp$count))
waders_sp$avgabundance <- waders_sp$count/waders_sp$total *100

waders_szn_estuary <- group_by(esteros, season, estuary, guild) %>% # sightings per season by estuary
  summarise(sum(count))%>%
  rename(count = 'sum(count)') %>%
  filter(guild == "long-legged waders")

## Waterfoul ##

waterfoul_sp <- group_by(esteros, species, guild) %>%
  summarise(sum(count, na.rm=TRUE)) %>%
  rename(count = 'sum(count, na.rm = TRUE)') %>%
  filter(guild == "waterfoul")
waterfoul_sp$total <- as.numeric(sum(waterfoul_sp$count))
waterfoul_sp$avgabundance <- waterfoul_sp$count/waterfoul_sp$total *100

waterfoul_szn_estuary <- group_by(esteros, season, estuary, guild) %>% # sightings per season by estuary
  summarise(sum(count))%>%
  rename(count = 'sum(count)') %>%
  filter(guild == "waterfoul")

## divers ##

divers_sp <- group_by(esteros, species, guild) %>%
  summarise(sum(count, na.rm=TRUE)) %>%
  rename(count = 'sum(count, na.rm = TRUE)') %>%
  filter(guild == "divers")
divers_sp$total <- as.numeric(sum(divers_sp$count))
divers_sp$avgabundance <- divers_sp$count/divers_sp$total *100

divers_szn_estuary <- group_by(esteros, season, estuary, guild) %>% # sightings per season by estuary
  summarise(sum(count))%>%
  rename(count = 'sum(count)') %>%
  filter(guild == "divers")

#Individual Estuary Stats -----------------------------------------------------------------------------------------------

# Relative species abundance ----------------------------------------------------------
options(scipen=999)

# La Cruz
cruz_sp <- group_by(cruz, species) %>%
  summarise(sum(count, na.rm=TRUE)) %>%
  rename(count = 'sum(count, na.rm = TRUE)')
cruz_sp$total <- as.numeric(sum(cruz_sp$count))
cruz_sp$avgabundance <- cruz_sp$count/cruz_sp$total *100

cruz_sp_real <- filter(cruz_sp, count != 0) # number of speceis

sum(cruz$count) # total count

# Cardonal
card_sp <- group_by(card, species) %>%
  summarise(sum(count, na.rm=TRUE)) %>%
  rename(count = 'sum(count, na.rm = TRUE)')
card_sp$total <- as.numeric(sum(card_sp$count))
card_sp$avgabundance <- card_sp$count/card_sp$total *100

card_sp_real <- filter(card_sp, count != 0) # number of species

sum(card$count) # total count

#Tastiota
tast_sp <- group_by(tast, species) %>%
  summarise(sum(count, na.rm=TRUE)) %>%
  rename(count = 'sum(count, na.rm = TRUE)')
tast_sp$total <- as.numeric(sum(tast_sp$count))
tast_sp$avgabundance <- tast_sp$count/tast_sp$total *100

tast_sp_real <- filter(tast_sp, count != 0) # number of species

sum(tast$count) # total count


## Guild Abundance ----------------------------------------------------------------------

#La Cruz
cruz_guild <- group_by(cruz, guild) %>%
  summarise(sum(count, na.rm=TRUE)) %>%
  rename(count = 'sum(count, na.rm = TRUE)')
cruz_guild$total <- as.numeric(sum(cruz_guild$count))
cruz_guild$avgabundance <- cruz_guild$count/cruz_guild$total *100

# Cardonal
card_guild <- group_by(card, guild) %>%
  summarise(sum(count, na.rm=TRUE)) %>%
  rename(count = 'sum(count, na.rm = TRUE)')
card_guild$total <- as.numeric(sum(card_guild$count))
card_guild$avgabundance <- card_guild$count/card_guild$total *100

#Tastiota
tast_guild <- group_by(tast, guild) %>%
  summarise(sum(count, na.rm=TRUE)) %>%
  rename(count = 'sum(count, na.rm = TRUE)')
tast_guild$total <- as.numeric(sum(tast_guild$count))
tast_guild$avgabundance <- tast_guild$count/tast_guild$total *100


