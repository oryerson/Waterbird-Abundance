# This Script Compiles and Cleans the raw data from Laguna La Cruz
# it does not remove any data

# Creator: Owen Ryerson
# Created on May 3, 2019
# Last edited May 5,2019
rm(list=ls())  ##clears everything

library(Kendall)
library(dplyr)
library(ggplot2)
library(foreign)


setwd("/Users/owenryerson/Desktop/Birds!/R data Processing/CSV/LaCruz")

getwd()

files<-dir()
files

#double check paths
lc0910 <- read.csv(files[5], stringsAsFactors = FALSE) # Csv called "ExtraSitesLaCruz_2009_2010"
lc1011 <- read.csv(files[6], stringsAsFactors = FALSE) # Csv called "ExtraSitesLaCruz_2010_2011"
lc1213 <- read.csv(files[9], stringsAsFactors = FALSE)
lc1314 <- read.csv(files[10], stringsAsFactors = FALSE)
lc1415 <- read.csv(files[11], stringsAsFactors = FALSE)
lc1516 <- read.csv(files[12], stringsAsFactors = FALSE)
lc1617 <- read.csv(files[13], stringsAsFactors = FALSE)
lc1718 <- read.csv(files[14], stringsAsFactors = FALSE)

#Convert datetime to posix format
lc1718$DateTime <- as.POSIXct(lc1718$Date , format = "%m/%d/%y" ) 
lc1617$DateTime <- as.POSIXct(lc1617$Date , format = "%m/%d/%y" ) 
lc1516$DateTime <- as.POSIXct(lc1516$Date , format = "%m/%d/%y" )
lc1415$DateTime <- as.POSIXct(lc1415$Date , format = "%m/%d/%y" )
lc1314$DateTime <- as.POSIXct(lc1314$Date , format = "%m/%d/%y" )
lc1213$DateTime <- as.POSIXct(lc1213$Date , format = "%d-%b-%y" )
lc1011$DateTime <- as.POSIXct(lc1011$Date , format = "%m/%d/%y" )
lc0910$DateTime <- as.POSIXct(lc0910$Date , format = "%m/%d/%y" )


 ##Season column

lc0910$season <- rep( "09-10" ,nrow(lc0910))
lc1011$season <- rep( "10-11" ,nrow(lc1011))
lc1213$season <- rep( "12-13" ,nrow(lc1213))
lc1314$season <- rep( "13-14" ,nrow(lc1314))
lc1415$season <- rep( "14-15" ,nrow(lc1415))
lc1516$season <- rep( "15-16" ,nrow(lc1516))
lc1617$season <- rep( "16-17" ,nrow(lc1617))
lc1718$season <- rep( "17-18" ,nrow(lc1718))



###merge all years to one data frame: ###

lc0910 <- rename(lc0910, Wind = Windspeed.MPH)
lc0910 <- select(lc0910, -X, -X.1, -X.2, -X.3)
lc0910 <- select(lc0910, -Notes)
lc1011 <- rename(lc1011, Wind = Windspeed.MPH)

lc1213 <- rename(lc1213, Observer1 = Observer.1 , Observer2 = Observer.2,
                 Cloud = Cloud.cover, TempF = Temp.F., PointLoc = Point.Location, TimeStart = Time.Started,
                 Timeend = Time.Ended, Precip = Precipitation )
lc1213 <- select(lc1213, -Notes)
lc1213 <- rename(lc1213, TimeEnd = Timeend)

lc1314 <- rename(lc1314, Observer1 = Observer.1 , Observer2 = Observer.2,
                 Cloud = Cloud.cover, TempF = Temp.F., PointLoc = Point.Location, TimeStart = Time.Started,
                 Timeend = Time.Ended, Precip = Precipitation )
lc1314 <- select(lc1314, -Notes, -X)
lc1314 <- rename(lc1314, TimeEnd = Timeend)
lc1314 <- lc1314[-2119,]

lc1415 <- rename(lc1415, Observer1 = Observer.1 , Observer2 = Observer.2,
                 Cloud = Cloud.cover, TempF = Temp.F., PointLoc = Point.Location, TimeStart = Time.Started,
                 Timeend = Time.Ended, Precip = Precipitation )
lc1415 <- rename(lc1415,  TimeEnd = Timeend)
lc1415 <- select(lc1415, -Notes, -X, -X.1, -X.2, -X.2, -X.3, -X.4, -X.5, -X.6, -X.7)

lc1516 <- rename(lc1516, Wind= WindBeaufort)
lc1516 <- select(lc1516, -Notes, -X, -X.1, -X.2, -X.2, -X.3, -X.4, -X.5)

lc1617 <- rename(lc1617,
                  Precip = Precipitation )
lc1617 <- select(lc1617, -Notes)
lc1617 <- lc1617[-c(1415,1416),]
lc1617[,14] <- as.numeric(lc1617[,14])

lc1718 <- rename(lc1718, Wind= WindBeaufort )
lc1718 <- select(lc1718, -Notes, -X, -X.1, -X.2, -X.2, -X.3, -Entered.By, -Proofed.By)
lc1718 <- lc1718[-c(1368,1369),]
lc1718[,14] <- as.numeric(lc1718[,14])

allyears <- rbind(lc0910, lc1011, lc1213, lc1314, lc1415, lc1516, lc1617, lc1718)
View(allyears)
allyears[,14] <- as.numeric(allyears[,14])


## Read in DBF file of four letter codes---------------------------------------------------

codes <- read.dbf("/Users/owenryerson/Desktop/Birds!/Misc Birds/4 letter codes/LIST18.DBF")
codes<- select(codes, SPEC,COMMONNAME)%>%
  rename("code4"="SPEC")

allyearscodes<-rename(allyears, "code4"= "Species")

newallyears<- left_join(allyearscodes,codes, by = "code4") # join common names to 4 letter codes

newallyears$COMMONNAME<- as.character(newallyears$COMMONNAME) #this keeps the the next step from acting all kinds of weird

newallyears <- newallyears %>% mutate(COMMONNAME=ifelse(is.na(COMMONNAME), code4, COMMONNAME))%>% # create a clumn with common names only
  select(-code4)%>%
  rename("Species"="COMMONNAME")


## Mark Common Speceies---------------------------------------------------------------------------------------

species <- unique(newallyears$Species)


Landbirds <- c('American Kestrel', 'Barn Swallow', 'Ash-throated Flycatcher', 'Black Vulture', "Bulluck's Oriole", 'Cactus Wren',
               'Common Raven', "Cooper's Hawk", "Cooper's hawk", 'Crested Caracara', 'Dark Ardeids', 'Gila Woodpecker',
               'Great-tailed Grackle', "Harris's Hawk", "House Sparrow", "Ladder-backed Woodpecker", 'Lark Sparrow', 
               'LoggerheadShrike', 'Mangrove Swallow', 'Mangrove Warbler', 'Merlin', 'Northen FLicker', 'Northern Harrier',
               'Northern Mockingbird','Peregrine Falcon', 'Red-Tailed Hawk', 'Savannah Sparrow', 'Turkey Vulture', 'Verdin', 
               'Vermillion Flycatcher', 'Vesper Sparrow', 'Violet-green Sparrow', 'White Ardeids', 'White-winged Dove',
               'Black-throated Sparrow', 'Loggerhead Shrike', 'Northern Flicker', 'Rock Wren',
               'Violet-green Swallow', 'Western Meadowlark')

newallyears$landbird <- ifelse(newallyears$Species %in% Landbirds, T, F)

# remove extra rows ----------------------------------------------------------------------------------------------------
newallyears <- filter(newallyears, Species != "Total Count:") # remove a yearly sum
newallyears <- filter(newallyears, Species != "No Data") # remove random rows
newallyears <- filter(newallyears, Species != "No Data ") # remove random rows
newallyears$Species <- na.omit(newallyears$Species) # remove random rows
newallyears <- filter(newallyears, Species != "") # remove random rows

# rename incorrect species names ------------------------------------------------------------------------------------


cleanallyears <- newallyears %>% mutate(Species = recode(Species, "Black-Bellied Plover" = "Black-bellied Plover",
                                                        "Dunlin "="Dunlin", "Gull sp. "="Gull sp.",
                                                        "Gull Sp."="Gull sp.", "HEGU"="Herring Gull",
                                                        "Large Shorebird sp. "="Large Shorebird sp.",
                                                        "Magnificant Frigatebird"="Magnificent Frigatebird",
                                                        "Medium Shorebird"="Medium Shorebird sp.",
                                                        "Osprey "="Ospery", "Osprey  "="Osprey",
                                                        "Peep sp. "="Peep sp.", "Peeps"="Peep sp.",
                                                        "Plover sp"="Plover sp.",
                                                        "Red-breated merganser"="Red-breasted Merganser",
                                                        "Red-Breasted Merganser"="Red-breasted Merganser",
                                                        "Small Shorebird sp. "="Small Shorebird sp.",
                                                        "Sterna Sp."="Sterna sp.", "Tern sp."="Tern sp.",
                                                        "Yellow-crowned night-heron"="Yellow-crowned Night-Heron"))

cleanallyears <- cleanallyears %>% mutate(Species = recode(Species, "BFBO "="Blue-footed Booby", "BRAN " = "Brant", 
                                                           "WILL "="Willet", "WLPL"="Wilson's Plover", 
                                                           "MAGO "="Marbled Godwit", "Ospery"="Osprey",
                                                           "REDH "="Redhead", "ROYT/ELTE"="Ro/El Tern",
                                                           "SEPL "="Semipalmated Plover", 
                                                           "Tern sp. "="Tern sp.",
                                                           "Red-breated merganser "="Red-breasted Merganser",
                                                            "Double-crested cormorant"="Double-crested Cormorant",
                                                            "Double-Crested Cormorant"="Double-crested Cormorant",
                                                           "Brant's Cormorants"="Brandt's Cormorant",
                                                           "Brant's Cormorant"="Brandt's Cormorant",
                                                           "Red-breasted merganser"="Red-breasted Merganser"))
                                                         
                                                         
species <- unique(cleanallyears$Species)
View(species)

# Recode Site names
table(cleanallyears$PointLoc)
cleanallyears <- cleanallyears %>% mutate(PointLoc = recode(PointLoc, "Crab Coop"= "Crab Camp", "Monte Cristo"="Monteristo",
                                                            "Punta Santa Cruz"="Punta La Cruz", "Oyster Farm"="Oyster Camp", 
                                                            "Restaurant"="Oyster Camp"))

# write.csv(cleanallyears, "LaCruz_09-18_AllData.csv")



# Create Guilds ----------------------------------------------------------------------------------------------------

# Shorebirds


sb <- c("American Avocet", "American Oystercatcher", "Black Turnstone", "Black-bellied Plover", 
        "Black-necked Stilt", "Dowitcher sp.", "Dunlin", "Greater Yellowlegs", "Killdeer", "Large Shorebird sp.",
        "Least Sandpiper", "Lesser Yellowlegs", "Long-billed Curlew", "Long-billed Dowitcher", "Marbled Godwit",
        "Marbled Godwit/Long Billed Curlew", "Medium Shorebird sp.", "Peep sp.", "Plover sp.", "Rail sp.", 
        "Red Knot", "Roseate Spoonbill", "Ruddy Turnstone", "Sanderling", "Sandpiper sp.", 
        "Semipalmated Plover", "Short-billed Dowitcher", "Small Shorebird sp.", "Snowy Plover", "Spotted Sandpiper", 
        "Surfbird", "Turnstone sp.", "Wandering Tattler", "Western Sandpiper", "Whimbrel", "Willet", 
        "Wilson's Plover", "Yellowlegs sp.", "Marbled Godwit/Long-billed Curlew")


# Divers

d <- c("Common Loon", "Loon sp.", "Pacific Loon", "Clark's Grebe",
       "Pied-billed Grebe", "Red-throated Loon", "Western Grebe",
       "Eared Grebe", "Grebe sp.", "Horned Grebe", "Surf Scoter")


# Pelicans and Allies

p <- c("American White Pelican", "Blue-footed Booby", "Brown Booby",
       "Brown Pelican", "Double-crested Cormorant", "Neotropic Cormorant",
       "Brandt's Cormorant")


# long-legged waders

llw <- c("Black-crowned Night-Heron", "Egret sp.", "Great Blue Heron", "Great Egret", "Green Heron",
         "Heron sp.", "Little Blue Heron", "Reddish Egret", "Snowy Egret", "Tricolored Heron",
         "Snowy Egret", "Cattle Egret", "Yellow-crowned Night-Heron", "White Ibis", "White-faced Ibis")


# Waterfoul

wf <- c("Common Merganser", "Merganser sp.", "Red-breasted Merganser", "Brant", "American Coot",
        "Greater White-fronted Goose", "Green-winged Teal", "Redhead", "Ring-necked Duck",
        "Ring-billed Duck", "Snow Goose", "Bufflehead", "Cinnamon Teal", "Ruddy Duck",
        "Scaup sp.", "Greater Scaup", "Lesser Scaup", "Common Goldeneye", "Blue-winged Teal",
        "Merganser Species", "Waterfowl sp.")


# Gulls, Terns, and Skimmers

gts <- c("Black Skimmer", "Black Tern", "Large Tern sp.", "Least Tern",
         "Elegant Tern", "Forster's Tern", "Common Tern", "Caspian Tern",
         "Sterna sp.", "Tern sp.", "Royal Tern", "Royal Tern/Elegant Tern",
         "Small Tern sp.", "Bonaparte's Gull", "California Gull", 
         "California/Herring Gull", "Franklin's Gull", "Glaucous-winged Gull",
         "Gull sp.", "Gull-billed Tern", "Gull/Tern sp.", "Heermann's Gull",
         "Herring Gull", "Laughing Gull", "Ring-billed Gull", "Sabine's Gull",
         "Yellow-footed Gull", "Ro/El Tern", "Laughing Gull/Bonaparte's Gull")


#Marchbirds

mb <- c("Clapper Rail", "Least Bittern", "Sandhill Crane", "Ridgway's Rail")


#Dabbles

db <- c("Northern Pintail", "Northern Shoveler", "Wilson's Phalarope", 
        "Red Phalarope", "Red-necked Phalarope", "Phalarope", "American Wigeon", "Phalarope sp.",
        "Greater White Fronted Goose")

# New guilds -----------------------------------------------------

cruzguilds <- cleanallyears
cruzguilds$guild <- ifelse(cleanallyears$Species %in% d, 'Divers', ifelse(cleanallyears$Species %in% db, 'Dabblers', 
                                                              ifelse(cleanallyears$Species %in% gts, 'Gulls, Terns, and Skimmers',
                                                                     ifelse(cleanallyears$Species %in% llw, 'Long-legged Waders',  ifelse(cleanallyears$Species %in% mb, 'marshbirds',  
                                                                      ifelse(cleanallyears$Species %in% p, 'Pelicans and allies',  ifelse(cleanallyears$Species %in% sb, 'Shorebirds',
                                                                         ifelse(cleanallyears$Species %in% wf, 'waterfoul', 'not in guild'))))))))

# Check guilds
cruzguilds$Count <- as.numeric(cruzguilds$Count)

checkguilds <- group_by(cruzguilds, Species, landbird, guild)%>%
  summarise(sum(Count, na.rm=T))%>%
  filter(landbird== FALSE)

# add migratory column -----------------------------------------------------------------------------

Migratories <- c("Common Loon", "Pacific Loon", "Red-throated Loon", "Western Grebe", "Eared Grebe", "Horned Grebe", "American Avocet",
                 "Black Turnstone", "Black-bellied Plover", "Dunlin", "Greater Yellowlegs", "Least Sandpiper", "Lesser Yellowlegs",
                 "Long-billed Curlew", "Long-billed Dowitcher", "Marbled Godwit", "Red Knot", "Ruddy Turnstone", "Sanderling", "Semipalmated Plover",
                 "Short-billed Dowitcher", "Snowy Plover", "Spotted Sandpiper", "Surfbird", "Wandering Tattler", "Western Sandpiper", "Whimbrel",
                 "Willet", "Wilson's Plover", "American White Pelican", "Little Blue Heron", "White-faced Ibis", "Common Merganser",
                 "Red-breasted Merganser", "Brant", "American Coot", "Green-winged Teal", "Redhead", "Ring-necked Duck", "Snow Goose",
                 "Bufflehead", "Cinnamon Teal", "Greater Scaup", "Lesser Scaup", "Common Goldeneye", "Blue-winged Teal", "Black Tern",
                 "Least Tern", "Elegant Tern", "Forster's Tern", "Common Tern", "Caspian Tern", "Royal Tern", "Bonaparte's Gull",
                 "California Gull", "Franklin's Gull", "Glaucous-winged Gull", "Herring Gull", "Ring-billed Gull", "Sabine's Gull",
                 "Sandhill Crane", "Northern Pintail", "Northern Shoveler", "Wilson's Phalarope", "Red Phalarope", "Red-necked Phalarope")


cruzguilds$Migratory <- ifelse(cruzguilds$Species %in% Migratories, "TRUE", "FALSE")


unique(cruzguilds$DateTime)



#write.csv(cruzguilds,"LaCruz_Guilds_AllYearsAllSites.csv")   
