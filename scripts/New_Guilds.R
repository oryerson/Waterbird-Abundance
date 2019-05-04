# New Guilds
#
# Author: Owen Ryerson
# Date created: 27 Apr 2019
# Date last modified: 27 Apr 2019
#
rm(list=ls())

library(ggplot2)
library(dplyr)

options(na.action="na.fail")

### Load and pepare data ###
if(Sys.info()[6]!="abramfleishman"){
  # set your paths here Owen
  cruz<-read_csv("/Users/owenryerson/Google Drive/Birds!/R data Processing/CSV/LaCruz/LaCruz_Guilds.csv")
  tast<-read_csv("/Users/owenryerson/Google Drive/Birds!/R data Processing/CSV/Tastiota/Tastiota_guilds.csv")
  card<-read_csv("/Users/owenryerson/Google Drive/Birds!/R data Processing/CSV/Cardonal/Cardonal_Guilds.csv")
}else{
  # paths for abram
  cruz<-read_csv("/Users/abramfleishman/google_drive/R data Processing/CSV/LaCruz/LaCruz_Guilds.csv")
  tast<-read_csv("/Users/abramfleishman/google_drive/R data Processing/CSV/Tastiota/Tastiota_guilds.csv")
  card<-read_csv("/Users/abramfleishman/google_drive/R data Processing/CSV/Cardonal/Cardonal_Guilds.csv")
  out_dir<-'/Users/abramfleishman/google_drive/R data Processing/'
}


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

d <- c("Common Loon", "Loon sp.", "Pacific Loon",
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
        "Merganser Species")


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


#Landbirds

lb <- c("Belted Kingfisher", "Osprey")

#Dabbles

db <- c("Northern Pintail", "Northern Shoveler", "Wilson's Phalarope", 
        "Red Phalarope", "Red-necked Phalarope", "Phalarope", "American Wigeon", "Phalarope sp.")



# New guilds -----------------------------------------------------

cardnew <- card
cardnew$guild <- ifelse(card$Species %in% d, 'Divers', ifelse(card$Species %in% db, 'Dabblers', 
               ifelse(card$Species %in% gts, 'Gulls, Terns, and Skimmers',  ifelse(card$Species %in% lb, 'Landbirds',
               ifelse(card$Species %in% llw, 'Long-legged Waders',  ifelse(card$Species %in% mb, 'marshbirds',  
               ifelse(card$Species %in% p, 'Pelicans and allies',  ifelse(card$Species %in% sb, 'Shorebirds',
               ifelse(card$Species %in% wf, 'waterfoul', 'Issue')))))))))

tastnew <- tast
tastnew$guild <- ifelse(tast$Species %in% d, 'Divers', ifelse(tast$Species %in% db, 'Dabblers', 
               ifelse(tast$Species %in% gts, 'Gulls, Terns, and Skimmers',  ifelse(tast$Species %in% lb, 'Landbirds',
               ifelse(tast$Species %in% llw, 'Long-legged Waders',  ifelse(tast$Species %in% mb, 'marshbirds',  
               ifelse(tast$Species %in% p, 'Pelicans and allies',  ifelse(tast$Species %in% sb, 'Shorebirds',
               ifelse(tast$Species %in% wf, 'waterfoul', 'Issue')))))))))

cruznew <- cruz
cruznew$guild <- ifelse(cruz$Species %in% d, 'Divers', ifelse(cruz$Species %in% db, 'Dabblers', 
               ifelse(cruz$Species %in% gts, 'Gulls, Terns, and Skimmers',  ifelse(cruz$Species %in% lb, 'Landbirds',
               ifelse(cruz$Species %in% llw, 'Long-legged Waders',  ifelse(cruz$Species %in% mb, 'marshbirds',  
               ifelse(cruz$Species %in% p, 'Pelicans and allies',  ifelse(cruz$Species %in% sb, 'Shorebirds',
               ifelse(cruz$Species %in% wf, 'waterfoul', 'Issue')))))))))


#write.csv(cardnew, file = "Cardonal_New_Guilds.csv")
#write.csv(tastnew, file = "Tastiota_New_Guilds.csv")
#write.csv(cruznew, file = "LaCruz_New_Guilds.csv")

