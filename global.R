### File: global.R
###
### Template Created: 14/07/2020	08:55:00
### Author: Guillermo Martin
###
####################################################################################################
### Description:
###
### Global functions to load packages and Inshore database data
###   
###

# Used packages
packs = c("shiny",
          "shinyBS",
          "shinydashboard", 
          "shinythemes",
          "shinyjs",
          "shinyWidgets",
          "leaflet",
          "dplyr",
          "tidyr",
          "ggplot2",
          "lubridate", 
          "plotly",
          "sp",
          "sf",
          #"rgdal",
          "shinycssloaders",
          "ggsci",
          "ggthemes",
          "paletteer",
          #"colorRamps",
          "ggrepel",
          "rintrojs",
          "fontawesome")

library(shiny)
library(shinyBS)
library(shinydashboard )
library(shinythemes)
library(shinyjs)
library(shinyWidgets)
library(leaflet)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate )
library(plotly)
library(sp)
library(sf)
library(shinycssloaders)
library(ggsci)
library(ggthemes)
library(paletteer)
library(ggrepel)
library(rintrojs)
library(fontawesome)
    


# Run the following command to verify that the required packages are installed. If some package
# is missing, it will be installed automatically
#package.check <- lapply(packs, FUN = function(x) {
#  if (!require(x, character.only = TRUE)) {
#    install.packages(x, dependencies = TRUE)
#  }
#})
#library(shiny)

# Intro steps
intro<-read.csv("./data/intro.csv")

##Loading Inshore fisheries data:
catch_plot<-read.csv("./data/Data_prep_Output/catch_plot.csv")

#Biological data
bio<-read.csv("data/Data_prep_Output/bio_summary.csv")

#Summary statistics
dat_sta<-read.csv("data/Data_prep_Output/Programme_summary.csv")
minY<-as.numeric(min(dat_sta$Year))
maxY<-as.numeric(max(dat_sta$Year))

#Landings table
landings<-read.csv(file.path("data/Data_prep_Output/",
                             "Landings_Table.csv"))
maxY_landings <- as.numeric(substr(colnames(landings)[length(colnames(landings))], 2, 5))
landings <- landings %>%
  pivot_longer(cols = starts_with("X"),
               names_to = "Year",
               values_to = "Landings") %>%
  mutate(Year=gsub("x","",Year,ignore.case = T)) %>%
  filter(Year <= maxY_landings) %>%
  data.frame()

#Adding Scientific Name 
landings$ScientificName<-NA
landings$ScientificName[landings$SpeciesName == "Cockle"]<-"(Cerastoderma edule)"
landings$ScientificName[landings$SpeciesName == "Crayfish"]<-"(Palinurus elephas)"
landings$ScientificName[landings$SpeciesName == "Edible crab"]<-"(Cancer pagurus)"
landings$ScientificName[landings$SpeciesName == "King Scallop"]<-"(Pecten maximus)"
landings$ScientificName[landings$SpeciesName == "Lobster"]<-"(Homarus gammarus)"
landings$ScientificName[landings$SpeciesName == "Native oyster"]<-"(Ostrea edulis)"
landings$ScientificName[landings$SpeciesName == "Queen scallop"]<-"(Aequipecten opercularis)"
landings$ScientificName[landings$SpeciesName == "Razor clams"]<-"(Ensis sp.)"
landings$ScientificName[landings$SpeciesName == "Shore crab"]<-"(Carcinus sp.)"
landings$ScientificName[landings$SpeciesName == "Shrimp"]<-"(Palaemon sp./Crangon sp.)"
landings$ScientificName[landings$SpeciesName == "Spider crab"]<-"(Maja squinado)"
landings$ScientificName[landings$SpeciesName == "Surf clam"]<-"(Spisula sp.)"
landings$ScientificName[landings$SpeciesName == "Velvet crab"]<-"(Necora puber)"
landings$ScientificName[landings$SpeciesName == "Whelk"]<-"(Buccinum undatum)"

landings$SS<-paste(landings$SpeciesName,landings$ScientificName,sep=" ")

#Assessment and advice table

# Bivalves:
ba_a<-read.csv("data/Data_prep_Output/Bivalves Assessment and Advice.csv")
ba_a<-ba_a[order(ba_a$Specie,ba_a$Area,-ba_a$Year),]

#Crustaceans:
ca_a<-read.csv("data/Data_prep_Output/Crustacean Assessment and Advice.csv")

#Loading Spatial Data
source("./lib/01_Loading_Spatial_Data.R",
       encoding="latin1")

#Additional functions and Shinny formatting
source("./lib/help_funs.R",
       encoding="latin1")
source("./lib/override.R", 
       local = TRUE)

#Load data at ICES_rectangle
ICES_LPUE<-st_read(dsn = "data/Data_prep_Output",
                   layer = "ICES_LPUE")
                   #encoding = "utf8")
#ICES_LPUE <- spTransform(ICES_LPUE, CRSobj=projWGS84)
ICES_LPUE <- st_transform(ICES_LPUE, CRSobj=projWGS84)
#ICES_LPUE<-st_as_sf(ICES_LPUE)


#Read rds file related to the polygons
ICES_dat<-readRDS(file=file.path("data/Data_prep_Output/ICES_slope_data.rds"))
ICES_dat<-subset(ICES_dat,Year <= maxY)
ICES_dat <- ICES_dat %>%
  group_by(Year) %>%
  arrange(EventStartDate) %>%
  ungroup() %>%
  mutate(units=ifelse(CommonName %in% "EDIBLE CRAB UNSEXED","kg/Pot","Number/100 Pots")) %>%
  data.frame()
