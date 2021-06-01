library(sf)
library(dplyr)
#setwd("/Users/alexhernandez/Desktop/BUG_BLM/Temps/Alrus")
library(here)
set_here("/Users/alexhernandez/Desktop/BUG_BLM/ZonalTest/GIS_Stats01")
library(mapview)


### Read in CO features and just keeping identifiers and sample date
CO.Wats<-st_read(here("CO/COEDAS/MineralCrSheds.shp"))
CO.Points<-st_read(here("CO/COEDAS/MineralCrPts.shp"))
myvars<-c("sampleid","sampdate")
CO.Wats<-CO.Wats[myvars]
CO.Points<-CO.Points[myvars]
#######

