## Load useful packages
library(sf)
library(raster)
library(data.table)
setwd("/Users/alexhernandez/Desktop/BUG_BLM/ZonalTest")
library(here)
library(ggpubr)
library(mapview)
library(prism)
library(exactextractr)
library(mapedit)
library(reticulate)
library(rgee)
library(tidyverse)
library(survival)
library(dplyr)
library(nhdplusTools)
library(lubridate)
library(units)

ee_Initialize()

## PIBO Idaho model

### Required for PIBO model are:
# Shapefile of sampling points
# Shapefile of delineated watershed polygons


## Read in the points for 2019 AIM
# Register the points

master.list<-read.csv(here("Vectors","Sites_For_Delineation_2019F.csv"), na.strings = c("","NA"))
master.list<-subset(master.list, Bug.model =="PIBO")
## Now convert to POINT geometry
Wy.points<- st_as_sf(master.list, coords = c("Long", "Lat"), 
                     crs = 4326,remove = FALSE)

head(Wy.points)
# Register the watersheds that are ready

Wsheds.raw<-st_read(here("/Users/alexhernandez/Desktop/BUG_BLM/AIM2019/Wsheds_Total_before07_Merge.shp"))
Wsheds.raw<-Wsheds.raw[,c(1,3)]
### Divide the Watersheds into Two 2 sets based on OLDUID or Current UID

## !!!!!! CREATE TWO POLYGON DATASETS BASED ON UID OR OLDUID
## Now bring the Julian Date to the polygons 
Wsheds.att.OLD<-merge(Wsheds.raw, master.list, by.x ="OLD_UIDT", by.y="OLD_UID")
Wsheds.att.CUR<-merge(Wsheds.raw, master.list, by.x ="OLD_UIDT", by.y="UID")

## !!! Must leave only attributes of interest here experiencing some issues, is it because of the fields included?
Wsheds.att.CUR<-Wsheds.att.CUR[,c(1,3,5,7,10,14)]
Wsheds.att.OLD<-Wsheds.att.OLD[,c(9,3,1,6,10,14)]
setnames(Wsheds.att.CUR, old = c('OLD_UIDT'), new = c('UID'))
setnames(Wsheds.att.OLD, old = c('OLD_UIDT'), new = c('OLD_UID'))
# Merge the two datasets so that we have one unique set
Wsheds.att<-rbind(Wsheds.att.CUR, Wsheds.att.OLD)

## Let's work using the geospatial information (PROJECTION) from the HUCL4 as usual
## Let's first read the HUCs shapefile
HUCs<- st_read(here("Vectors","HUC4_All.shp"))
## And now extract its crs or projection info
crs2use<-crs(HUCs)

## And reproject the points and polygons
Wy.points.albers<-st_transform(Wy.points, crs2use)
Wy.polys.albers<-st_transform(Wsheds.att, crs2use)
#Wy.polys.albers<-st_transform(Wy.polys, crs2use)

# Extract only the points and polygons that are Bug Model = PIBO
Wy.points.albers2<-subset(Wy.points.albers, Bug.model == "PIBO")
Wy.polys.albers2<-subset(Wy.polys.albers, Bug.model == "PIBO")

#### Define predictors

TMAX_PT.ras<-raster(here("GIS_Stats01/Climate/Data/tmax_usgs","w001001.adf"))

LOG_LT_PPT_PT.ras<-raster(here("GIS_Stats01/Climate/Data/meanppt_pibo.tif"))

### Extract values at the points
ptm <- proc.time()
Wy.points.albers2$TMAX_PT<-terra::extract(TMAX_PT.ras,Wy.points.albers2)
Wy.points.albers2$LOG_LT_PPT_PT<-log10(terra::extract(LOG_LT_PPT_PT.ras,Wy.points.albers2))
proc.time() - ptm

## Now bring the point attributes to the polygons
# First calculate the area in sqkm2
Wy.polys.albers2$WsAreaSqKm<-st_area(Wy.polys.albers2)/1000000
Wy.polys.albers2<-drop_units(Wy.polys.albers2) # remove the square area units
Wy.polys.albers3<-Wy.polys.albers2[,c(4,7)]

# Create a dataframe with all available attributes
Wy.points.albers3<-inner_join(st_drop_geometry(Wy.points.albers2), st_drop_geometry(Wy.polys.albers3), by="SiteCode")

Wy.points.albers3$LOG_KM2<-log10(Wy.points.albers3$WsAreaSqKm)

### Format the output *.csv

Df2export<-Wy.points.albers3[,c(4,6,9:12,14:17)]
#setnames(Df2export, old = c('Lat','Long','LAST_COUNT','Log10_WatershedArea'), new = c('LAT','LONG','BIOREGIONS_2011_modifiedCP','W_LOG'))

## EXPORT
write.csv(Df2export,here("AIM2019_PIBO_Points_PIBOmodelpredictors.csv"))







