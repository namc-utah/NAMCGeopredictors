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
### Wyoming model
### Description


### Load required inputs
### Required for WY model are:
# Shapefile of sampling points
# Shapefile of delineated watershed polygons

# Register the points

master.list<-read.csv(here("Vectors","Sites_For_Delineation_2019F.csv"), na.strings = c("","NA"))
master.list<-subset(master.list, Bug.model =="WY")
## Now convert to POINT geometry
Wy.points<- st_as_sf(master.list, coords = c("Long", "Lat"), 
                               crs = 4326,remove = FALSE)

## Read in the points

#Wy.points<-st_read(here("AIM2020/All2020Sheds","All2020Points.shp"))
#head(Wy.points)

# Read in polygons
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


# This model only requires one geospatial predictor == Bioregions
# Read in Bioregions
Wy.bioregions<-st_read(here("WY_Model","BIOREGIONS_2011_modifiedCP.shp"))
st_is_valid(Wy.bioregions) # Check if the geometries are valid, otherwise it will not carry out processes such as intersection
Wy.bioregions<-st_make_valid(Wy.bioregions) # Fix invalid polygon geometries


## Let's work using the geospatial information (PROJECTION) from the HUCL4 as usual
## Let's first read the HUCs shapefile
HUCs<- st_read(here("Vectors","HUC4_All.shp"))
## And now extract its crs or projection info
crs2use<-crs(HUCs)

## And reproject the points and polygons
Wy.points.albers<-st_transform(Wy.points, crs2use)
Wy.polys.albers<-st_transform(Wsheds.att, crs2use)
#Wy.polys.albers<-st_transform(Wy.polys, crs2use)

## Intersect the points and the bioregions - This will clip the points to the WY bioregions spatial domain
Wy.points.albers2<-st_intersection(Wy.points.albers, Wy.bioregions)

## Add required fields for Dummy Variables and populate them with zeroes
Wy.points.albers2$SR_BIGHORNS<-0
Wy.points.albers2$MRE<-0
Wy.points.albers2$HV_UPPERPLATTE<-0
Wy.points.albers2$SFLR<-0

## Reclassify the bioregions into dummy variables

# Recoding Southern Rockies and Bighorn Mountains
Wy.points.albers2$SR_BIGHORNS[Wy.points.albers2$LAST_COUNT == "SOUTHERN ROCKIES"|
                                Wy.points.albers2$LAST_COUNT == "BIGHORN BASIN FOOTHILLS"|
                                Wy.points.albers2$LAST_COUNT == "WB - BIGHORN BASIN"]<-1
# Recoding Black hills
Wy.points.albers2$MRE[Wy.points.albers2$LAST_COUNT == "BLACK HILLS"]<-1
# Recoding High Valleys and Upper North Platte 
Wy.points.albers2$HV_UPPERPLATTE[Wy.points.albers2$LAST_COUNT == "HIGH VALLEYS"]<-1
# Recoding Southern Foothills and Laramie Range
Wy.points.albers2$SFLR[Wy.points.albers2$LAST_COUNT == "S WY FH & LARAMIE RANGE"]<-1


### Extract the watershed area in SqrdKm
### Leave only the watersheds that match the selected points

### Leave only the attributes of interests of the points: SiteCode and the bioregion dummy variables
Wy.points.albers3<-Wy.points.albers2[,c(6,11,12,15,19:22)]

## Now bring the point attributes to the polygons
# First calculate the area in sqkm2
Wy.polys.albers$WsAreaSqKm<-st_area(Wy.polys.albers)/1000000
Wy.polys.albers<-drop_units(Wy.polys.albers) # remove the square area units


# Extract only the polygons that are Bug Model = Wyoming
#Wy.polys.albers2<-subset(Wy.polys.albers, Bug_model == "WY")
# Create a dataframe with all available attributes
Wy.polys.albers3<-inner_join(st_drop_geometry(Wy.polys.albers), st_drop_geometry(Wy.points.albers3), by="SiteCode")

## Get the Log10 of Area
Wy.polys.albers3$Log10_WatershedArea<-log10(Wy.polys.albers3$WsAreaSqKm)

### Format the output *.csv

Df2export<-Wy.polys.albers3[,c(4,3,1,5,7,8,6,9:14)]
setnames(Df2export, old = c('Lat','Long','LAST_COUNT','Log10_WatershedArea'), new = c('LAT','LONG','BIOREGIONS_2011_modifiedCP','W_LOG'))

## EXPORT
write.csv(Df2export,here("AIM2019_WY_Points_WYmodelpredictors.csv"))

