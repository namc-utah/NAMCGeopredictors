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

## NVMMI model

# The model requires a suite of predictors. In order to extract the appropriate metric a particular
# geospatial operation is required. The script is prepared based on inputs and required
# operation

## Work with watersheds - polygons that require mean statistics for the entire area or zone

## Zonal statistics require that both rasters: the one being used to extract statistics from
## and the ones for which statistics will be calculated to have the same extent, pixel size,
## and origin

## Define the projection to work throughout the project
# Load the HUCs - this shapefile is a good place to start for projection references
HUCs<- st_read(here("Vectors","HUC4_All.shp"))
## And now extract its crs
crs2use<-crs(HUCs)

#### This version of the model focuses on a few points that are considered priority for the State of Utah
#### The version that will be used for all the points to be processed should be fairly similar to this version

### Define the inputs that will be used for point extractions and zonal statistics
## Reference for models is: //Zonal environmental statistics/AllPredictiveModelsCurrentlyRun.xlsx
# Register the points

master.list<-read.csv("/Users/alexhernandez/Desktop/BUG_BLM/ZonalTest/Vectors/NV/AIM_NV_2020_priority.csv",na.strings = c("","NA"))
## The Lat Long are in a separate file
#NV.locations<-read.csv("/Users/alexhernandez/Desktop/BUG_BLM/ZonalTest/Vectors/NV/NV_MMI_CalSites.csv",na.strings = c("","NA"))
  
#master.list<-master.list[,c(2,8,10,11,7,15,9,16,6,13,12,5,21,4,3)]  # Keeping only the predictors included in the actual model

#master.list<-inner_join(master.list, NV.locations, by=c("Sitecode"="Station"))

# Register the watersheds that are ready - Prepared by Kristin

Wsheds.raw<-st_read("/Users/alexhernandez/Desktop/BUG_BLM/ZonalTest/Vectors/NV/GlobalWatershed.shp")
## Wsheds should be transformed
Wsheds.albers<-st_transform(Wsheds.raw, crs = crs2use)

## Keeping "Name" as the unique identifier 
Wsheds.albers<-Wsheds.albers[,c(3,14)] # Here we are keeping the Name and FlowLength from StreamStats as field to match existing predictors

Wsheds.albers<-drop_na(Wsheds.albers) # We're dropping all NA's from the ArcMap-derived Flow length

# Now bring in the original predictors
#Wsheds.albers<-inner_join(Wsheds.albers,master.list, by=c("Name"="Sitecode"))
#Wsheds.albers<-Wsheds.albers[,c(28,48,95,101,103,99,49,100,105,96,97,80,90,92,35,91,107)]


### Define the inputs that will be used for point extractions and zonal statistics
## Reference for models is: //Zonal environmental statistics/AllPredictiveModelsCurrentlyRun.xlsx


### Climate
WDmax_WS.ras<-raster(here("GIS_Stats01/Climate/Data/wdmax_usgs","w001001.adf"))
Pmin_WS.ras<-raster(here("GIS_Stats01/Climate/Data/pmin_usgs","w001001.adf"))
Tmax_WS.ras<-raster(here("GIS_Stats01/Climate/Data/tmax_usgs","w001001.adf"))
HYDR_WS.ras<-raster(here("GIS_Stats01/Climate/Data/hydr_all","w001001.adf"))
Pmax_WS.ras<-raster(here("GIS_Stats01/Climate/Data/pmax_usgs","w001001.adf"))

# Metrics
#ELVcv_PT.ras<-st_read(here("GIS_Stats01/Metrics/Colorado/Data","topocv.shp"))
BFI_WS.ras<-raster(here("GIS_Stats01/Metrics/Data/bfi_usgs.tif"))


### GEE variables
### Extract elevation using Earth Engine
USGS_NED<-ee$Image("USGS/NED")$select("elevation")


#### Geoprocessing area

### Extract the values at the point

# first we utilize the point locations for the first variables::: TMAX_PT, TMEANPT, 

## Now convert to POINT geometry
zones.points.latlon<- st_as_sf(master.list, coords = c("Long", "Lat"), 
                               crs = 4326, remove=FALSE)
# Transform to the CRS of interest
zones.albers.points<-st_transform(zones.points.latlon,crs=crs2use)

# Now let's extract some values
# First let's minimize the size of the object for quicker comparisons
zones.albers.points2<-zones.albers.points # Making a copy of the object in order to keep ALL attributes back to TRIP
#master.list.UIDLAT<-master.list[,c(8,10)] # create an object that brings back UID and Lat so that it can be merged with albers projection object
#zones.albers.points2<-merge(zones.albers.points2, master.list.UIDLAT, by="UID") # merge the objects so that we can have access to Lat
# Now let's extract the values at the points
#ptm <- proc.time()
zones.albers.points2$Pmax_PT<-terra::extract(Pmax_WS.ras,zones.albers.points2)
zones.albers.points2$Tmax_PT<-terra::extract(Tmax_WS.ras,zones.albers.points2)

# Create a buffer to extract CV around points
zones.albers.points2.buff<-st_buffer(zones.albers.points2,150)
zones.albers.points2.buff$mean<-pull(ee_extract(USGS_NED, zones.albers.points2.buff, fun = ee$Reducer$mean(), scale=30)%>% as_tibble())
zones.albers.points2.buff$stdev<-pull(ee_extract(USGS_NED, zones.albers.points2.buff, fun = ee$Reducer$stdDev(), scale=30)%>% as_tibble())
zones.albers.points2.buff$ELVcv_PT<-zones.albers.points2.buff$stdev/zones.albers.points2.buff$mean

## Now let's work with the polygon variables

# Keep only the variables that pertain to the NVMMI Model

dropvar <- c("WDmax_WS","Pmin_WS","Tmax_WS","HYDR_WS","Pmax_WS","BFI_WS")

#ECzones3<- ECzones[,(names(ECzones) %in% dropvar)]

### Areal zonal stats /// Try a loop to obtain all variables at the polygon level

zonalvars<-dropvar[c(1:6)]
for (var in zonalvars){
  #newvar<-paste0(var,"_alex")
  newvar<-var
  gisvar<-eval(parse(text = paste0(var,".ras")))# This makes sure that the character of the variable name can be recognized as a variable name in exact_extract
  print(paste("processing",var))
  Wsheds.albers[[newvar]]<-exact_extract(x=gisvar,y=Wsheds.albers,'mean')

}

## Now add GEE variables

Wsheds.albers$ELVmax_WS<-NA
#Wsheds.att.OLD$PPT_ACCUM<-NA
ptm <- proc.time()
for (i in 1:nrow(Wsheds.albers)){
  tryCatch({ #if an error is found then it is printed, but the loop does not break and continues with the next iteration
    objecto<-Wsheds.albers[i,] # Take the first feature
    pcpacum.extraction<-ee_extract(USGS_NED, objecto, fun = ee$Reducer$max(), scale=30)%>% as_tibble()
    print(pcpacum.extraction)
    pcpacum.extraction<-pull(pcpacum.extraction)
    Wsheds.albers[[12]][i]<-pcpacum.extraction
  },error=function(e){cat("ERROR :",conditionMessage(e), "\n")})  
}
proc.time() - ptm

## Now compute Slope_WS based on ArcMap
#Wsheds.albers$Slope_WS_Arcmap<-(Wsheds.albers$ELVmax_WS_alex - Wsheds.albers$ELVmin_WS_alex)/Wsheds.albers$AMFloLen_m

## Now compute Slope_WS based on StreamStats
Wsheds.albers$Slope_WS<-(Wsheds.albers$ELVmax_WS - Wsheds.albers$ELVmin_WS)/Wsheds.albers$SSFloLen_m


## Extract the area in SQ KM
Wsheds.albers$SQ_KM<-drop_units(st_area(Wsheds.albers)/1000000)

# Finally divide the Tmax variables by 10

Wsheds.albers$Tmax_WS<-Wsheds.albers$Tmax_WS/10
zones.albers.points2$Tmax_PT<-zones.albers.points2$Tmax_PT/10
zones.albers.points2.buff$Tmax_PT<-zones.albers.points2.buff$Tmax_PT/10
### Putting things together

## Bringing everything together

#master.deliver<-master.list[,c(1:15)]
# Drop the geometries to merge the two (point and polygon) dataframe

points<-st_set_geometry(zones.albers.points2, NULL) # remove geometry, coerce to data.frame
buffer<-st_set_geometry(zones.albers.points2.buff, NULL) # remove geometry, coerce to data.frame
buffer<-buffer[,c(1:14,17)]
points<-inner_join(points, buffer, by="Sitecode")
polys<-st_set_geometry(Wsheds.albers, NULL) # remove geometry, coerce to data.frame

# Bring the point-based predictors
master.deliver01<-inner_join(buffer, polys, by=c("reachid"="Name"))


# Write the CSV

write.csv(master.deliver01, "/Users/alexhernandez/Desktop/BUG_BLM/ZonalTest/Vectors/NV/AIM_NV_2020Priority_Predictors.csv")


