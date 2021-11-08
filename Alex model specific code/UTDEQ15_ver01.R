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

## UTDEQ15 model

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

master.list<-read.csv("/Users/alexhernandez/Desktop/BUG_BLM/ZonalTest/Vectors/UTRefPolys/Priority/AIMUT2020Priority.csv",na.strings = c("","NA"))

# Register the watersheds that are ready


Wsheds.raw<-st_read("/Users/alexhernandez/Desktop/BUG_BLM/ZonalTest/Vectors/UTRefPolys/Priority/GlobalWatershedUTPriority.shp")
Wpoints.raw<-st_read("/Users/alexhernandez/Desktop/BUG_BLM/ZonalTest/Vectors/UTRefPolys/Priority/GlobalWatershedPointUTPriority1.shp")
## Wsheds should be transformed
Wsheds.albers<-st_transform(Wsheds.raw, crs = crs2use)

## Keeping "Name" as the unique identifier which will match SITECODE
Wsheds.albers<-Wsheds.albers[,c(3)]


### Define the inputs that will be used for point extractions and zonal statistics
## Reference for models is: //Zonal environmental statistics/AllPredictiveModelsCurrentlyRun.xlsx


### Climate
TMAX_AVE.ras<-raster(here("GIS_Stats01/Climate/Data/tmax_usgs","w001001.adf"))
TMEAN_AVE.ras<-raster(here("GIS_Stats01/Climate/Data/tmean_usgsut","w001001.adf"))
TMIN_AVE.ras<-raster(here("GIS_Stats01/Climate/Data/tmin_usgs","w001001.adf"))
RH_AVE.ras<-raster(here("GIS_Stats01/Climate/Data/rhmean_usgs","w001001.adf"))
MEANP_AVE.ras<-raster(here("GIS_Stats01/Climate/Data/meanp_usgs","w001001.adf"))
MAXP_AVE.ras<-raster(here("GIS_Stats01/Climate/Data/pmax_usgs","w001001.adf"))
MAXWD_AVE.ras<-raster(here("GIS_Stats01/Climate/Data/Wdmax_usgs","w001001.adf"))
FST32F_AVE.ras<-raster(here("GIS_Stats01/Climate/Data/fstfrz_usgs","w001001.adf"))


EVI_MAX_AVE.ras<-raster(here("GIS_Stats01/Vegetation/Data/evi_max_10b.tif"))


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
zones.albers.points2<-zones.albers.points # Making a copy of the object
#master.list.UIDLAT<-master.list[,c(8,10)] # create an object that brings back UID and Lat so that it can be merged with albers projection object
#zones.albers.points2<-merge(zones.albers.points2, master.list.UIDLAT, by="UID") # merge the objects so that we can have access to Lat
# Now let's extract the values at the points
#ptm <- proc.time()
zones.albers.points2$TMAXPT<-terra::extract(TMAX_AVE.ras,zones.albers.points2)
zones.albers.points2$TMEANPT<-terra::extract(TMEAN_AVE.ras,zones.albers.points2)
zones.albers.points2$DD_LAT_Y<-zones.albers.points2$Lat

## Now let's work with the polygon variables

# Keep only the variables that pertain to the UTDEQ15 Model

dropvar <- c("TMAX_AVE","TMEAN_AVE","TMIN_AVE","RH_AVE","MEANP_AVE","MAXP_AVE",
             "MAXWD_AVE","FST32F_AVE","EVI_MAX_AVE")

#ECzones3<- ECzones[,(names(ECzones) %in% dropvar)]

### Areal zonal stats /// Try a loop to obtain all variables at the polygon level

zonalvars<-dropvar[c(1:9)]
for (var in zonalvars){
  #newvar<-paste0(var,"_alex")
  #newvar<-var
  gisvar<-eval(parse(text = paste0(var,".ras")))# This makes sure that the character of the variable name can be recognized as a variable name in exact_extract
  print(paste("processing",var))
  Wsheds.albers[[var]]<-exact_extract(x=gisvar,y=Wsheds.albers,'mean')

}

## Now add GEE variables
Wsheds.albers$ELEV_MEAN<-NA
Wsheds.albers$ELEV_MIN<-NA
#Wsheds.att.OLD$PPT_ACCUM<-NA
ptm <- proc.time()
for (i in 1:nrow(Wsheds.albers)){
  tryCatch({ #if an error is found then it is printed, but the loop does not break and continues with the next iteration
    objecto<-Wsheds.albers[i,] # Take the first feature
    pcpacum.extraction<-ee_extract(USGS_NED, objecto, fun = ee$Reducer$min(), scale=30)%>% as_tibble()
    print(pcpacum.extraction)
    pcpacum.extraction<-pull(pcpacum.extraction)
    Wsheds.albers[[13]][i]<-pcpacum.extraction
  },error=function(e){cat("ERROR :",conditionMessage(e), "\n")})  
}
proc.time() - ptm

sum(is.na(Wsheds.albers$ELEV_MIN_alex))

## Finally extract the area in SQ KM
Wsheds.albers$SQ_KM<-drop_units(st_area(Wsheds.albers)/1000000)

### Putting things together

## Bringing everything together

#master.deliver<-master.list[,c(1:15)]
# Drop the geometries to merge the two (point and polygon) dataframe

points<-st_set_geometry(zones.albers.points2, NULL) # remove geometry, coerce to data.frame
polys<-st_set_geometry(Wsheds.albers, NULL) # remove geometry, coerce to data.frame

# Bring the point-based predictors
master.deliver01<-inner_join(points, polys, by=c("reachid"="Name"))

# Write the CSV

write.csv(master.deliver01, "/Users/alexhernandez/Desktop/BUG_BLM/ZonalTest/Vectors/UTRefPolys/Priority/AIMUT2020Priority_Predictors.csv")










## Preparations for correlation plots

dropvar <- c("TMAXPT","TMEANPT","TMAX_AVE", "TMEAN_AVE","TMIN_AVE", "RH_AVE","MEANP_AVE",
            "MAXP_AVE","MAXWD_AVE","FST32F_AVE", "EVI_MAX_AV","ELEV_MIN", "ELEV_MEAN","SQ_KM")

names(master.deliver01)[27]<- "EVI_MAX_AV_alex"

master.deliver01$diff<-master.deliver01$SQ_KM- master.deliver01$SQ_KM_alex

master.deliver02<-subset(master.deliver01, diff > -2 & diff < 2)

### Comparison plots

for (varplot in dropvar){
  scatter<-paste0(varplot,"_compare")
  scatter<-ggscatter(master.deliver02, x = varplot, y = paste0(varplot,"_alex"), 
                     add = "reg.line", conf.int = TRUE, 
                     cor.coef = TRUE, cor.method = "pearson",
                     xlab = "Existing", ylab = "Alex", title=varplot)
  ggexport(scatter,filename = here("UtahDEQ_plots2",paste0(varplot,"_Refs.pdf")))
}


