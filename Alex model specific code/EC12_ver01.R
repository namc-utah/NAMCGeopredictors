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

## TNTP model

# The model requires a suite of predictors. In order to extract the appropriate metric a particular
# geospatial operation is required. The script is prepared based on inputs and required
# operation

## Work with watersheds - polygons that require mean statistics for the entire area or zone

####### Define inputs

## Define the projection to work throughout the project
# Load the HUCs - this shapefile is a good place to start for projection references
HUCs<- st_read(here("Vectors","HUC4_All.shp"))
## And now extract its crs
crs2use<-crs(HUCs)

### Define the inputs that will be used for point extractions and zonal statistics
## Reference for models is: //Zonal environmental statistics/AllPredictiveModelsCurrentlyRun.xlsx
# Register the points

master.list<-read.csv(here("Vectors","Sites_For_Delineation_2019F.csv"), na.strings = c("","NA"))

# Register the watersheds that are ready

Wsheds.raw<-st_read(here("Vectors","Wsheds_Total_before07_merge.shp"))
Wsheds.raw<-Wsheds.raw[,c(1,3)]
### Divide the Watersheds into Two 2 sets based on OLDUID or Current UID
### first let's convert to a proper date format --- the sampling date
master.list$date<- as.Date(master.list$SampleDate, "%m/%d/%y")
## Then convert to Julian format
master.list$julian <- yday(master.list$date)

## !!!!!! CREATE TWO POLYGON DATASETS BASED ON UID OR OLDUID
## Now bring the Julian Date to the polygons 
Wsheds.att.OLD<-merge(Wsheds.raw, master.list, by.x ="OLD_UIDT", by.y="OLD_UID")
Wsheds.att.CUR<-merge(Wsheds.raw, master.list, by.x ="OLD_UIDT", by.y="UID")

## !!! Must leave only attributes of interest here experiencing some issues, is it because of the fields included?
Wsheds.att.CUR<-Wsheds.att.CUR[,c(1,11,12,15,16)]
Wsheds.att.OLD<-Wsheds.att.OLD[,c(1,11,12,15,16)]

# Merge the two datasets so that we have one unique set
Wsheds.att<-rbind(Wsheds.att.CUR, Wsheds.att.OLD)
### Define the inputs that will be used for point extractions and zonal statistics
## Reference for models is: //Zonal environmental statistics/AllPredictiveModelsCurrentlyRun.xlsx



## Atmosphere !!!
AtmCa.ras<-raster(here("GIS_Stats01/Atmos/Data/atm_ca","w001001.adf"))
AtmSO4.ras<-raster(here("GIS_Stats01/Atmos/Data/atm_so4","w001001.adf"))
AtmMg.ras<-raster(here("GIS_Stats01/Atmos/Data/atm_mg2.tif"))

## Climate !!!
LST32AVE.ras<-raster(here("GIS_Stats01/Climate/Data/lstfrz_usgs","w001001.adf"))
MINWD_WS.ras<-raster(here("GIS_Stats01/Climate/Data/Wdmin_usgs","w001001.adf"))
MEANP_WS.ras<-raster(here("GIS_Stats01/Climate/Aug28/Clima/meanp_usgs1","w001001.adf"))
XWD_WS.ras<-raster(here("GIS_Stats01/Climate/Data/xwd_usgs","w001001.adf"))
SumAve_P.ras<-raster(here("GIS_Stats01/Climate/Data/sumave_p2.tif"))
MAXWD_WS.ras<-raster(here("GIS_Stats01/Climate/Data/Wdmax_usgs","w001001.adf"))
MINP_WS.ras<-raster(here("GIS_Stats01/Climate/Aug28/Clima/pmin_usgs1","w001001.adf"))
TMAX_WS.ras<-raster(here("GIS_Stats01/Climate/Data/tmax_usgs","w001001.adf"))


## Geology
LPREM_mean.ras<-raster(here("GIS_Stats01/Geology/Data/lperm_2feb10","w001001.adf"))

## Landcover
EVI_MaxAve.ras<-raster(here("GIS_Stats01/Vegetation/Data/evi_max_10B.tif"))

## Metrics
PRMH_AVE.ras<-raster(here("GIS_Stats01/Soils/Data/permh_usgs","w001001.adf"))
CaO_Mean.ras<-raster(here("GIS_Stats01/Geology/Data/cao_19jan10","w001001.adf"))
MgO_Mean.ras<-raster(here("GIS_Stats01/Geology/Data/Mgo_19jan10","w001001.adf"))
S_Mean.ras<-raster(here("GIS_Stats01/Geology/Aug28/Data/s_23aug10","w001001.adf"))
UCS_Mean.ras<-raster(here("GIS_Stats01/Geology/Data/ucs_19jan10","w001001.adf"))
BDH_AVE.ras<-raster(here("GIS_Stats01/Soils/Data/bdh_usgs","w001001.adf"))
KFCT_AVE.ras<-raster(here("GIS_Stats01/Soils/Data/kfact_usgs","w001001.adf"))


#### Geoprocessing area

### Extract the values at the point

# first we utilize the point locations for the first variables::: AtmCa, AtmSO4, AtmNa, AtmNO3, PT_Tmin

## Now convert to POINT geometry
zones.points.latlon<- st_as_sf(master.list, coords = c("Long", "Lat"), 
                               crs = 4326)
# Transform to the CRS of interest
zones.albers.points<-st_transform(zones.points.latlon,crs=crs2use)

# Now let's extract some values
# First let's minimize the size of the object for quicker comparisons
zones.albers.points2<-zones.albers.points[,c(4,6,9,13,14)] # Keeping only OLD_UID, SiteCode, UID, Julian Date, Geometry
# Now let's extract the values at the points
#ptm <- proc.time()
zones.albers.points2$AtmCa<-terra::extract(AtmCa.ras,zones.albers.points2)
zones.albers.points2$AtmSO4<-terra::extract(AtmSO4.ras,zones.albers.points2)
zones.albers.points2$AtmMg<-terra::extract(AtmMg.ras,zones.albers.points2)

## Now let's work with the polygon variables

# Keep only the variables that pertain to the EC Model
# Create a list of variables so that zonal statistics can be extracted in one step

dropvar <- c("LST32AVE","MINWD_WS","MEANP_WS","XWD_WS","SumAve_P","MAXWD_WS",
             "MINP_WS","TMAX_WS","LPREM_mean","EVI_MaxAve","PRMH_AVE","CaO_Mean","MgO_Mean","S_Mean",
             "UCS_Mean","BDH_AVE","KFCT_AVE")


### Areal zonal stats /// Try a loop to obtain all variables at the polygon level
ptm <- proc.time()
zonalvars<-dropvar[c(1:17)]
for (var in zonalvars){
  #newvar<-paste0(var,"_AH")
  newvar<-var
  gisvar<-eval(parse(text = paste0(var,".ras")))# This makes sure that the character of the variable name can be recognized as a variable name in exact_extract
  print(paste("processing",var))
  Wsheds.att[[newvar]]<-exact_extract(x=gisvar,y=Wsheds.att,'mean')

}

proc.time() - ptm
### Putting things together

## Bringing everything together

master.deliver<-master.list[,c(1:15)]

# Bring the point-based predictors
master.deliver01<-merge(master.deliver, zones.albers.points2, by="UID", no.dups=TRUE)
# Drop duplicated variables, get rid of the .x and remove the .y variables

combined <- master.deliver01 %>% 
  rename_at(
    vars(ends_with(".x")),
    ~str_replace(., "\\..$","")
  ) %>% 
  select_at(
    vars(-ends_with(".y"))
  )

## RBIND the two polygon datasets
master.deliver02<-Wsheds.att
#names(master.deliver01)

## Merge points and polygons
## Perform inner joins using shared UID or OLD_UID
master.deliver03<- inner_join(master.deliver01, master.deliver02, by=c("UID"= "OLD_UIDT"))
master.deliver04<- inner_join(master.deliver01, master.deliver02, by=c("OLD_UID.y"= "OLD_UIDT"))
# Bind it together
master.deliver05<-rbind(master.deliver03,master.deliver04)
# Keep ONLY unique rows
master.deliver07<- distinct(master.deliver05, UID, .keep_all = TRUE)
master.deliver07<-master.deliver07[-c(25)]

master.deliver09 <- master.deliver07 %>% 
  rename_at(
    vars(ends_with(".x")),
    ~str_replace(., "\\..$","")
  ) %>% 
  select_at(
    vars(-ends_with(".y"))
  )

#st_geometry(master.deliver09) <- NULL
#st_set_geometry(master.deliver09, NULL)
names(master.deliver09)
master.deliver09<-master.deliver09[,-c(16)]
write_csv(master.deliver07, here("TNTP_2019_4Review_Dec01_2020f.csv"))
# Export to a csv
write_csv(master.deliver09, here("EC_2019_4Review_Dec01_2020f.csv"))

#####################################################################
#####################################################################
#####################################################################





