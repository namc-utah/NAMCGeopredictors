## Load useful packages
library(sf)
library(raster)
library(data.table)
setwd("/Volumes/IntelSSD/BUG_BLM/ZonalTest")
library(here)
library(ggpubr)
library(mapview)
library(prism)
library(exactextractr)
library(mapedit)
library(units)
library(rgee)
ee_Initialize()

## AREMP OE model

# The model requires a suite of predictors. In order to extract the appropriate metric a particular
# geospatial operation is required. The script is prepared based on inputs and required
# operation

## Work with watersheds - polygons that require mean statistics for the entire area or zone


## Define the projection to work throughout the project
# Load the HUCs - this shapefile is a good place to start for projection references
HUCs<- st_read(here("Vectors","HUC4_All.shp"))
## And now extract its crs
crs2use<-crs(HUCs)


### Begin loading into memory the required rasters and vectors

## Climate !!!
TMAX_WS.ras<-raster(here("GIS_Stats01/Climate/Data/tmax_usgs","w001001.adf"))
PMIN_WS.ras<-raster(here("GIS_Stats01/Climate/Data/pmin_usgs","w001001.adf"))
RH_WS.ras<-raster(here("GIS_Stats01/Climate/Data/rhmean_usgs","w001001.adf"))
TMEAN_WS.ras<-raster(here("GIS_Stats01/Climate/Data/tmean_usgs","w001001.adf"))

## Soils !!!
KFACT.ras<-raster(here("GIS_Stats01/Soils/Data/kfact_usgs","w001001.adf"))

### Extract elevation using Earth Engine
USGS_NED<-ee$Image("USGS/NED")$select("elevation")


### Let's see if reference data can be read to validate this scripts results

## S Miller said on 08/18/2020 that this may be it
## Z:\buglab\OE_Modeling\NAMC_Supported_OEmodels\AREMP2014\IndexDevelopment2014\RefSiteChecks\RefSiteChecks\CAL_ws_4June2014.kmz

arem.kmz<-st_read(here("Vectors","CAL_ws_4June2014.kml"))
arem.kmz<-st_transform(arem.kmz, crs2use)
arem.kmz.valid<-st_make_valid(arem.kmz)


# Extract the mean elevation across the watershed
Ref.elmean001to115<-ee_extract(USGS_NED, arem.kmz.valid[c(1:115),], fun = ee$Reducer$mean(), scale=30, sf = TRUE)
Ref.elmean116to215<-ee_extract(USGS_NED, arem.kmz.valid[c(116:215),], fun = ee$Reducer$mean(), scale=30, sf = TRUE)
# Extract the maximum elevation across the watershed
Ref.elmax001to115<-ee_extract(USGS_NED, arem.kmz.valid[c(1:115),], fun = ee$Reducer$max(), scale=30, sf = TRUE)
Ref.elmax116to215<-ee_extract(USGS_NED, arem.kmz.valid[c(116:215),], fun = ee$Reducer$max(), scale=30, sf = TRUE)
# Extract the maximum elevation across the watershed
Ref.elmin001to115<-ee_extract(USGS_NED, arem.kmz.valid[c(1:115),], fun = ee$Reducer$min(), scale=30, sf = TRUE)
Ref.elmin116to215<-ee_extract(USGS_NED, arem.kmz.valid[c(116:215),], fun = ee$Reducer$min(), scale=30, sf = TRUE)

# Keep only attributes of interest prior to extracting zonals stats from existing datasets @ NAMC server
names(Ref.elmean001to115)
names(Ref.elmean116to215)
## Let's keep only Name (sampleid),Shape_Length,"Shape_Area","elevation","geometry" 
Ref.elmean001to115<-Ref.elmean001to115[,c(1,3,4)]
Ref.elmean116to215<-Ref.elmean116to215[,c(1,3,4)]
# rename elevation to ELVmean
setnames(Ref.elmean001to115, "elevation", "ELVmean")
setnames(Ref.elmean116to215, "elevation", "ELVmean")
# Create an object that merges the OR and WA objects mean elevation
Ref.elmean<-rbind(Ref.elmean001to115,Ref.elmean116to215)
# Now process the max elevation
## Let's keep only Name (sampleid),"elevation","geometry" 
Ref.elmax001to115<-Ref.elmax001to115[,c(1,3,4)]
Ref.elmax116to215<-Ref.elmax116to215[,c(1,3,4)]
# rename elevation to ELVmax
setnames(Ref.elmax001to115, "elevation", "ELVmax")
setnames(Ref.elmax116to215, "elevation", "ELVmax")
# Create an object that merges the OR and WA objects max elevation
Ref.elmax<-rbind(Ref.elmax001to115,Ref.elmax116to215)

# Now process the min elevation
## Let's keep only Name (sampleid),"elevation","geometry" 
Ref.elmin001to115<-Ref.elmin001to115[,c(1,3,4)]
Ref.elmin116to215<-Ref.elmin116to215[,c(1,3,4)]
# rename elevation to ELVmax
setnames(Ref.elmin001to115, "elevation", "ELVmin")
setnames(Ref.elmin116to215, "elevation", "ELVmin")
# Create an object that merges the OR and WA objects max elevation
Ref.elmin<-rbind(Ref.elmin001to115,Ref.elmin116to215)

# Calculate the area for the mean elevation object
Ref.elmean$WSA_SQKM<-st_area(Ref.elmean)/1000000

Ref.elmean<-drop_units(Ref.elmean)
# Areal zonal stats using Terra from locally saved rasters

Ref.elmean$TMAX_WS<-exact_extract(TMAX_WS.ras,Ref.elmean,'mean')
Ref.elmean$PMIN_WS<-exact_extract(PMIN_WS.ras,Ref.elmean,'mean')
Ref.elmean$KFACT<-exact_extract(KFACT.ras,Ref.elmean,'mean')
Ref.elmean$RH_WS<-exact_extract(RH_WS.ras,Ref.elmean,'mean')
Ref.elmean$TMEAN_WS<-exact_extract(TMEAN_WS.ras,Ref.elmean,'mean')

## Now merge objects
## Drop the geometry from elevation objects
Ref.elmean<-st_set_geometry(Ref.elmean, NULL)
Ref.elmax<-st_set_geometry(Ref.elmax, NULL)
Ref.elmin<-st_set_geometry(Ref.elmin, NULL)


#the mean and max objects -- This is done as data.frames at the end
Ref.AREMP<-merge(Ref.elmean, Ref.elmax, by="Name")
#setnames(Ref.AREMP, "AreaKM", "WSA_SQKM")
#Ref.AREMP<-drop_units(Ref.AREMP)
Ref.AREMP<-merge(Ref.AREMP, Ref.elmin, by="Name")
names(Ref.AREMP)
# Changing the names so that they reconcile with the expectation for Trip's script

setnames(Ref.AREMP, c("ELVmean","ELVmax" ,  "ELVmin"), c("ELVmean_WS","ELVmax_WS" ,  "ELVmin_WS"))
## Now bring the original values from Scotts's file
ref.predictors<-read.csv(here("Vectors","ref_preds_MMI.csv"))

names(ref.predictors)

# Keeping only predictors of interest
ref.predictors<-ref.predictors[,c(4,32,5,7,12,22, 37,6,34,33)]
setnames(ref.predictors,c("ELVmean_WS","WSA_SQKM","TMAX_WS","PMIN_WS","KFCT_WS","rh_WS","TMEAN_WS" ,"ELVmax_WS","ELVmin_WS"),
         c("ELVmean_WSOR","WSA_SQKMOR","TMAX_WSOR","PMIN_WSOR","KFCT_WSOR","rh_WSOR","TMEAN_WSOR" ,"ELVmax_WSOR","ELVmin_WSOR"))

Comparison<-merge(Ref.AREMP, ref.predictors, by.x="Name", by.y="MASTER_ID")

#Comparing variables
ggscatter(Comparison, x = "ELVmin_WS", y = "ELVmin_WSOR", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "ELVmin_WS_Alex", ylab = "ELVmin_WS_Original")


## Save dataframe for report
write.csv(Comparison, here("Models_Comparisons/AREM","AREMP2014_Comparison_0820_2020_OEMMI.csv"))


## Import the pour points
ref.predictors.pourpoints<-st_read(here("Vectors","CAL_pt_4June2014.kml"))
ref.predictors.pourpoints<-st_transform(ref.predictors.pourpoints, crs2use)

## Extract values from REQUIRED rasters
ref.predictors.pourpoints$TMEAN_PT<-extract(TMEAN_WS.ras, ref.predictors.pourpoints)
ref.predictors.pourpoints$PMIN_PT<-extract(PMIN_WS.ras, ref.predictors.pourpoints)

extract(PMIN_WS.ras, ref.predictors.pourpoints)

# Keeping only predictors of interest
ref.predictors2<-ref.predictors[,c(4,44,49)]
names(ref.predictors2)
setnames(ref.predictors2,c("TMEAN_PT" , "PMIN_PT"),
         c("TMEAN_PTOR" , "PMIN_PTOR"))

ref.predictors.pourpoints2<-st_set_geometry(ref.predictors.pourpoints, NULL)

Comparison2<-merge(ref.predictors.pourpoints2, ref.predictors2, by.x="Name", by.y="MASTER_ID")

### !!!! NOTICE !!! the pourpoints that were provided contains decimal places for some variables, but the ones that are in the
### Server are INTEGER --- How is that possible?????????? !!!!

#Comparing variables
ggscatter(Comparison2, x = "PMIN_PT", y = "PMIN_PTOR", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "PMIN_PT_Alex", ylab = "PMIN_PT_Original")


st_write(gdbORWA.merge,here("Vectors","AREMP2019_StreamStatsTest08052020_OEpredictors.shp"))


## 



######## Removed from previous version
## Load the geodatabases that were obtained from StreamStats
## Oregon
gdbOR<-st_read(dsn=here("Zonal_Stats/Vectors/aremp2019_oregon_qgis_snapped300m_manual132410562672032774","aremp2019_oregon_qgis_snapped300m_manual132410562672032774.gdb"),
               layer="GlobalWatershed")
gdbOR<-st_make_valid(gdbOR)
gdbOR<- st_set_crs(gdbOR,crs2use)
gdbWA<-st_read(dsn=here("Vectors/aremp2019_wa_qgis_snapped300m_manual132410572081397476","aremp2019_wa_qgis_snapped300m_manual132410572081397476.gdb"),
               layer="GlobalWatershed")
gdbWA<-st_make_valid(gdbWA)
gdbWA<- st_set_crs(gdbWA,crs2use)
# ## For some reason the number of columns were not identical
# gdbOR<-gdbOR[,c(1:9,11:14)]
# # Names MUST be identical prior to merging the datasets into one
# names(gdbWA)<-names(gdbOR)
# gdbAREMP<-rbind(gdbOR,gdbWA)
# 
# gdbAREMP<-st_make_valid(gdbAREMP)
# 
# gdbAREMP<- st_set_crs(gdbAREMP,crs2use)


##gdbAREMP.albers<-st_transform(gdbAREMP,crs2use)

## GEE seems to accept less than 200 features
## Watersheds must be processed individually
# Extract the mean elevation across the watershed
gdbOR.elmean<-ee_extract(USGS_NED, gdbOR, fun = ee$Reducer$mean(), scale=30, sf = TRUE)
gdbWA.elmean<-ee_extract(USGS_NED, gdbWA, fun = ee$Reducer$mean(), scale=30, sf = TRUE)
# Extract the maximum elevation across the watershed
gdbOR.elmax<-ee_extract(USGS_NED, gdbOR, fun = ee$Reducer$max(), scale=30, sf = TRUE)
gdbWA.elmax<-ee_extract(USGS_NED, gdbWA, fun = ee$Reducer$max(), scale=30, sf = TRUE)

# Keep only attributes of interest prior to extracting zonals stats from existing datasets @ NAMC server
names(gdbOR.elmean)
names(gdbOR.elmax)
## Let's keep only Name (sampleid),Shape_Length,"Shape_Area","elevation","geometry" 
gdbOR.elmean<-gdbOR.elmean[,c(3,11:14)]
gdbWA.elmean<-gdbWA.elmean[,c(3,11:14)]
# rename elevation to ELVmean
setnames(gdbOR.elmean, "elevation", "ELVmean")
setnames(gdbWA.elmean, "elevation", "ELVmean")
# Create an object that merges the OR and WA objects mean elevation
gdbORWA<-rbind(gdbOR.elmean,gdbWA.elmean)
# Now process the max elevation
## Let's keep only Name (sampleid),"elevation","geometry" 
gdbOR.elmax<-gdbOR.elmax[,c(3,13:14)]
gdbWA.elmax<-gdbWA.elmax[,c(3,13:14)]
# rename elevation to ELVmax
setnames(gdbOR.elmax, "elevation", "ELVmax")
setnames(gdbWA.elmax, "elevation", "ELVmax")
# Create an object that merges the OR and WA objects max elevation
gdbORWAmax<-rbind(gdbOR.elmax,gdbWA.elmax)



# Now let's do zonal stats with NAMC datasets
# Areal zonal stats

gdbORWA$TMAX_WS<-exact_extract(TMAX_WS.ras,gdbORWA,'mean')
gdbORWA$PMIN_WS<-exact_extract(PMIN_WS.ras,gdbORWA,'mean')

## Now merge the mean and max objects -- This is done as data.frames at the end
gdbORWA.merge<-cbind(gdbORWA,gdbORWAmax)

gdbORWA.merge$WSA_SQKM<-gdbORWA.merge$Shape_Area/1000000

names(gdbORWA.merge)

gdbORWA.merge<-gdbORWA.merge[,c(1:6,8,9,11)]

st_write(gdbORWA.merge,here("Vectors","AREMP2019_StreamStatsTest08052020_OEpredictors.shp"))

