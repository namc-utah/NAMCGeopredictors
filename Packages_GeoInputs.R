## Load required packages
## Load useful packages
library(sf)
library(raster)
library(data.table)
setwd("Z://GIS/GIS_Stats")
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
library(geojsonio)
library(rmapshaper)

ee_Initialize()

###### Define predictors GEE
USGS_NED<-ee$Image("USGS/NED")$select("elevation") # elevation
slopegee<-ee$Terrain$slope(USGS_NED) # slope
slopegee.perc<- slopegee$divide(180)$multiply(3.14159)$tan()$multiply(1)$rename("percent")#Slope percent

## PRISM accumulated precipitation from May - April of the previous year
curYear<-2019 # Insert the value of current year here: !!!
prevYear1<-curYear-1
prevYear0<-prevYear1-1
WaterYearStart<-paste0(prevYear0,"-05-01")
WaterYearEnd<-paste0(prevYear1,"-04-30")
# Obtain a GEE image that has the accumulated precipitation
prism.accum0<-ee$ImageCollection('OREGONSTATE/PRISM/AN81m')$filter(ee$Filter$date(WaterYearStart, WaterYearEnd))$select('ppt')
prism.accum.precip<-prism.accum0$sum()
## Now preparing the PPT_2MoAvg variable 
curYear.2month<-2019# Insert the value of current year here: !!!

# Obtain a GEE image that has the monthly precipitation for those months where sample can occur -- in this case from February to November
prism.1<-ee$ImageCollection('OREGONSTATE/PRISM/AN81m')$filter(ee$Filter$date(paste0(curYear.2month,"-01-01"), paste0(curYear.2month,"-01-31")))$select('ppt')
prism.2<-ee$ImageCollection('OREGONSTATE/PRISM/AN81m')$filter(ee$Filter$date(paste0(curYear.2month,"-02-01"), paste0(curYear.2month,"-02-28")))$select('ppt')
prism.3<-ee$ImageCollection('OREGONSTATE/PRISM/AN81m')$filter(ee$Filter$date(paste0(curYear.2month,"-03-01"), paste0(curYear.2month,"-03-31")))$select('ppt')
prism.4<-ee$ImageCollection('OREGONSTATE/PRISM/AN81m')$filter(ee$Filter$date(paste0(curYear.2month,"-04-01"), paste0(curYear.2month,"-04-30")))$select('ppt')
prism.5<-ee$ImageCollection('OREGONSTATE/PRISM/AN81m')$filter(ee$Filter$date(paste0(curYear.2month,"-05-01"), paste0(curYear.2month,"-05-31")))$select('ppt')
prism.6<-ee$ImageCollection('OREGONSTATE/PRISM/AN81m')$filter(ee$Filter$date(paste0(curYear.2month,"-06-01"), paste0(curYear.2month,"-06-30")))$select('ppt')
prism.7<-ee$ImageCollection('OREGONSTATE/PRISM/AN81m')$filter(ee$Filter$date(paste0(curYear.2month,"-07-01"), paste0(curYear.2month,"-07-31")))$select('ppt')
prism.8<-ee$ImageCollection('OREGONSTATE/PRISM/AN81m')$filter(ee$Filter$date(paste0(curYear.2month,"-08-01"), paste0(curYear.2month,"-08-31")))$select('ppt')
prism.9<-ee$ImageCollection('OREGONSTATE/PRISM/AN81m')$filter(ee$Filter$date(paste0(curYear.2month,"-09-01"), paste0(curYear.2month,"-09-30")))$select('ppt')
prism.10<-ee$ImageCollection('OREGONSTATE/PRISM/AN81m')$filter(ee$Filter$date(paste0(curYear.2month,"-10-01"), paste0(curYear.2month,"-10-31")))$select('ppt')
prism.11<-ee$ImageCollection('OREGONSTATE/PRISM/AN81m')$filter(ee$Filter$date(paste0(curYear.2month,"-11-01"), paste0(curYear.2month,"-11-30")))$select('ppt')


###### Define predictors stored in-house
## Climate !!!
TMIN_WS.ras<-raster(here("Climate/Data/tmin_oldtntp/w001001.adf")) 
TMIN_UT_WS.ras<-raster(here("/Climate/Data/tmin_usgs","w001001.adf"))# UTDEQ version
#PT_Tmin.ras<-raster(here("/Climate/Data/tmin_usgs","w001001.adf"))
#TMIN_AVE.ras<-raster(here("/Climate/Data/tmin_usgs","w001001.adf"))# UTDEQ version
KFACT.ras<-raster(here("/Soils/Data/kfact_usgs","w001001.adf"))
PMIN_WS.ras<-raster(here("/Climate/Data/pmin_usgs","w001001.adf"))
RH_WS.ras<-raster(here("/Climate/Data/rhmean_usgs","w001001.adf"))# UTDEQ version is RH_AVE
TMAX_WS.ras<-raster(here("/Climate/Data/tmax_usgs","w001001.adf")) # UTDEQ version is TMAX_AVE
TMEAN_WS.ras<-raster(here("/Climate/Data/tmean_usgs","w001001.adf"))
TMEAN_UT_WS.ras<-raster(here("/Climate/Data/tmean_usgsut","w001001.adf"))# UTDEQ version
XWD_WS.ras<-raster(here("/Climate/Data/xwd_usgs","w001001.adf"))
MEANP_WS.ras<-raster(here("/Climate/Data/meanp_usgs","w001001.adf"))# UTDEQ version
MAXP_WS.ras<-raster(here("/Climate/Data/pmax_usgs","w001001.adf"))# UTDEQ version
MAXWD_WS.ras<-raster(here("/Climate/Data/Wdmax_usgs","w001001.adf"))# UTDEQ version
FST32F_WS.ras<-raster(here("/Climate/Data/fstfrz_usgs","w001001.adf"))# UTDEQ version



## Atmosphere !!!
AtmCa.ras<-raster(here("/Atmos/Data/atm_ca","w001001.adf"))
AtmSO4.ras<-raster(here("/Atmos/Data/atm_so4","w001001.adf"))
AtmNa.ras<-raster(here("/Atmos/Data/atm_na","w001001.adf"))
AtmNO3.ras<-raster(here("/Atmos/Data/atm_no3","w001001.adf"))


Eco3_PT.vec<-st_read(here("/Ecoregions/Data","Eco_Level_III_US.shp"))
Vol_ave.ras<-raster(here("/Geology/Data/vol","w001001.adf"))
alru_dom.ras<-raster(here("/Vegetation/Data/alru_domrec","w001001.adf"))
Evergr_ave.ras<-raster(here("/Vegetation/Data/evergr","w001001.adf"))
EVI_AveAve.ras<-raster(here("/Vegetation/Data/evi_ave","w001001.adf"))
EVI_MAX_AVE.ras<-raster(here("/Vegetation/Data/evi_max_10b.tif"))# UTDEQ version
CaO_Mean.ras<-raster(here("/Geology/Data/cao_19jan10","w001001.adf"))
TP_Mean.ras<-raster(here("/Geology/Data/p_19jan10","w001001.adf"))
AWC_soil.ras<-raster(here("/Soils/Data/awc","w001001.adf"))
GW_P_Sp_Mx.ras<-raster(here("/Hydro/Data/gw_p_sp","w001001.adf"))
SOC.ras<-raster(here("/Soils/Data/soc","w001001.adf"))
Pct_Alfi.ras<-raster(here("/Soils/Data/alfi_nonulls","w001001.adf"))
Wb_mx_area.vec<-st_read(here("/Metrics/Data","Wb.shp"))
Kfact.ras<-raster(here("/Soils/Data/kfact_usgs","w001001.adf"))
Db3rdbar.ras<-raster(here("/Soils/Data/db3rdbar","w001001.adf"))



