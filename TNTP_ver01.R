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

## Zonal statistics require that both rasters: the one being used to extract statistics from
## and the ones for which statistics will be calculated to have the same extent, pixel size,
## and origin

##### Start loading and defining critical inputs
## Define the projection to work throughout the project
# Load the HUCs - this shapefile is a good place to start for projection references
HUCs<- st_read(here("Vectors","HUC4_All.shp"))

## And now extract its crs
crs2use<-crs(HUCs)


# Register the points

master.list<-read.csv(here("Vectors","Sites_For_Delineation_2019F.csv"), na.strings = c("","NA"))

# Register the watersheds that are ready

Wsheds.raw<-st_read(here("Vectors","Wsheds_Total_before07_merge.shp"))
Wsheds.raw<-Wsheds.raw[,c(1,3)]
### Divide the Watersheds into Two 2 sets based on OLDUID or Current UID
### first let's convert to a proper date format --- the sampling date
master.list$date<- as.Date(master.list$SampleDate, "%m/%d/%y")
##3 Then convert to Julian format
master.list$julian <- yday(master.list$date)

#### RUNNING THE EXTRACTION AS ONE SET --- EXACTLY AS IT WAS RUN FOR OLSON'S
#### !!!!!!!!! MUST COME BACK TO THIS SECTION FOR JOINING BASED ON OLD_UIDT OR UID
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
AtmNa.ras<-raster(here("GIS_Stats01/Atmos/Data/atm_na","w001001.adf"))
AtmNO3.ras<-raster(here("GIS_Stats01/Atmos/Data/atm_no3","w001001.adf"))

## Climate !!!
PT_Tmin.ras<-raster(here("GIS_Stats01/Climate/Data/tmin_usgs","w001001.adf"))
TMIN_WS.ras<-raster(here("GIS_Stats01/Climate/Data/tmin_oldtntp","w001001.adf"))
#st_crs(TMIN_WS.ras)<-42303
RH_WS.ras<-raster(here("GIS_Stats01/Climate/Data/rhmean_usgs","w001001.adf"))
XWD_WS.ras<-raster(here("GIS_Stats01/Climate/Data/xwd_usgs","w001001.adf"))

### Set up slope percent variable in GEE
#### Extract the slope using GEE

### Extract elevation using Earth Engine
USGS_NED<-ee$Image("USGS/NED")$select("elevation")

slopegee<-ee$Terrain$slope(USGS_NED)

slopegee.perc<- slopegee$divide(180)$multiply(3.14159)$tan()$multiply(1)$rename("percent")



# Set up the PRISM inputs from GEE
## First let's get the accumulated precipitation from May - April of the previous year
# Insert the value of current year here: !!!
curYear<-2019

prevYear1<-curYear-1
prevYear0<-prevYear1-1

WaterYearStart<-paste0(prevYear0,"-05-01")
WaterYearEnd<-paste0(prevYear1,"-04-30")
# Obtain a GEE image that has the accumulated precipitation
prism.accum0<-ee$ImageCollection('OREGONSTATE/PRISM/AN81m')$filter(ee$Filter$date(WaterYearStart, WaterYearEnd))$select('ppt')
prism.accum.precip<-prism.accum0$sum()

# visualize annual precipitation

visParams = list(
  palette = c("white", "blue"),
  min = 60,
  max = 3000
)

Map$addLayer(prism.accum.precip, visParams, paste('accumulated precipitation Water Year:', prevYear0,'-',prevYear1))

## Now preparing the PPT_2MoAvg variable 

# Insert the value of current year here: !!!
curYear.2month<-2019

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

## EcoRegion !!!
Eco3_PT.vec<-st_read(here("GIS_Stats01/Ecoregion/Data","Eco_Level_III_US.shp"))

## Geology
Vol_ave.ras<-raster(here("GIS_Stats01/Geology/Data/vol","w001001.adf"))

## Landcover

alru_dom.ras<-raster(here("GIS_Stats01/Vegetation/Data/alru_domrec","w001001.adf"))
Evergr_ave.ras<-raster(here("GIS_Stats01/Vegetation/Data/evergr","w001001.adf"))
EVI_AveAve.ras<-raster(here("GIS_Stats01/Vegetation/Data/evi_ave","w001001.adf"))

## Metrics

CaO_Mean.ras<-raster(here("GIS_Stats01/Geology/Data/cao_19jan10","w001001.adf"))
TP_Mean.ras<-raster(here("GIS_Stats01/Geology/Data/p_19jan10","w001001.adf"))
AWC_soil.ras<-raster(here("GIS_Stats01/Soils/Data/awc","w001001.adf"))
GW_P_Sp_Mx.ras<-raster(here("GIS_Stats01/Hydro/Data/gw_p_sp","w001001.adf"))
SOC.ras<-raster(here("GIS_Stats01/Soils/Data/soc","w001001.adf"))
Pct_Alfi.ras<-raster(here("GIS_Stats01/Soils/Data/alfi_nonulls","w001001.adf"))
Wb_mx_area.vec<-st_read(here("GIS_Stats01/Metrics/Data","Wb.shp"))
Kfact.ras<-raster(here("GIS_Stats01/Soils/Data/kfact_usgs","w001001.adf"))
Db3rdbar.ras<-raster(here("GIS_Stats01/Soils/Data/db3rdbar","w001001.adf"))


######## !!!!!! Now start getting zonal Stats !!!!!################
###################################################################
###################################################################

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
ptm <- proc.time()
zones.albers.points2$AtmCa<-terra::extract(AtmCa.ras,zones.albers.points2)
zones.albers.points2$AtmSO4<-terra::extract(AtmSO4.ras,zones.albers.points2)
zones.albers.points2$AtmNa<-terra::extract(AtmNa.ras,zones.albers.points2)
zones.albers.points2$AtmNO3<-terra::extract(AtmNO3.ras,zones.albers.points2)
zones.albers.points2$PT_Tmin<-terra::extract(PT_Tmin.ras,zones.albers.points2)
# For ecoregions we're only interested in the first column == US_L3CODE
Eco3_PT.vec.l3<-Eco3_PT.vec[,c(1)]
zones.albers.points2$Eco3_PT<-NA
zones.albers.points2$Eco3_PT<-st_intersection(zones.albers.points2, Eco3_PT.vec.l3)%>%pull(US_L3CODE)
zones.albers.points2$ER13<-NA

zones.albers.points2$ER13<- zones.albers.points2 %>%
                              mutate(ER13 = case_when(
                              Eco3_PT == 23 ~ "Y",
                              Eco3_PT != 23 ~ "N"))%>%pull(ER13)

proc.time() - ptm

# !!!! Now work with Areal zonal stats
### So that we maintain the purity of OLD_UID and UID, this must be done for each set
### 

ptm <- proc.time()
Wsheds.att$TMIN_WS<-exact_extract(TMIN_WS.ras,Wsheds.att,'mean')
Wsheds.att$RH_WS<-exact_extract(RH_WS.ras,Wsheds.att,'mean')
Wsheds.att$XWD_WS<-exact_extract(XWD_WS.ras,Wsheds.att,'mean')
Wsheds.att$TMIN_WS<-exact_extract(TMIN_WS.ras,Wsheds.att,'mean')
Wsheds.att$RH_WS<-exact_extract(RH_WS.ras,Wsheds.att,'mean')
Wsheds.att$XWD_WS<-exact_extract(XWD_WS.ras,Wsheds.att,'mean')
proc.time() - ptm

####################### This section uses GEE
## Get the accumulated precipitation !!! More than likely these loops will need to be translated to functions for more transparency

Wsheds.att$PPT_ACCUM<-NA
#Wsheds.att.OLD$PPT_ACCUM<-NA
ptm <- proc.time()
for (i in 1:nrow(Wsheds.att)){
  tryCatch({ #if an error is found then it is printed, but the loop does not break and continues with the next iteration
    objecto<-Wsheds.att[i,] # Take the first feature
    pcpacum.extraction<-ee_extract(prism.accum.precip, objecto, fun = ee$Reducer$mean(), scale=4000)%>% as_tibble()
    print(pcpacum.extraction)
    pcpacum.extraction<-pull(pcpacum.extraction)
    Wsheds.att[[9]][i]<-pcpacum.extraction
  },error=function(e){cat("ERROR :",conditionMessage(e), "\n")})  
}
proc.time() - ptm
# Check if there are NA's during the calculation and we HAVE TO WORK WITH A MUCH FINER RESOLUTION
sum(is.na(Wsheds.att$PPT_ACCUM))
## put them in a df
cochinto<-is.na(Wsheds.att$PPT_ACCUM)

## Populate ONLY the records that WERE NOT calculated BEFORE
for (i in 1:nrow(Wsheds.att)){
  if (cochinto[i]==TRUE){
    tryCatch({ #if an error is found then it is printed, but the loop does not break and continues with the next iteration
      objecto<-Wsheds.att[i,] # Take the first feature
      #objecto<-objecto[,c(1)]
      pcpacum.extraction<-ee_extract(prism.accum.precip, objecto, fun = ee$Reducer$mean(), scale=30)%>% as_tibble()
      pcpacum.extraction<-pull(pcpacum.extraction)
      print(i)
      print(pcpacum.extraction)
      Wsheds.att[[9]][i]<-pcpacum.extraction
  },error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    #print(Wsheds.att.CUR$Lat[i])
  }
  
}

### Now let's get the PPT_2MoAvg variable
Wsheds.att$PPT_2MoAvg<-NA
#Wsheds.att.OLD$PPT_2MoAvg<-NA
ptm <- proc.time()
for (i in 1:nrow(Wsheds.att)){
  tryCatch({ #if an error is found then it is printed, but the loop does not break and continues with the next iteration
    juliandate<-Wsheds.att[[4]][i] # Grab the Julian Date
    #row<-row.names(testin.2MO)[j]
    #site<-testin.2MO[[1]][j] # Grab the site name
    month.cur<-as.Date(juliandate-1, origin = "2019-01-01") # Transform to a YYYY-MM-DD format
    monthy.cur<-as.numeric(substr(month.cur, 6, 7)) # Estimate the CURRENT month number based on the YYYY-MM-DD format
    monthy.pre<-monthy.cur-1 # Estimate the PREVIOUS month number based on the YYYY-MM-DD format
    xx<-eval(parse(text = paste0("prism.",monthy.cur))) # Evaluations that are required so that a variable is recognized as such
    xxx<-eval(parse(text = paste0("prism.",monthy.pre)))# Evaluations that are required so that a variable is recognized as such
    objecto<-Wsheds.att[i,] # Take the first feature
    pcp.extraction.cur<-ee_extract(xx, objecto, fun = ee$Reducer$mean(), scale=4000)%>% as_tibble() # Compute pcp for CURRENT month
    pcp.extraction.pre<-ee_extract(xxx, objecto, fun = ee$Reducer$mean(), scale=4000)%>% as_tibble()# Compute pcp for PREVIOUS month
    pcp.extraction<-((pcp.extraction.pre+pcp.extraction.cur)/2)*100 # Obtain average and multiply by 100 so it is similar to Olson
    pcp.extraction<-pcp.extraction%>% as_tibble()
    #print(site)
    pcp.extraction<-pull(pcp.extraction)
    print(pcp.extraction)
    Wsheds.att[[10]][i]<-pcp.extraction
  },error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
   # ticurin[[j]]<-pcp.extraction ## Aggregate the computation to the list
}
# Stop the clock
proc.time() - ptm

# Check if there are NA's during the calculation
sum(is.na(Wsheds.att$PPT_2MoAvg))
#sum(is.na(Wsheds.att.CUR$PPT_ACCUM))
cochinto<-is.na(Wsheds.att$PPT_2MoAvg)
## Populate ONLY the records that WERE NOT calculated BEFORE
for (i in 1:nrow(Wsheds.att)){
  if (cochinto[i]==TRUE){
    tryCatch({ #if an error is found then it is printed, but the loop does not break and continues with the next iteration
      juliandate<-Wsheds.att[[4]][i] # Grab the Julian Date
      #row<-row.names(testin.2MO)[j]
      #site<-testin.2MO[[1]][j] # Grab the site name
      month.cur<-as.Date(juliandate-1, origin = "2019-01-01") # Transform to a YYYY-MM-DD format
      monthy.cur<-as.numeric(substr(month.cur, 6, 7)) # Estimate the CURRENT month number based on the YYYY-MM-DD format
      monthy.pre<-monthy.cur-1 # Estimate the PREVIOUS month number based on the YYYY-MM-DD format
      xx<-eval(parse(text = paste0("prism.",monthy.cur))) # Evaluations that are required so that a variable is recognized as such
      xxx<-eval(parse(text = paste0("prism.",monthy.pre)))# Evaluations that are required so that a variable is recognized as such
      objecto<-Wsheds.att[i,] # Take the first feature
      objecto<-objecto[,c(1)]
      pcp.extraction.cur<-ee_extract(xx, objecto, fun = ee$Reducer$mean(), scale=30)%>% as_tibble() # Compute pcp for CURRENT month
      pcp.extraction.pre<-ee_extract(xxx, objecto, fun = ee$Reducer$mean(), scale=30)%>% as_tibble()# Compute pcp for PREVIOUS month
      pcp.extraction<-((pcp.extraction.pre+pcp.extraction.cur)/2)*100 # Obtain average and multiply by 100 so it is similar to Olson
      pcp.extraction<-pcp.extraction%>% as_tibble()
      #print(site)
      pcp.extraction<-pull(pcp.extraction)
      print(pcp.extraction)
      Wsheds.att[[10]][i]<-pcp.extraction
    },error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    #print(Wsheds.att.CUR$Lat[i])
  }
  
}



####### Continue processing rasters stored on disk

ptm <- proc.time()
Wsheds.att$Vol_ave<-exact_extract(Vol_ave.ras,Wsheds.att,'mean')
#Wsheds.att.OLD$Vol_ave<-exact_extract(Vol_ave.ras,Wsheds.att.OLD,'mean')

### !!!! These variables need to be converted to % of the watershed
Wsheds.att$alru_dom<-exact_extract(alru_dom.ras,Wsheds.att,'count')## It is count because there are only 1 to 1 values
#Wsheds.att.OLD$alru_dom<-exact_extract(alru_dom.ras,Wsheds.att.OLD,'count')
Wsheds.att$Evergr_ave<-exact_extract(Evergr_ave.ras,Wsheds.att,'sum')## It is sum because there are values 0 and 1
#Wsheds.att.OLD$Evergr_ave<-exact_extract(Evergr_ave.ras,Wsheds.att.OLD,'sum')
Wsheds.att$Pct_Alfi<-exact_extract(Pct_Alfi.ras,Wsheds.att,'sum')## It is sum because there are values 0 and 1
#Wsheds.att.OLD$Pct_Alfi<-exact_extract(Pct_Alfi.ras,Wsheds.att.OLD,'sum')## It is sum because there are values 0 and 1

Wsheds.att$AreaHa<-st_area(Wsheds.att)/10000
#Wsheds.att.OLD$AreaHa<-st_area(Wsheds.att.OLD)/10000

Wsheds.att<-drop_units(Wsheds.att)
#Wsheds.att.OLD<-drop_units(Wsheds.att.OLD)

Wsheds.att$alru_domf<-((Wsheds.att$alru_dom*0.09)/Wsheds.att$AreaHa)*100 # It is a 30x30 m pixel
Wsheds.att$Evergr_avef<-((Wsheds.att$Evergr_ave*0.09)/Wsheds.att$AreaHa) # It is a 30x30 m pixel !!!! It DOES NOT require to be divided by 100 because it uses decimals
Wsheds.att$Pct_Alfif<-((Wsheds.att$Pct_Alfi*25)/Wsheds.att$AreaHa)*100 # It is a 500x500 m pixel

#Wsheds.att.OLD$alru_domf<-((Wsheds.att.OLD$alru_dom*0.09)/Wsheds.att.OLD$AreaHa)*100 # It is a 30x30 m pixel
#Wsheds.att.OLD$Evergr_avef<-((Wsheds.att.OLD$Evergr_ave*0.09)/Wsheds.att.OLD$AreaHa)*100 # It is a 30x30 m pixel
#Wsheds.att.OLD$Pct_Alfif<-((Wsheds.att.OLD$Pct_Alfi*25)/Wsheds.att.OLD$AreaHa)*100 # It is a 500x500 m pixel
proc.time() - ptm


########!!!!!


Wsheds.att$EVI_AveAve<-exact_extract(EVI_AveAve.ras,Wsheds.att,'mean')## 
#Wsheds.att.OLD$EVI_AveAve<-exact_extract(EVI_AveAve.ras,Wsheds.att.OLD,'mean')

Wsheds.att$CaO_Mean<-exact_extract(CaO_Mean.ras,Wsheds.att,'mean')## 
#Wsheds.att.OLD$CaO_Mean<-exact_extract(CaO_Mean.ras,Wsheds.att.OLD,'mean')
Wsheds.att$TP_Mean<-exact_extract(TP_Mean.ras,Wsheds.att,'mean')## 
#Wsheds.att.OLD$TP_Mean<-exact_extract(TP_Mean.ras,Wsheds.att.OLD,'mean')
Wsheds.att$AWC_soil<-exact_extract(AWC_soil.ras,Wsheds.att,'mean')## 
#Wsheds.att.OLD$AWC_soil<-exact_extract(AWC_soil.ras,Wsheds.att.OLD,'mean')
Wsheds.att$GW_P_Sp_Mx<-exact_extract(GW_P_Sp_Mx.ras,Wsheds.att,'max')## !!!!! this changed
#Wsheds.att.OLD$GW_P_Sp_Mx<-exact_extract(GW_P_Sp_Mx.ras,Wsheds.att.OLD,'mean')## 
Wsheds.att$SOC<-exact_extract(SOC.ras,Wsheds.att,'mean')##
#Wsheds.att.OLD$SOC<-exact_extract(SOC.ras,Wsheds.att.OLD,'mean')##

### Export up to here in case calculating the largest water body interferes with topology !!!!
write.csv(Wsheds.att, here("TNTP_beforeLargestWBody.csv"))

## Area of the largest water body in the watershed

### object just for testing
#Wsheds.att.CUR.borrar<-Wsheds.att.CUR
#Wsheds.att.CUR.borrar$Wb_mx_area<-NA
Wb_mx_area.vec.valid<-st_make_valid(Wb_mx_area.vec)
Wsheds.att$Wb_mx_area<-NA
#Wsheds.att.OLD$Wb_mx_area<-NA
ptm <- proc.time()
for (i in 1:nrow(Wsheds.att)){
  tryCatch({ #if an error is found then it is printed, but the loop does not break and continues with the next iteration
    objecto<-Wsheds.att[i,] # Take the first feature 
    objecto<-objecto[,c(1)]
    objecto.val<-st_make_valid(objecto)
    bodies<-st_intersection(Wb_mx_area.vec.valid, objecto.val)
    bodies$AreaSqKm<-st_area(bodies)/1000000
    bodies<-drop_units(bodies)
    maxarea<-max(bodies$AreaSqKm)
    print(maxarea)
    Wsheds.att[[25]][i]<-maxarea ## !!! Why variable 21 in the previous code??????
  },error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}
# Stop the clock
proc.time() - ptm


Wsheds.att$Kfact<-exact_extract(Kfact.ras,Wsheds.att,'mean')## 
#Wsheds.att.OLD$Kfact<-exact_extract(Kfact.ras,Wsheds.att.OLD,'mean')

Wsheds.att$Db3rdbar<-exact_extract(Db3rdbar.ras,Wsheds.att,'mean')## 
#Wsheds.att.OLD$Db3rdbar<-exact_extract(Db3rdbar.ras,Wsheds.att.OLD,'mean')

## Export to csv so that we can detect issues
write.csv(Wsheds.att, here("TNTP_beforeSlope.csv"))


####### This area here just to regenerate mean slope watersheds for Jennifer
zones.Albers.3<-zones.Albers.2[,c(1:4)]
#Wsheds.att.OLD$PPT_ACCUM<-NA
zones.Albers.3$SlopeGEE<-NA
ptm <- proc.time()
for (i in 1:nrow(zones.Albers.3)){
  tryCatch({ #if an error is found then it is printed, but the loop does not break and continues with the next iteration
    objecto<-zones.Albers.3[i,] # Take the first feature
    slope.water<-ee_extract(slopegee.perc, objecto, fun = ee$Reducer$mean(), scale=30)%>% as_tibble()
    print(slope.water)
    slope.water<-pull(slope.water)
    zones.Albers.3[[6]][i]<-slope.water
  },error=function(e){cat("ERROR :",conditionMessage(e), "\n")})  
}
proc.time() - ptm
#################################################################################
#################################################################################
#################################################################################

### Just this once
#Wsheds.att$Evergr_avef<-(Wsheds.att$Evergr_avef/100)

## Bringing everything together



master.deliver<-master.list[,c(1:16)]

# Bring the point-based predictors
master.deliver01<-merge(master.deliver, zones.albers.points2, by="UID")

## RBIND the two polygon datasets
#master.deliver02<-rbind(Wsheds.att.CUR,Wsheds.att.OLD)
master.deliver02<-Wsheds.att
#names(master.deliver01)

## Merge points and polygons
## Perform inner joins using shared UID or OLD_UID
master.deliver03<- inner_join(master.deliver01, master.deliver02, by=c("UID"= "OLD_UIDT"))
master.deliver04<- inner_join(master.deliver01, master.deliver02, by=c("OLD_UID.y"= "OLD_UIDT"))
# Bind it together
master.deliver05<-rbind(master.deliver03,master.deliver04)
# Keep ONLY unique rows
master.deliver06<- distinct(master.deliver05, UID, .keep_all = TRUE)
master.deliver07<- distinct(master.deliver05, UID, .keep_all = TRUE)

# master.deliver07<-master.deliver07[ -c(15,17,18,19,20,28,29,38:40,45,53) ] ## variable extraction seems wrong!!!
master.deliver07<-master.deliver07[ -c(15,17,18,19,20,28,29,31,38:40) ] 
master.deliver061<-master.deliver06[ -c(15,17,18,19,20,28,29,31,38:40) ] 

## Before writing, let's make Jenniffer's modifications

master.deliver061$PPT_ACCUM<-master.deliver061$PPT_ACCUM*100
master.deliver061$Wb_mx_area<-master.deliver061$Wb_mx_area*1000000
master.deliver061$alru_domf<-master.deliver061$alru_domf/100

write_csv(master.deliver061, here("TNTP_2019_4Review_Dec07_2020f.csv"))
st_geometry(master.deliver07) <- NULL




# Because GEE variables were list they needed to be unnested 
master.deliver09<- master.deliver07 %>% unnest(cols = c(PPT_ACCUM, PPT_2MoAvg))



# Export to a csv
write_csv(master.deliver09, here("TNTP_2019_4Review.csv"))

duplicates01<- data.frame(table(master.deliver05$UID))




