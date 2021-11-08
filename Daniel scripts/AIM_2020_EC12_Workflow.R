#AIM 2020 EC12 MOdeling#

#####Load required packages#####
library(sf)
library(raster)
library(mapview)
library(exactextractr)
library(rgee)
library(dplyr)
library(units)
library(geojsonio)
library(rmapshaper)

ee_Initialize() #Initialize Google Earth Engine (GEE)

######Load the required rasters#####
setwd("/Volumes/GIS/GIS_Stats/")
## Atmosphere !!!
AtmCa.ras<-raster("Atmos/Data/atm_ca/w001001.adf")
AtmSO4.ras<-raster("Atmos/Data/atm_so4/w001001.adf")
AtmMg.ras<-raster("Atmos/Data/atm_mg2.tif")

## Climate !!!
LST32AVE.ras<-raster("Climate/Data/lstfrz_usgs/w001001.adf")
MINWD_WS.ras<-raster("Climate/Data/Wdmin_usgs/w001001.adf")
MEANP_WS.ras<-raster("Climate/Data/meanp_usgs/w001001.adf")
XWD_WS.ras<-raster("Climate/Data/xwd_usgs/w001001.adf")
SumAve_P.ras<-raster("Climate/Data/sumave_p2.tif")
MAXWD_WS.ras<-raster("Climate/Data/Wdmax_usgs/w001001.adf")
MINP_WS.ras<-raster("Climate/Data/pmin_usgs/w001001.adf")
TMAX_WS.ras<-raster("Climate/Data/tmax_usgs/w001001.adf")

## Geology
LPREM_mean.ras<-raster("Geology/Data/lperm_2feb10/w001001.adf")

## Landcover
EVI_MaxAve.ras<-raster("Vegetation/Data/evi_max_10B.tif")

## Metrics
PRMH_AVE.ras<-raster("Soils/Data/permh_usgs/w001001.adf")
CaO_Mean.ras<-raster("Geology/Data/cao_19jan10/w001001.adf")
MgO_Mean.ras<-raster("Geology/Data/Mgo_19jan10/w001001.adf")
S_Mean.ras<-raster("Geology/Data/s_23aug10/w001001.adf")
UCS_Mean.ras<-raster("Geology/Data/ucs_19jan10/w001001.adf")
BDH_AVE.ras<-raster("Soils/Data/bdh_usgs/w001001.adf")
KFCT_AVE.ras<-raster("Soils/Data/kfact_usgs/w001001.adf")

#####Load the functions needed to extract predictors for the EC12 model#####
#Predictor Functions for EC12 Model
BDH_AVE<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exactextractr::exact_extract(BDH_AVE.ras,validgeometry,'mean')
  return(media)
}

CaO_Mean<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(CaO_Mean.ras,validgeometry,'mean')
  return(media)
}

KFCT_AVE<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(KFCT_AVE.ras,validgeometry,'mean')
  return(media)
}

LPREM_mean<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(LPREM_mean.ras,validgeometry,'mean')
  return(media)
}

PRMH_AVE<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(PRMH_AVE.ras,validgeometry,'mean')
  return(media)
}

S_Mean<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(S_Mean.ras,validgeometry,'mean')
  return(media)
}

SumAve_P<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(SumAve_P.ras,validgeometry,'mean')
  return(media)
}

TMAX_WS<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(TMAX_WS.ras,validgeometry,'mean')
  return(media)
}

XWD_WS<-function(polygon2process){
  sfobject<-geojson_sf(polygon2process)
  validgeometry<-st_make_valid(sfobject)
  validgeometry$XWD_WS<-exact_extract(XWD_WS.ras,validgeometry,'mean')
  media<-as.data.frame(validgeometry$XWD_WS)
  colnames(media)<-"XWD_WS"
  return(media)
}

AtmCa<-function(points2process){
  validgeometry<-geojson_sf(points2process)
  media<-raster::extract(AtmCa.ras,validgeometry)
  return(media)
}

AtmMg<-function(points2process){
  validgeometry<-geojson_sf(points2process)
  media<-raster::extract(AtmMg.ras,validgeometry)
  return(media)
}

AtmSO4<-function(points2process){
  validgeometry<-geojson_sf(points2process)
  media<-raster::extract(AtmSO4.ras,validgeometry)
  return(media)
}

EVI_MaxAve<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(EVI_MaxAve.ras,validgeometry,'mean')
  return(media)
}

LST32AVE<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(LST32AVE.ras,validgeometry,'mean')
  return(media)
}

MAXWD_WS<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(MAXWD_WS.ras,validgeometry,'mean')
  return(media)
}

MEANP_WS<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(MEANP_WS.ras,validgeometry,'mean')
  return(media)
}

MgO_Mean<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(MgO_Mean.ras,validgeometry,'mean')
  return(media)
}

MINP_WS<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(MINP_WS.ras,validgeometry,'mean')
  return(media)
}

MINWD_WS<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(MINWD_WS.ras,validgeometry,'mean')
  return(media)
}

UCS_Mean<-function(polygon2process){
  validgeometry<-geojson_sf(polygon2process)
  media<-exact_extract(UCS_Mean.ras,validgeometry,'mean')
  return(media)
}

#####Load the watersheds shapefile#####
AIM2020 <- st_read("/Volumes/GIS/GIS_Stats/MasterSheds/AIM2020Sheds/All2020Sheds.shp")

AIM2020<-AIM2020[,2]
AIM2020.WGS<-st_transform(AIM2020, crs = 4326)
AIM2020.WGS.json<-geojson_json(AIM2020.WGS) #Use this to extract predictors

#Polygons
BDH_AVE_list <- BDH_AVE(AIM2020.WGS.json)
CaO_Mean_list <- CaO_Mean(AIM2020.WGS.json)
EVI_MaxAve_list <- EVI_MaxAve(AIM2020.WGS.json) 
KFCT_AVE_list <- KFCT_AVE(AIM2020.WGS.json)
LPREM_mean_list <- LPREM_mean(AIM2020.WGS.json)
LST32AVE_list <- LST32AVE(AIM2020.WGS.json)
MAXWD_WS_list <- MAXWD_WS(AIM2020.WGS.json)
MEANP_WS_list <- MEANP_WS(AIM2020.WGS.json)
MgO_Mean_list <- MgO_Mean(AIM2020.WGS.json)
MINP_WS_list <- MINP_WS(AIM2020.WGS.json)
MINWD_WS_list <- MINWD_WS(AIM2020.WGS.json)
PRMH_AVE_list <- PRMH_AVE(AIM2020.WGS.json)
S_Mean_list <- S_Mean(AIM2020.WGS.json)
SumAve_P_list <- SumAve_P(AIM2020.WGS.json)
TMAX_WS_list <- TMAX_WS(AIM2020.WGS.json)
XWD_WS_list <- XWD_WS(AIM2020.WGS.json)
UCS_Mean_list <- UCS_Mean(AIM2020.WGS.json)

AIM2020.sheds.1 <- AIM2020.sheds %>% st_set_geometry(NULL)
wshed_preds <- cbind(AIM2020.sheds.1, BDH_AVE_list, CaO_Mean_list, EVI_MaxAve_list, KFCT_AVE_list, LPREM_mean_list, 
                     LST32AVE_list, MAXWD_WS_list, MEANP_WS_list, MgO_Mean_list, MINP_WS_list,
                     MINWD_WS_list, PRMH_AVE_list, S_Mean_list, SumAve_P_list, TMAX_WS_list, XWD_WS_list,
                     UCS_Mean_list)

#####Load the points shapefile#####
AIM2020 <- st_read("/Volumes/GIS/GIS_Stats/MasterSheds/AIM2020Sheds/All2020Points.shp")

#Points
AtmCa_list <- AtmCa(AIM2020.WGS.json) 
AtmMg_list <- AtmMg(AIM2020.WGS.json) 
AtmSO4_list <- AtmSO4(AIM2020.WGS.json) 

AIM2020.pts.1 <- AIM2020.pts %>% st_set_geometry(NULL)
point_preds <- cbind(AIM2020.pts.1, AtmCa_list, AtmMg_list, AtmSO4_list)

df <- wshed_preds %>% full_join(point_preds, by = "UID")
head(df)

library(lubridate)
julian <- yday(df$SampleDate.y)
df$SampleDate.y <- format(df$SampleDate.y, format = "%m/%d/%y")
date <- df$SampleDate.y
df <- data.frame(df$UID, df$Priority_s.y, df$Bug_model.y, df$WQ_model.y, df$OLD_UID.y,
                 df$Existing_w.y, df$SiteCode.y, df$location.y, df$MasterCode.y, df$SampleDate.y, 
                 df$Lat.y, df$Long.y, df$Project.y, date, julian, df$AtmCa_list, df$AtmSO4_list,
                 df$AtmMg_list, df$LST32AVE_list, df$MINWD_WS_list, df$MEANP_WS_list, df$XWD_WS_list, 
                 df$SumAve_P_list, df$MAXWD_WS_list, df$MINP_WS_list, df$TMAX_WS_list, df$LPREM_mean_list,
                 df$EVI_MaxAve_list, df$PRMH_AVE_list, df$CaO_Mean_list, df$MgO_Mean_list, df$S_Mean_list,
                 df$UCS_Mean_list, df$BDH_AVE_list, df$KFCT_AVE_list)

AIM2020_EC12_Predictors <- df %>% 
  rename(
    UID = df.UID, Priority.site = df.Priority_s.y, Bug.model = df.Bug_model.y, WQ.model = df.WQ_model.y, 
    OLD_UID = df.OLD_UID.y, Existing.watershed.location = df.Existing_w.y, SiteCode = df.SiteCode.y, 
    location = df.location.y, MasterCode = df.MasterCode.y, SampleDate = df.SampleDate.y, Lat = df$Lat.y,
    Long = df.Long.y, Project = df.Project.y, date = date, julian = julian, AtmCa = df.AtmCa_list, AtmSO4 = df.AtmSO4_list,
    AtmMg = df.AtmMg_list, LST32AVE = df.LST32AVE_list, MINWD_WS = df.MINWD_WS_list, MEANP_WS = df.MEANP_WS_list, 
    XWD_WS = df.XWD_WS_list, SumAve_P = df.SumAve_P_list, MAXWD_WS = df.MAXWD_WS_list, MINP_WS = df.MINP_WS_list,
    TMAX_WS = df.TMAX_WS_list, LPREM_mean = df.LPREM_mean_list, EVI_MaxAve = df.EVI_MaxAve_list, PRMH_AVE = df.PRMH_AVE_list, 
    CaO_Mean = df.CaO_Mean_list, MgO_Mean = df.MgO_Mean_list, S_Mean = df.S_Mean_list, UCS_Mean = df.UCS_Mean_list, 
    BDH_AVE = df.BDH_AVE_list, KFCT_AVE = df.KFCT_AVE_list
  )

getwd() #Where the file will end up
#write.csv(AIM2020_EC12_Predictors, "AIM2020EC12_Predictors.csv")

#####Run the EC12 model on AIM 2020 data#####
library(randomForest)
head(AIM2020_EC12_Predictors)

ECref = read.csv("/Volumes/miller/buglab/Transferred/OE_Modeling/Transferred/NAMC_Supported_OEmodels/Transferred/WQ models/Rfiles/EC_Model_ReferenceData_cleaned_JC_28April2016.csv")
EC_AIM = AIM2020_EC12_Predictors
rownames(EC_AIM) = EC_AIM$UID
names(EC_AIM)[names(EC_AIM) == 'SITE_ID'] = 'SiteCode'
EC_AIM$SiteCode=as.factor(EC_AIM$SiteCode)
EC_AIM$Type = "AIM"
EC_AIM = EC_AIM[,c("SiteCode", "AtmCa", "AtmMg", "AtmSO4","BDH_AVE", "CaO_Mean", "EVI_MaxAve", "KFCT_AVE", "LPREM_mean", "LST32AVE", "MAXWD_WS", "MEANP_WS", "MgO_Mean", "MINP_WS", "MINWD_WS", "PRMH_AVE", "S_Mean", "SumAve_P", "TMAX_WS","UCS_Mean", "XWD_WS","Type")]

ECboxplot=rbind(ECref,EC_AIM)
ECboxplot=ECboxplot[,c(6,13,17,21,9,2,3,4,14,12,18,19,22,11,10,5,8,16,7,20)] # In the order in which they are listed in the saved RandomForest model 

# Boxplots: These will write as .png's directly to the current working directory
getwd()

# EC 
png('AIM2020_EC12_boxplots.png',height=1000,width=2000,units="px")
par(mfrow=c(3,7))
for (i in 1:(length(ECboxplot)-1)) {
  boxplot(ECboxplot[,i]~ECboxplot$Type, main=names(ECboxplot[i]),col=c(7,4))
}
dev.off()

load(sprintf("/Volumes/miller/buglab/Transferred/OE_Modeling/Transferred/NAMC_Supported_OEmodels/Transferred/WQ models/Rfiles/rf17bCnd9")); rf17bCnd9; #EC

EC_AIM_RF_out=as.data.frame(predict(rf17bCnd9,EC_AIM))
colnames(EC_AIM_RF_out) = c("EC_predicted")
