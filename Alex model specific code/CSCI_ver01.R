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

## CSCI model

### Description


### Load required inputs
### Required for WY model are:
# Shapefile of sampling points // *csv of Lat Long
# Shapefile of delineated watershed polygons

# Register the points ///
## In this case we are trying to replicate reference predictors
master.list<-st_read(here("CSCI/CSCIsheds","CSCIpts.shp"))
# Retaining attributes of interest: New_Lat, New_Long, siteid, StationCod
master.list<-master.list[,c(9,10,14,15)]

# Read in polygons
# Register the watersheds that are ready
## In this case we are trying to replicate reference predictors
Wsheds.raw<-st_read(here("CSCI/CSCIsheds","CSCIsheds.shp"))
# Retaining attributes of interest:  siteid, StationC_1
Wsheds.raw<-Wsheds.raw[,c(14,15)]


## Define the projection to work throughout the project
# Load the HUCs - this shapefile is a good place to start for projection references
HUCs<- st_read(here("Vectors","HUC4_All.shp"))
## And now extract its crs
crs2use<-crs(HUCs)


# Project the watersheds
Wsheds.albers<-st_transform(Wsheds.raw, crs2use)
Points.albers<-st_transform(master.list, crs2use)

Wsheds.albers<-Wsheds.raw
Points.albers<-master.list


### Define the inputs that will be used for point extractions and zonal statistics
## Reference for models is: //Zonal environmental statistics/AllPredictiveModelsCurrentlyRun.xlsx

## Climate !!!
SumAve_P.ras<-raster(here("GIS_Stats01/Climate/Data/sumave_p2.tif"))

# Geology
PCT_SEDIM.vec<-st_read(here("GIS_Stats01/Geology/Data/N_America_Geol_usgsNumeric.shp"))
LPREM_mean.ras<-raster(here("GIS_Stats01/Geology/Data/lperm_2feb10","w001001.adf"))

# Metrics
# Watershed Area --> to be extracted from polygon
PRMH_AVE.ras<-raster(here("GIS_Stats01/Soils/Data/permh_usgs","w001001.adf"))
CaO_Mean.ras<-raster(here("GIS_Stats01/Geology/Data/cao_19jan10","w001001.adf"))
MgO_Mean.ras<-raster(here("GIS_Stats01/Geology/Data/Mgo_19jan10","w001001.adf"))
N_MEAN.ras<-raster(here("GIS_Stats01/Geology/Data/n_19jan10","w001001.adf"))
P_MEAN.ras<-raster(here("GIS_Stats01/Geology/Data/p_19jan10","w001001.adf"))
S_Mean.ras<-raster(here("GIS_Stats01/Geology/Aug28/Data/s_23aug10","w001001.adf")) # !!! Note here about source
# New_Long --> to be extracted from points
# New_Lat --> to be extracted from points

### GEE variables

USGS_NED<-ee$Image("USGS/NED")$select("elevation")
# More Metrics
TEMP_00_09.ras<-raster(here("GIS_Stats01/Metrics/California/Data/temp00_09.tif"))
PPT_00_09.ras<-raster(here("GIS_Stats01/Metrics/California/Data/ppt_00_09.tif"))
BDH_AVE.ras<-raster(here("GIS_Stats01/Soils/Data/bdh_usgs","w001001.adf"))
KFCT_AVE.ras<-raster(here("GIS_Stats01/Soils/Data/kfact_usgs","w001001.adf"))


#### Geoprocessing area

### Extract the values at the point

# Now let's extract some values
# First let's minimize the size of the object for quicker comparisons
zones.albers.points2<-Points.albers # Make a copy of the object
# Now let's extract the values at the points
#ptm <- proc.time()
zones.albers.points2$SITE_ELEV<-pull(ee_extract(x=USGS_NED, y=st_set_crs(zones.albers.points2,crs2use), scale=30)%>% as_tibble())/10
zones.albers.points2$TEMP_00_09<-terra::extract(TEMP_00_09.ras,zones.albers.points2)
zones.albers.points2$PPT_00_09<-terra::extract(PPT_00_09.ras,zones.albers.points2)

## Now let's work with the polygon variables

# Keep only the variables that pertain to the CSCI Model
# These variables DO NOT require any additional arithmetic operation or transformation (i.e. division, log)

dropvar <- c("SumAve_P","LPREM_mean","PRMH_AVE","CaO_Mean","MgO_Mean","N_MEAN","P_MEAN","S_Mean","BDH_AVE","KFCT_AVE")

#ECzones3<- ECzones[,(names(ECzones) %in% dropvar)]

### Areal zonal stats /// Try a loop to obtain all variables at the polygon level
ptm <- proc.time()
#zonalvars<-dropvar[c(1:17)]
for (var in dropvar){
  #newvar<-paste0(var,"_AH")
  newvar<-var
  gisvar<-eval(parse(text = paste0(var,".ras")))# This makes sure that the character of the variable name can be recognized as a variable name in exact_extract
  print(paste("processing",var))
  Wsheds.albers[[newvar]]<-exact_extract(x=gisvar,y=Wsheds.albers,'mean')

}

proc.time() - ptm

### Vector geoprocessing
### Obtain the polygon area in AREA_SQKM
Wsheds.albers$AREA_SQKM<-st_area(Wsheds.albers)/1000000
Wsheds.albers<-drop_units(Wsheds.albers)

## Now Sedimentary geology
## First clip the geology to the watersheds
Sedim<-st_make_valid(PCT_SEDIM.vec)
Wsheds.albers<-st_make_valid(Wsheds.albers) # Found invalid geometries thus fixing them
geology<-st_intersection(Sedim, Wsheds.albers)
## Summarize and find the % per watershed that is sedimentary
geology2<-st_cast(geology, "POLYGON") ## Experimenting issues with the intersection
# Prepare an intermediary sf object to obtain % ONLY for sedimentary geologies
geo3<-geology2 %>%
  #st_cast(geology, "POLYGON")%>%
  select(GEOnum, siteid, AREA_SQKM)%>%
  mutate(AREA_SQKM = st_area(geology2)/1000000)%>%# update the AREA for subsequent calculations
  group_by(siteid)%>%
  #mutate(PORC = round(AREA_SQKM/sum(AREA_SQKM)*100,2))
  mutate(PORC = ifelse(GEOnum == 5, round(AREA_SQKM/sum(AREA_SQKM)*100,2),0))
  #summarise(PORC = ifelse(GEOnum == 5, round(AREA_SQKM/sum(AREA_SQKM)*100,2),0))

## Finally, obtain the added Percent by watershed  
geo4<-geo3%>%
  select(siteid,PORC)%>%
  group_by(siteid)%>%
 summarise(PCT_SEDIM = sum(PORC))

Sedim.perc<-st_set_geometry(geo4, NULL)# Create an object that is just a DF from the previous sf object

# Now bring the attribute to the general table
Wsheds.albers<-merge(Wsheds.albers, Sedim.perc, by ="siteid")

### Finally GEE calculations
# Create required attributes with NA
# Create a copy of the wshed object only preserving some attributes
#Wsheds.albers2<-Wsheds.albers[,1]
Wsheds.albers$ELVmax_WS<-NA
Wsheds.albers$ELVmin_WS<-NA
Wsheds.albers$ELEV_RANGE<-NA
#Wsheds.att.OLD$PPT_ACCUM<-NA
ptm <- proc.time()
for (i in 1:nrow(Wsheds.albers)){
  tryCatch({ #if an error is found then it is printed, but the loop does not break and continues with the next iteration
    objecto<-Wsheds.albers[i,] # Take the first feature
    elmax<-ee_extract(x=USGS_NED, y=st_set_crs(objecto,crs2use), fun = ee$Reducer$max(), scale=30)%>% as_tibble()
    elmin<-ee_extract(x=USGS_NED, y=st_set_crs(objecto,crs2use), fun = ee$Reducer$min(), scale=30)%>% as_tibble()
    print(elmax)
    print(elmin)
    #slope.water<-pull(slope.water)
    Wsheds.albers[[15]][i]<-elmax
    Wsheds.albers[[16]][i]<-elmin
    Wsheds.albers[[17]][i]<-(elmax - elmin)
  },error=function(e){cat("ERROR :",conditionMessage(e), "\n")})  
}
proc.time() - ptm

Wsheds.albers$ELVmax_WS<-unlist(Wsheds.albers$ELVmax_WS)
Wsheds.albers$ELVmin_WS<-unlist(Wsheds.albers$ELVmin_WS)
Wsheds.albers$ELEV_RANGE<-unlist(Wsheds.albers$ELEV_RANGE)
Wsheds.albers$ELVmax_WS<-Wsheds.albers$ELVmax_WS/10
Wsheds.albers$ELVmin_WS<-Wsheds.albers$ELVmin_WS/10


Wsheds.albers$ELVmax_WS<-pull(ee_extract(USGS_NED, Wsheds.albers, fun = ee$Reducer$max(), scale=30)%>% as_tibble())/10


## Bringing everything together

#master.deliver<-master.list[,c(1:15)]
# Drop the geometries to merge the two (point and polygon) dataframe

points<-st_set_geometry(zones.albers.points2, NULL) # remove geometry, coerce to data.frame
#buffer<-st_set_geometry(zones.albers.points2.buff, NULL) # remove geometry, coerce to data.frame
#buffer<-buffer[,c(1:14,17)]
#points<-inner_join(points, buffer, by="Sitecode")
polys<-st_set_geometry(Wsheds.albers, NULL) # remove geometry, coerce to data.frame

# Bring the point-based predictors
master.deliver01<-merge(points, polys, by="reachid")

# Just this time
master.deliver01$SITE_ELEV<-master.deliver01$SITE_ELEV*10

master.deliver01<-master.deliver01[,c(1:5,22,19,7,6,9,17,18,15)]
colnames(master.deliver01[2:4])<-c("station","New_Lat","New_Long")

keepvar<-c("New_Lat",    "New_Long","SITE_ELEV",  "TEMP_00_09", "PPT_00_09",   "SumAve_P",  
           "LPREM_mean", "PRMH_AVE" ,  "CaO_Mean" ,  "MgO_Mean" ,  "N_MEAN" ,    "P_MEAN",     "S_Mean",
           "BDH_AVE" ,   "KFCT_AVE",  
           "AREA_SQKM",  "PCT_SEDIM", "ELEV_RANGE")
######33
# Write the CSV

write.csv(master.deliver01, here("CSCI/CSCI_Priority_Predictors_0319_2021.csv"))

######### To prepare correlation plots

# Bring in the reference predictions
ref.preds<-read.csv(here("vectors/CSCI_ModelData_16May2014.csv"))
# leave only the predictors of interest
# For some reason `siteid`, `StationCod`, `StationC_1`, `ELVmax_WS`, and `ELVmin_WS` don't exist.
ref.preds2<-ref.preds%>%
  select(StationCode, all_of(keepvar))

## Modify column names in Alex's predictor names
master.deliver02<-master.deliver01
colnames(master.deliver02) <- paste(colnames(master.deliver02),"_Alex", sep = "_")

# Now put everything in one big dataframe
master.deliver03<-left_join(master.deliver02, ref.preds2, by=c("StationCod__Alex"="StationCode"))

master.deliver03$diffsed<-master.deliver03$PCT_SEDIM - master.deliver03$PCT_SEDIM__Alex


for (varplot in keepvar){
  #scatter<-paste0(varplot,"_compare")
  scatter<-ggscatter(master.deliver03, x = varplot, y = paste0(varplot,"__Alex"), 
                     add = "reg.line", conf.int = TRUE, 
                     cor.coef = TRUE, cor.method = "pearson",
                     xlab = "Existing", ylab = "Alex", title=varplot)
  ggexport(scatter,filename = here("CSCI_Plots",paste0(varplot,"_CorrPlot.pdf")))
}









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










## Load the Reference Sites Predictor Values
RefSites<-readxl::read_xlsx(here("Vectors","EC_referencedata_with_resd_pred_08272020.xlsx"))


## Load the polygons as a simple features object

zones01<-st_read(here("Vectors", "John_WS_14NOV12.shp"))
names(zones01)
## Keeping only important columns to perform a "merge"
zones01<-zones01[,c(1:4,240)]

ECzones<-merge(zones01, RefSites, by="SiteCode")

# Keep only the variables that pertain to the EC Model

dropvar <- c("SiteCode","AtmCa","AtmSO4","AtmMg","LST32AVE","MINWD_WS","MEANP_WS","XWD_WS","SumAve_P","MAXWD_WS",
             "MINP_WS","TMAX_WS","LPREM_mean","EVI_MaxAve","PRMH_AVE","CaO_Mean","MgO_Mean","S_Mean",
             "UCS_Mean","BDH_AVE","KFCT_AVE","GIS_LAT","GIS_LONG")
             
ECzones3<- ECzones[,(names(ECzones) %in% dropvar)]

### Areal zonal stats /// Try a loop to obtain all variables at the polygon level

zonalvars<-dropvar[c(5:21)]
for (var in zonalvars){
  newvar<-paste0(var,"_AH")
  gisvar<-eval(parse(text = paste0(var,".ras")))# This makes sure that the character of the variable name can be recognized as a variable name in exact_extract
  print(paste("processing",var))
  ECzones3[[newvar]]<-exact_extract(x=gisvar,y=ECzones3,'mean')
  #ECzones3[[newvar]]<-NA
  #print(gisvar)
}

## Now let's generate correlation plots to assess how good the new predictors compared to ArcPy versions

### Comparison plots

for (varplot in zonalvars){
  scatter<-paste0(varplot,"_compare")
  scatter<-ggscatter(ECzones3, x = varplot, y = paste0(varplot,"_AH"), 
            add = "reg.line", conf.int = TRUE, 
            cor.coef = TRUE, cor.method = "pearson",
            xlab = "Existing", ylab = "Alex", title=varplot)
  ggexport(scatter,filename = here("EC_plots",paste0(varplot,"_test.pdf")))
}





zones.Albers.2$TMIN_WSalex<-exact_extract(TMIN_WS.ras,zones.Albers.2,'mean')
zones.Albers.2$RH_WSalex<-exact_extract(RH_WS.ras,zones.Albers.2,'mean')
zones.Albers.2$XWD_WSalex<-exact_extract(XWD_WS.ras,zones.Albers.2,'mean')
zones.Albers.2$Vol_avealex<-exact_extract(Vol_ave.ras,zones.Albers.2,'mean')




zones.Albers<-zones01
#zones.latlong<-st_transform(zones01, crslatlon)

# First let's work with the points
## Let's do some testing with a few variables focusing on Atmospheric deposition and see if watershed area may be
## the source of differences between original values and those obtained using R


## Now let's try some raster values extraction at the points
# Start by preparing a simple features object that is point-based NOT polygon

# Put some info into a dataframe
zones.point.df<-as.data.frame(zones.Albers)
## Now convert to POINT geometry
zones.points.latlon<- st_as_sf(zones.point.df, coords = c("GIS_LONG", "GIS_LAT"), 
                     crs = 4326)
# Transform
zones.albers.points<-st_transform(zones.points.latlon,crs=crs2use)

# Now let's extract some values
# First let's minimize the size of the object for quicker comparisons
#zones.albers.points2<-zones.albers.points[,c(1:4,51,55,59,61)]
# Now let's extract the values at the points
ptm <- proc.time()
zones.albers.points2$AtmCa_alex<-terra::extract(AtmCa.ras,zones.albers.points2)
zones.albers.points2$AtmSO4_alex<-terra::extract(AtmSO4.ras,zones.albers.points2)
zones.albers.points2$AtmNa_alex<-terra::extract(AtmNa.ras,zones.albers.points2)
zones.albers.points2$AtmNO3_alex<-terra::extract(AtmNO3.ras,zones.albers.points2)

proc.time() - ptm


## Let's get residuals

zones.albers.points2$NO3res<-zones.albers.points2$AtmNO3_alex-(zones.albers.points2$AtmNO3)

## Culprits have been found EPA-01-514 and PIBO275

# They have "wrong"? coordinates???

## Now let's work with zonal Statistics for polygons

zones.Albers.2<-zones01[,c(1:4,68,72,76,227,152,95,22,34,160,120,231,162,158,153,185)]
#zones.Albers.2<-st_transform(zones.Albers.2, crs=crsraster)
# Let's try a quick zonal stats with the previous variables



# Work with the ALRU variable
# First recode the alru_dom.ras raster --- we need to only keep the 1
# create classification matrix
# reclass_df <- c(0, 0, NA,
#                 1, 1, 1)
# reclass_m <- matrix(reclass_df,
#                     ncol = 3,
#                     byrow = TRUE)
# ptm <- proc.time()
# alru_dom.ras[alru_dom.ras == 0] <- NA
# proc.time() - ptm

zones.Albers.2$alru_domalex<-exact_extract(alru_dom.ras,zones.Albers.2,'count')


proc.time() - ptm


## Now let's try a quick way to obtain zonal statistics
ptm <- proc.time()
zones.latlong$PPT_ACCUMalex<-exact_extract(RS.PRISM.sum, zones.latlong, 'mean')
proc.time() - ptm


zones.latlong2<-zones.latlong[,c(1:4,225,240,241)]



# simple map
mapview(list(HUCs["REGION"],zones.Albers),layer.name=c("HUCs Level 4", "Sites"))

## Let's create an object that just keep the SiteCode field 
zones.Albers.code<-zones.Albers[,1]
summary(as.factor(zones.Albers$Source))

