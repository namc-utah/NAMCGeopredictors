## Load required packages
## Load useful packages
library(sf)
library(raster)
library(data.table)
#setwd("/Users/alexhernandez/Desktop/BUG_BLM/ZonalTest")
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

###### Define predictors
USGS_NED<-ee$Image("USGS/NED")$select("elevation")
KFACT.ras<-raster("/Users/alexhernandez/Desktop/BUG_BLM/ZonalTest/GIS_Stats01/Soils/Data/kfact_usgs/w001001.adf")

PMIN_WS.ras<-raster("/Users/alexhernandez/Desktop/BUG_BLM/ZonalTest/GIS_Stats01/Climate/Data/pmin_usgs/w001001.adf")
RH_WS.ras<-raster("/Users/alexhernandez/Desktop/BUG_BLM/ZonalTest/GIS_Stats01/Climate/Data/rhmean_usgs/w001001.adf")
TMAX_WS.ras<-raster("/Users/alexhernandez/Desktop/BUG_BLM/ZonalTest/GIS_Stats01/Climate/Data/tmax_usgs/w001001.adf")
TMEAN_WS.ras<-raster("/Users/alexhernandez/Desktop/BUG_BLM/ZonalTest/GIS_Stats01/Climate/Data/tmean_usgs/w001001.adf")







#### list of functions for each predictor #######################

ELVmean_WS<-function(polygon2process){
  validgeometry<-st_make_valid(polygon2process)
  validgeometry$ELVmean_WS<-NA
  ptm <- proc.time()
  for (i in 1:nrow(validgeometry)){
    tryCatch({ #if an error is found then it is printed, but the loop does not break and continues with the next iteration
      objecto<-validgeometry[i,] # Take the first feature
      elmean<-ee_extract(USGS_NED, objecto, fun = ee$Reducer$mean(), scale=30)%>% as_tibble()
      validgeometry[[4]][i]<-elmean
    },error=function(e){cat("ERROR :",conditionMessage(e), "\n")})  
  }
  proc.time() - ptm
  validgeometry$ELVmean_WS<-unlist(validgeometry$ELVmean_WS)
  media<-as.data.frame(validgeometry$ELVmean_WS)
  colnames(media)<-"ELVmean_WS"
  return(media)
}

ELVmin_WS<-function(polygon2process){
  validgeometry<-st_make_valid(polygon2process)
  validgeometry$ELVmean_WS<-NA
  ptm <- proc.time()
  for (i in 1:nrow(validgeometry)){
    tryCatch({ #if an error is found then it is printed, but the loop does not break and continues with the next iteration
      objecto<-validgeometry[i,] # Take the first feature
      elmean<-ee_extract(USGS_NED, objecto, fun = ee$Reducer$min(), scale=30)%>% as_tibble()
      validgeometry[[4]][i]<-elmean
    },error=function(e){cat("ERROR :",conditionMessage(e), "\n")})  
  }
  proc.time() - ptm
  validgeometry$ELVmean_WS<-unlist(validgeometry$ELVmean_WS)
  media<-as.data.frame(validgeometry$ELVmean_WS)
  colnames(media)<-"ELVmin_WS"
  return(media)
}

KFACT<-function(polygon2process){
  validgeometry<-st_make_valid(polygon2process)
  validgeometry$KFACT<-exact_extract(KFACT.ras,validgeometry,'mean')
  media<-as.data.frame(validgeometry$KFACT)
  colnames(media)<-"KFACT"
  return(media)
}

PMIN_WS<-function(polygon2process){
  validgeometry<-st_make_valid(polygon2process)
  validgeometry$PMIN_WS<-exact_extract(KFACT.ras,validgeometry,'mean')
  media<-as.data.frame(validgeometry$PMIN_WS)
  colnames(media)<-"PMIN_WS"
  return(media)
}

RH_WS<-function(polygon2process){
  validgeometry<-st_make_valid(polygon2process)
  validgeometry$RH_WS<-exact_extract(RH_WS.ras,validgeometry,'mean')
  media<-as.data.frame(validgeometry$RH_WS)
  colnames(media)<-"RH_WS"
  return(media)
}

TMAX_WS<-function(polygon2process){
  validgeometry<-st_make_valid(polygon2process)
  validgeometry$TMAX_WS<-exact_extract(TMAX_WS.ras,validgeometry,'mean')
  media<-as.data.frame(validgeometry$TMAX_WS)
  colnames(media)<-"TMAX_WS"
  return(media)
}

TMEAN_WS<-function(polygon2process){
  validgeometry<-st_make_valid(polygon2process)
  validgeometry$TMEAN_WS<-exact_extract(TMEAN_WS.ras,validgeometry,'mean')
  media<-as.data.frame(validgeometry$TMEAN_WS)
  colnames(media)<-"TMEAN_WS"
  return(media)
}







##### Model functions
AREM_model<-function(polygon2process){
  inputpolys<-polygon2process
  ELVmean_WSS<-ELVmean_WS(inputpolys)
  ELVmin_WSS<-ELVmin_WS(inputpolys)
  KFACTS<-KFACT(inputpolys)
  PMIN_WSS<-PMIN_WS(inputpolys)
  RH_WSS<-RH_WS(inputpolys)
  TMAX_WSS<-TMAX_WS(inputpolys)
  TMEAN_WSS<-TMEAN_WS(inputpolys)
  df2render<-cbind(ELVmean_WSS,ELVmin_WSS,KFACTS,PMIN_WSS,RH_WSS,TMAX_WSS,TMEAN_WSS)
  return(df2render)
}

ptm <- proc.time()
AREMtest<-AREM_model(putin)
proc.time() - ptm

pechereco<-ELVmean_WS(putin)

geojson_write()