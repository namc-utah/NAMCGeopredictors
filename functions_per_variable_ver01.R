## Load required packages
## Load useful packages
library(sf)
library(raster)
library(data.table)
setwd("Z://GIS//GIS_Stats")
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
KFACT.ras<-raster(here("/Soils/Data/kfact_usgs","w001001.adf"))

PMIN_WS.ras<-raster(here("/Climate/Data/pmin_usgs","w001001.adf"))
RH_WS.ras<-raster(here("/Climate/Data/rhmean_usgs","w001001.adf"))
TMAX_WS.ras<-raster(here("/Climate/Data/tmax_usgs","w001001.adf"))
TMEAN_WS.ras<-raster(here("/Climate/Data/tmean_usgs","w001001.adf"))







#### list of functions for each predictor #######################

WSA_SQKM<-function(polygon2process){
  sfobject<-geojson_sf(polygon2process)
  validgeometry<-st_make_valid(sfobject)
  validgeometry$WSA_SQKM<-drop_units(st_area(validgeometry)/1000000)
  media<-as.data.frame(validgeometry$WSA_SQKM)
  colnames(media)<-"WSA_SQKM"
  return(media)
}



ELVmean_WS<-function(polygon2process){
  sfobject<-geojson_sf(polygon2process)
  validgeometry<-st_make_valid(sfobject)
  validgeometry$ELVmean_WS<-NA
  ncolumn<-as.numeric(ncol(validgeometry))
  ptm <- proc.time()
  for (i in 1:nrow(validgeometry)){
    tryCatch({ #if an error is found then it is printed, but the loop does not break and continues with the next iteration
      objecto<-validgeometry[i,] # Take the first feature
      elmean<-ee_extract(USGS_NED, objecto, fun = ee$Reducer$mean(), scale=30)%>% as_tibble()
      elmean<-elmean[,ncolumn]
      validgeometry[[ncolumn]][i]<-elmean
    },error=function(e){cat("ERROR :",conditionMessage(e), "\n")})  
  }
  proc.time() - ptm
  validgeometry$ELVmean_WS<-unlist(validgeometry$ELVmean_WS)
  media<-as.data.frame(validgeometry$ELVmean_WS)
  colnames(media)<-"ELVmean_WS"
  return(media)
}



ELVmin_WS<-function(polygon2process){
  sfobject<-geojson_sf(polygon2process)
  validgeometry<-st_make_valid(sfobject)
  validgeometry$ELVmin_WS<-NA
  ncolumn<-as.numeric(ncol(validgeometry))
  ptm <- proc.time()
  for (i in 1:nrow(validgeometry)){
    tryCatch({ #if an error is found then it is printed, but the loop does not break and continues with the next iteration
      objecto<-validgeometry[i,] # Take the first feature
      elmean<-ee_extract(USGS_NED, objecto, fun = ee$Reducer$min(), scale=30)%>% as_tibble()
      elmean<-elmean[,ncolumn]
      validgeometry[[ncolumn]][i]<-elmean
    },error=function(e){cat("ERROR :",conditionMessage(e), "\n")})  
  }
  proc.time() - ptm
  validgeometry$ELVmin_WS<-unlist(validgeometry$ELVmin_WS)
  media<-as.data.frame(validgeometry$ELVmin_WS)
  colnames(media)<-"ELVmin_WS"
  return(media)
}




ELVmax_WS<-function(polygon2process){
  sfobject<-geojson_sf(polygon2process)
  validgeometry<-st_make_valid(sfobject)
  validgeometry$ELVmax_WS<-NA
  ncolumn<-as.numeric(ncol(validgeometry))
  ptm <- proc.time()
  for (i in 1:nrow(validgeometry)){
    tryCatch({ #if an error is found then it is printed, but the loop does not break and continues with the next iteration
      objecto<-validgeometry[i,] # Take the first feature
      elmean<-ee_extract(USGS_NED, objecto, fun = ee$Reducer$max(), scale=30)%>% as_tibble()
      elmean<-elmean[,ncolumn]
      validgeometry[[ncolumn]][i]<-elmean
    },error=function(e){cat("ERROR :",conditionMessage(e), "\n")})  
  }
  proc.time() - ptm
  validgeometry$ELVmax_WS<-unlist(validgeometry$ELVmax_WS)
  media<-as.data.frame(validgeometry$ELVmax_WS)
  colnames(media)<-"ELVmax_WS"
  return(media)
}


KFACT<-function(polygon2process){
  sfobject<-geojson_sf(polygon2process)
  validgeometry<-st_make_valid(sfobject)
  validgeometry$KFACT<-exact_extract(KFACT.ras,validgeometry,'mean')
  media<-as.data.frame(validgeometry$KFACT)
  colnames(media)<-"KFACT"
  return(media)
}


PMIN_WS<-function(polygon2process){
  sfobject<-geojson_sf(polygon2process)
  validgeometry<-st_make_valid(sfobject)
  validgeometry$PMIN_WS<-exact_extract(PMIN_WS.ras,validgeometry,'mean')
  media<-as.data.frame(validgeometry$PMIN_WS)
  colnames(media)<-"PMIN_WS"
  return(media)
}


RH_WS<-function(polygon2process){
  sfobject<-geojson_sf(polygon2process)
  validgeometry<-st_make_valid(sfobject)
  validgeometry$RH_WS<-exact_extract(RH_WS.ras,validgeometry,'mean')
  media<-as.data.frame(validgeometry$RH_WS)
  colnames(media)<-"RH_WS"
  return(media)
}

TMAX_WS<-function(polygon2process){
  sfobject<-geojson_sf(polygon2process)
  validgeometry<-st_make_valid(sfobject)
  validgeometry$TMAX_WS<-exact_extract(TMAX_WS.ras,validgeometry,'mean')
  media<-as.data.frame(validgeometry$TMAX_WS)
  colnames(media)<-"TMAX_WS"
  return(media)
}

TMEAN_WS<-function(polygon2process){
  sfobject<-geojson_sf(polygon2process)
  validgeometry<-st_make_valid(sfobject)
  validgeometry$TMEAN_WS<-exact_extract(TMEAN_WS.ras,validgeometry,'mean')
  media<-as.data.frame(validgeometry$TMEAN_WS)
  colnames(media)<-"TMEAN_WS"
  return(media)
}

TMEAN_PT<-function(points2process){
  sfobject<-geojson_sf(points2process)
  validgeometry<-st_make_valid(sfobject)
  validgeometry$TMEAN_PT<-raster::extract(TMEAN_WS.ras,validgeometry)
  media<-as.data.frame(validgeometry$TMEAN_PT)
  colnames(media)<-"TMEAN_PT"
  return(media)
}

PMIN_PT<-function(points2process){
  sfobject<-geojson_sf(points2process)
  validgeometry<-st_make_valid(sfobject)
  validgeometry$PMIN_PT<-raster::extract(PMIN_WS.ras,validgeometry)
  media<-as.data.frame(validgeometry$PMIN_PT)
  colnames(media)<-"PMIN_PT"
  return(media)
}





pecherecopoint<-PMIN_PT(AREMP2020.WGS.json.points)



##### Model functions
AREMP_model<-function(polygon2process, points2process){
  inputpolys<-polygon2process
  inputpoints<-points2process
  ELVmean_WSS<-ELVmean_WS(inputpolys)
  ELVmin_WSS<-ELVmin_WS(inputpolys)
  KFACTS<-KFACT(inputpolys)
  PMIN_WSS<-PMIN_WS(inputpolys)
  RH_WSS<-RH_WS(inputpolys)
  TMAX_WSS<-TMAX_WS(inputpolys)
  TMEAN_WSS<-TMEAN_WS(inputpolys)
  TMEAN_PTT<-TMEAN_PT(inputpoints)
  PMIN_PTT<-PMIN_PT(inputpolys)
  dfpolys<-cbind(inputpolys$reachid,ELVmean_WSS,ELVmin_WSS,KFACTS,PMIN_WSS,RH_WSS,TMAX_WSS,TMEAN_WSS)
  dfpoints<-cbind(inputpoints$reachid,TMEAN_PTT,PMIN_PTT)
  df2render<-merge(dfpolys, dfpoints, by="reachid")
  return(df2render)
}


ptm <- proc.time()
AREMtest<-AREM_model(polygon2process = AREMP2020.WGS.json.simp, points2process = AREMP2020.WGS.json.points)
proc.time() - ptm

pechereco<-ELVmean_WS(putin)

geojson_write()